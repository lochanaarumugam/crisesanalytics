# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(cluster)
library(tidyr)
library(dplyr)
library(plotly)
library(ggdendro)
library(scales)
library(xts)
source("fred_utils.R")


# Define UI for the Shiny App
ui <- fluidPage(
  titlePanel("Economic Indicators with Inflation and Crises Analysis"),
  
  fluidRow(
    column(6,
           sliderInput("yearRange", 
                       "Select Year Range:", 
                       min = 1980, 
                       max = 2024, 
                       value = c(1980, 2024), 
                       step = 1, 
                       sep = "")
    ),
    column(6,
           selectInput("plotType", 
                       "Select Plot Type:", 
                       choices = c("PCA Biplot", "Dendrogram", "Heatmap"), 
                       selected = "PCA Biplot")
    )
  ),
  
  # Main panel for displaying the plots
  mainPanel(
    plotlyOutput("plot", width = "100%", height = "600px"),
    htmlOutput("plotDescription"),
    # Copyright Information
    h6("© Lochana Arumugam, 2024. All rights reserved."),
    h6("This application is for personal or academic use only. Commercial use or redistribution is prohibited without explicit permission.")
    
  )
)

# Define Server logic
server <- function(input, output, session) {

  crisis_years <- c(1987, 1997, 1998, 2000, 2001, 2002, 2007, 2008, 2011, 2012, 2020, 2024)
  
  
  gdp_data_file_path <- "data/gdp.RData"
  gdp_series_id <- "GDP"
  
  gdp_data_raw <- load_or_update_fred_data(gdp_series_id, gdp_data_file_path)
  gdp_data_clean <- na.omit(gdp_data_raw)
  annual_gdp_data <- apply.yearly(gdp_data_clean, sum)
  # Convert xts object to a data frame for easier merging and manipulation
  annual_gdp_df <- data.frame(
    year = as.integer(format(index(annual_gdp_data), "%Y")),
    annual_gdp = coredata(annual_gdp_data)
  )
  
  # Filter the data frame for years from 1980 to 2024
  annual_gdp_df <- annual_gdp_df %>%
    filter(year >= "1980", year <= "2024")
  
  # Ensure the data is sorted by Year
  annual_gdp_df <- annual_gdp_df %>%
    arrange(year)
  
  annual_gdp_df <- annual_gdp_df %>%
    mutate(gdp_change = (annual_gdp / lag(annual_gdp) - 1) * 100)
  
  # Inflation
  
  inflation_data_file_path <- "data/inflation.RData"
  inflation_series_id <- "CPIAUCSL"  # Example: CPI for All Urban Consumers, change to actual inflation series if different
  
  # Load or update inflation data
  inflation_raw <- load_or_update_fred_data(inflation_series_id, inflation_data_file_path)
  colnames(inflation_raw) <- "inflation_rate"
  
  # Aggregate to annual inflation data (usually calculated as the yearly change in CPI)
  annual_inflation <- apply.yearly(inflation_raw, mean)
  
  # Convert to a data frame
  annual_inflation_df <- data.frame(
    year = as.integer(format(index(annual_inflation), "%Y")),
    inflation_rate = coredata(annual_inflation)
  )
  
  # Filter inflation data for relevant years
  annual_inflation_df <- annual_inflation_df %>%
    filter(year >= 1980, year <= 2024) %>%
    arrange(year)
  
  # Calculate the year-to-year percentage change in inflation
  annual_inflation_df <- annual_inflation_df %>%
    mutate(inflation_change = round((inflation_rate / lag(inflation_rate) - 1) * 100, 2))
  
  # Intereste rate
  
  interest_rate_data_file_path <- "data/interest_rate.RData"
  interest_rate_series_id <- "FEDFUNDS"
  
  # Load interest rate data
  interest_rate_raw <- load_or_update_fred_data(interest_rate_series_id, interest_rate_data_file_path)
  colnames(interest_rate_raw) <- "interest_rate"
  
  # Aggregate to annual inflation data (usually calculated as the yearly change in CPI)
  interest_rate  <- apply.yearly(interest_rate_raw, mean)
  
  # Convert to a data frame
  interest_rate_df <- data.frame(
    year = as.integer(format(index(interest_rate), "%Y")),
    interest_rate = coredata(interest_rate)
  )
  
  # Filter inflation data for relevant years
  interest_rate_df <- interest_rate_df %>%
    filter(year >= 1980, year <= 2024) %>%
    arrange(year)
  
  # Calculate the year-to-year percentage change in inflation
  interest_rate_df <- interest_rate_df %>%
    mutate(interest_change = round((interest_rate / lag(interest_rate) - 1) * 100, 2))

  # Loading unemployment data
  
  unemployment_data_file_path <- "data/unemployment.RData"
  unemployment_series_id <- "UNRATE"
  
  unemployment_raw <- load_or_update_fred_data(unemployment_series_id, unemployment_data_file_path)
  colnames(unemployment_raw) <- "unemployment_rate"
  # Aggregate monthly unemployment rates to annual averages
  annual_unemployment <- apply.yearly(unemployment_raw, mean)
  
  # Convert to a data frame for easier integration with other datasets
  annual_unemployment_df <- data.frame(
    year = as.integer(format(index(annual_unemployment), "%Y")),
    unemployment_rate = coredata(annual_unemployment)
  )
  # Filter the unemployment data for these years
  filtered_unemployment_data <- annual_unemployment_df %>%
    filter(year %in% crisis_years)
  
  # Calculate the year-to-year change in unemployment rate
  filtered_unemployment_data <- filtered_unemployment_data %>%
    mutate(unemployment_change = c(NA, diff(unemployment_rate)))
  
  # Replace NA in Unemployment_Change with 0
  filtered_unemployment_data <- filtered_unemployment_data %>%
    mutate(unemployment_change = replace_na(unemployment_change, 0))
  
  # Load S&P 500 stock market data
  SP500 <- read.csv("data/sp500_data.csv")
  SP500$Date <- as.Date(SP500$X)
  
  # Convert the date column to Date type in stock data
  SP500$year <- as.integer(format(SP500$Date, "%Y"))
  
  # Remove rows with NA values in the 'GSPC.Close' column (assuming this is the relevant column)
  SP500_clean <- SP500 %>%
    filter(!is.na(GSPC.Close))
  
  # Calculate the yearly percentage change in the S&P 500 index
  SP500_clean <- SP500_clean %>%
    group_by(year) %>%
    summarize(
      first_value = first(GSPC.Close),
      last_value = last(GSPC.Close),
      stock_market_change = round((last_value - first_value) / first_value * 100, 2)
    )
  
  
  # Merge GDP and unemployment data by country and year
  #crisis_data <- merge(annual_gdp_df, filtered_unemployment_data, by = "year")
  
  #crisis_data <- merge(crisis_data, annual_inflation_df, by = "year")
  
  # Merge U.S. S&P 500 data into crisis_data, propagating the stock market data to all countries for the same years
  #crisis_data <- crisis_data %>%
  #  left_join(SP500_clean %>% select(year, stock_market_change), by = "year")
  
  
  
  # Remove unnecessary columns and rows with NA values
  #crisis_data <- na.omit(crisis_data)
  
  
  # Reactive data filtered by year range
  filtered_data <- reactive({
    annual_gdp_df_filtered <- annual_gdp_df %>%
      filter(year >= input$yearRange[1], year <= input$yearRange[2])
    
    crisis_data <- merge(annual_gdp_df_filtered, annual_inflation_df, by = "year")
    crisis_data <- merge(crisis_data, interest_rate_df, by = "year")
    
    crisis_data_filtered <- merge(crisis_data, filtered_unemployment_data, by = "year") %>%
      left_join(SP500_clean %>% select(year, stock_market_change), by = "year") %>%
      na.omit()
    
    return(crisis_data_filtered)
  })
  # Reactive PCA result
  pca_result <- reactive({
    data <- filtered_data()
    PCA(data[, c("gdp_change", "unemployment_change", "stock_market_change", "inflation_change", "interest_change")], 
        scale.unit = TRUE, ncp = 2, graph = FALSE)
  })
  
  # Reactive distance matrix and clustering
  dist_matrix_reactive <- reactive({
    data <- filtered_data()
    dist(scale(data[, c("gdp_change", "unemployment_change", "stock_market_change", "inflation_change", "interest_change")]))
  })
  
  hc_reactive <- reactive({
    hclust(dist_matrix_reactive(), method = "ward.D2")
  })
  
  # Render interactive plot based on selected plot type
  output$plot <- renderPlotly({
    if (input$plotType == "PCA Biplot") {
      # PCA Biplot
      pca_plot <- fviz_pca_biplot(pca_result(), repel = TRUE, col.var = "contrib", col.ind = "cos2")
      ggplotly(pca_plot)  # Convert ggplot to plotly object for interactivity
      
    } else if (input$plotType == "Dendrogram") {
      # Use ggdendro to create a ggplot object from the hierarchical clustering dendrogram
      hc_data <- as.dendrogram(hc_reactive())
      dendro_data <- dendro_data(hc_data, type = "rectangle")
      
      # Create ggplot object for dendrogram
      dend_plot <- ggplot() + 
        geom_segment(data = segment(dendro_data), aes(x = x, y = y, xend = xend, yend = yend)) +
        geom_text(data = label(dendro_data), aes(x = x, y = y, label = label), hjust = 1, vjust = 0.5) +
        labs(title = "Hierarchical Clustering Dendrogram") +
        theme_minimal()
      
      ggplotly(dend_plot)  # Convert ggplot to plotly object for interactivity
      
    } else if (input$plotType == "Heatmap") {
      # Heatmap
      data <- filtered_data()
      crisis_long <- pivot_longer(data, 
                                  cols = c("gdp_change", "unemployment_change", "stock_market_change", "inflation_change", "interest_change"), 
                                  names_to = "indicator", values_to = "value")
      
      heatmap_plot <- ggplot(crisis_long, aes(x = year, y = indicator, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                             labels = scales::number_format(accuracy = 0.01)) +  # Correctly format labels with 2 decimal places
        theme_minimal() +
        labs(title = "Enhanced Heatmap of Economic Indicators for Various Crises")
      
      ggplotly(heatmap_plot)  # Convert ggplot to plotly object for interactivity
    }
  })
  
  
  # Render explanation text based on selected plot type
  output$plotDescription <- renderUI({
    if (input$plotType == "PCA Biplot") {
      HTML("<p><strong>PCA Biplot:</strong> This plot shows the relationship between multiple economic indicators (GDP change, unemployment change, stock market change) and their contribution to economic crises. It helps identify patterns and correlations between these indicators.</p>")
      
    } else if (input$plotType == "Dendrogram") {
      HTML("<p><strong>Dendrogram (Hierarchical Clustering):</strong> This plot groups years based on the similarity of economic indicators (GDP, unemployment, stock market change). It shows how different crises are related to each other based on these factors.</p>")
      
    } else if (input$plotType == "Heatmap") {
      HTML("<p><strong>Heatmap:</strong> The heatmap visualizes changes in economic indicators across different years. The color intensity shows the magnitude of the change: red indicates an increase, blue indicates a decrease, and white represents little or no change.</p>")
    }
  })
}

# Run the Shiny App
shinyApp(ui, server)
