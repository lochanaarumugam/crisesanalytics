library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(cluster)
library(tidyr)

source("fred_utils.R")
crisis_years <- c(1987, 1997, 1998, 2000, 2001, 2002, 2007, 2008, 2011, 2012, 2020)


gdp_data_file_path <- "data/gdp.RData"
gdp_series_id <- "GDP"

gdp_data_Vector <- load_or_update_fred_data(gdp_series_id, gdp_data_file_path)
gdp_data_clean <- na.omit(gdp_data_Vector)
annual_gdp_data <- apply.yearly(gdp_data_clean, sum)
# Convert xts object to a data frame for easier merging and manipulation
annual_gdp_df <- data.frame(
  Year = format(index(annual_gdp_data), "%Y"),
  Annual_GDP = coredata(annual_gdp_data)
)

# Loading CSV data for GDP changes
gdp_data <- read.csv("data/gdp_data.csv")

# Loading unemployment data
unemployment_data <- read.csv("data/unemployment_data.csv")

# Load S&P 500 stock market data
SP500 <- read.csv("data/sp500_data.csv")

# Convert the date column to Date type in stock data
SP500$X <- as.Date(SP500$X)

# Extract the year from the date
SP500$year <- as.integer(format(SP500$X, "%Y"))

# Remove rows with NA values in the SP500 column
SP500_clean <- SP500 %>%
  filter(!is.na(SP500))

# Calculate the yearly percentage change in the S&P 500 index
SP500_clean <- SP500_clean %>%
  group_by(year) %>%
  summarize(
    first_value = first(SP500),
    last_value = last(SP500),
    stock_market_change = (last_value - first_value) / first_value * 100
  )

# Add the country column for S&P 500 data
SP500_clean$country <- "United States"

# Preview cleaned S&P 500 stock data
head(SP500_clean)

# Clean GDP data for identified crisis years
gdp_data_clean <- gdp_data %>%
  filter(year %in% crisis_years) %>%
  select(country, year, GDP_change = NY.GDP.MKTP.KD.ZG)

# Clean unemployment data for identified crisis years
unemployment_data_clean <- unemployment_data %>%
  filter(year %in% crisis_years) %>%
  select(country, year, unemployment_change = SL.UEM.TOTL.ZS)

# Merge GDP and unemployment data by country and year
crisis_data <- merge(gdp_data_clean, unemployment_data_clean, by = c("country", "year"))

# Merge U.S. S&P 500 data into crisis_data, propagating the stock market data to all countries for the same years
#crisis_data <- crisis_data %>%
#  left_join(SP500_clean %>% select(year, stock_market_change), by = "year")

# Remove unnecessary columns and rows with NA values
crisis_data <- na.omit(crisis_data)

# Perform PCA on the merged crisis_data
pca <- PCA(crisis_data[, c("GDP_change", "unemployment_change", "stock_market_change")], 
           scale.unit = TRUE, ncp = 2, graph = TRUE)

# Visualize PCA variable contributions
fviz_pca_var(pca, col.var = "cos2")

# Compute distance matrix for clustering
dist_matrix <- dist(scale(crisis_data[, c("GDP_change", "unemployment_change", "stock_market_change")]))
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot hierarchical clustering dendrogram
plot(hc, labels = crisis_data$country)

# PCA Biplot
fviz_pca_biplot(pca, repel = TRUE, col.var = "contrib", col.ind = "cos2")

# Reshape data for heatmap (for crises comparison)
crisis_long <- pivot_longer(crisis_data, cols = c("GDP_change", "unemployment_change", "stock_market_change"), 
                            names_to = "indicator", values_to = "value")


ggplot(crisis_long, aes(x = year, y = indicator, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Enhanced Heatmap of Economic Indicators for Various Crises")
