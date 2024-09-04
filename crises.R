library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(cluster)
library(tidyr)

source("fred_utils.R")
crisis_years <- c(1987, 1997, 1998, 2000, 2001, 2002, 2007, 2008, 2011, 2012, 2020)

# GDP

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
  mutate(inflation_change = (inflation_rate / lag(inflation_rate) - 1) * 100)


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
  mutate(interest_change = (interest_rate / lag(interest_rate) - 1) * 100)



# unemployment

unemployment_data_file_path <- "data/unemployment.RData"
unemployment_series_id <- "UNRATE"

unemployment_raw <- load_or_update_fred_data(unemployment_series_id, unemployment_data_file_path)
colnames(unemployment_raw) <- "unemployment_rate"
# Aggregate monthly unemployment rates to annual averages
annual_unemployment <- apply.yearly(unemployment_raw, mean)

# Convert to a data frame for easier integration with other datasets
annual_unemployment_df <- data.frame(
  year = as.integer(format(index(annual_unemployment), "%Y")),
  unemployment_Rate = coredata(annual_unemployment)
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

# S&P 500
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
    stock_market_change = (last_value - first_value) / first_value * 100
  )

# Inflation



# Merge GDP and unemployment data by country and year
crisis_data <- merge(annual_gdp_df, filtered_unemployment_data, by = "year")

crisis_data <- merge(crisis_data, annual_inflation_df, by = "year")

crisis_data <- merge(crisis_data, interest_rate_df, by = "year")

# Merge U.S. S&P 500 data into crisis_data, propagating the stock market data to all countries for the same years
crisis_data <- crisis_data %>%
  left_join(SP500_clean %>% select(year, stock_market_change), by = "year")

# Remove unnecessary columns and rows with NA values
crisis_data <- na.omit(crisis_data)

# Perform PCA on the merged crisis_data
pca <- PCA(crisis_data[, c("gdp_change", "unemployment_change", "stock_market_change", "inflation_change", "interest_change")], 
           scale.unit = TRUE, ncp = 2, graph = TRUE)

# Visualize PCA variable contributions
fviz_pca_var(pca, col.var = "cos2")

# Compute distance matrix for clustering
dist_matrix <- dist(scale(crisis_data[, c("gdp_change", "unemployment_change", "stock_market_change", "interest_change")]))
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot hierarchical clustering dendrogram
plot(hc, labels = crisis_data$country)

# PCA Biplot
fviz_pca_biplot(pca, repel = TRUE, col.var = "contrib", col.ind = "cos2")

# Reshape data for heatmap (for crises comparison)
crisis_long <- pivot_longer(crisis_data, cols = c("gdp_change", "unemployment_change", "stock_market_change", "interest_change"), 
                            names_to = "indicator", values_to = "value")


ggplot(crisis_long, aes(x = year, y = indicator, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Enhanced Heatmap of Economic Indicators for Various Crises")
