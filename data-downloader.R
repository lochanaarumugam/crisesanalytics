library(WDI)
library(quantmod)

# Fetch GDP growth data for different crises (2008, 2020, etc.)
#gdp_data <- WDI(indicator = "NY.GDP.MKTP.KD.ZG", start = 1980, end = 2024, country = "all")
#write.csv(gdp_data, file = "data/gdp_data.csv", row.names = FALSE)

# Fetch unemployment data
#unemployment_data <- WDI(indicator = "SL.UEM.TOTL.ZS", start = 1980, end = 2024, country = "all")
#write.csv(unemployment_data, file = "data/unemployment_data.csv", row.names = FALSE)
getSymbols("^GSPC", src = "yahoo", from = "1980-01-01")
write.csv(as.data.frame(GSPC), file = "data/sp500_data.csv")
