
# Economic Indicators with Inflation and Crises Analysis

This project is a Shiny web application that visualizes the relationships between various economic indicators and economic crises. Users can interact with different visualizations, including PCA biplots, hierarchical clustering dendrograms, and heatmaps, to explore how key economic metrics, such as GDP, inflation, unemployment, stock market performance, and interest rates, evolve over time, particularly during crisis periods.

## Features

- **Interactive Visualizations**: The app provides three main types of interactive plots:
  1. **PCA Biplot**: Visualize the relationships between multiple economic indicators using Principal Component Analysis (PCA).
  2. **Hierarchical Clustering Dendrogram**: Group years based on the similarity of economic indicators to reveal patterns during crises.
  3. **Heatmap**: Display the year-to-year changes in economic indicators across different periods, highlighting significant shifts during crises.
  
- **Data Filtering**: Users can select a specific range of years using a slider and view the changes in the selected period.
  
- **Economic Indicators**: The app tracks and analyzes the following key economic indicators:
  1. **GDP Growth**
  2. **Inflation Rate**
  3. **Unemployment Rate**
  4. **S&P 500 Stock Market Performance**
  5. **Interest Rates**

## Requirements

To run the app locally, the following libraries and dependencies must be installed:

- `shiny`
- `dplyr`
- `ggplot2`
- `FactoMineR`
- `factoextra`
- `cluster`
- `tidyr`
- `plotly`
- `ggdendro`
- `scales`
- `xts`

You can install the necessary R packages by running:

```R
install.packages(c("shiny", "dplyr", "ggplot2", "FactoMineR", "factoextra", "cluster", "tidyr", "plotly", "ggdendro", "scales", "xts"))
```

## Data Source

The economic data is sourced from the **Federal Reserve Economic Data (FRED)** system using the `load_or_update_fred_data()` function. The app expects the data files to be stored locally in the `data/` folder and loaded using the FRED series IDs.

The following FRED data series are used:
- **GDP**: Gross Domestic Product (`GDP`)
- **Inflation**: Consumer Price Index for All Urban Consumers (`CPIAUCSL`)
- **Interest Rates**: Federal Funds Rate (`FEDFUNDS`)
- **Unemployment**: Unemployment Rate (`UNRATE`)
- **S&P 500**: Stock market performance (`GSPC.Close`)

Ensure that the `fred_utils.R` script is available to handle data loading from FRED.

## Usage

### Running the App Locally

To run the Shiny app locally, follow these steps:

1. Clone or download this repository to your local machine.
2. Ensure that all necessary libraries are installed.
3. Place the economic data in the `data/` directory as expected by the app.
4. Source the `fred_utils.R` file to allow for data loading and updating.
5. Run the app by executing the following in your R console:

```R
shiny::runApp('path/to/your/app/folder')
```

### User Interaction

- **Year Range Selection**: Use the slider to select a range of years between 1980 and 2024.
- **Plot Selection**: Choose between the PCA Biplot, Hierarchical Clustering Dendrogram, or Heatmap to explore different perspectives on the data.

## Project Structure

```
.
├── data/                   # Folder containing the local data files (e.g., gdp.RData, inflation.RData)
├── fred_utils.R            # Helper script for loading or updating data from FRED
├── app.R                   # Main Shiny application script
├── README.md               # This README file
```
You have to create an APIKey with FRED and update the key file.

## License

© Lochana Arumugam, 2024. All rights reserved. This project is intended for personal or academic use only. Commercial use or redistribution is prohibited without explicit permission.

## Author

- **Lochana Arumugam**  
Feel free to reach out for questions, suggestions, or collaboration opportunities.
