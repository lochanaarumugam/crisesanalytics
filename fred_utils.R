# Read the FRED API key from keyfile
api_key <- readLines("fred_key.txt")[1]

# Common function to fetch data from FRED
fetch_fred_data <- function(series_id) {
  message(paste("Attempting to fetch data for series:", series_id))
  fred_api_url <- paste0("https://api.stlouisfed.org/fred/series/observations?series_id=", series_id, "&api_key=", api_key, "&file_type=json")
  response <- fromJSON(fred_api_url)
  new_data <- as.xts(as.numeric(response$observations$value), order.by = as.Date(response$observations$date))
  return(new_data)
}

# Common function to load or update FRED data
load_or_update_fred_data <- function(series_id, data_file_path) {
  tryCatch({
    if (file.exists(data_file_path) && file.size(data_file_path) > 0) {
      file_mod_time <- file.info(data_file_path)$mtime
      if (Sys.Date() - file_mod_time > 1) {  
        message(paste(series_id, "data is older than a day. Fetching new data..."))
        fred_data <- fetch_fred_data(series_id)
        save(fred_data, file = data_file_path)
      } else {
        load(data_file_path)
        message(paste("Loaded", series_id, "data from", data_file_path))
      }
    } else {
      fred_data <- fetch_fred_data(series_id)
      save(fred_data, file = data_file_path)
      message(paste("Fetched and saved new", series_id, "data."))
    }
    return(fred_data)
  }, error = function(e) {
    message(paste("Error in loading or updating", series_id, "data:", e$message))
    return(NULL)
  })
}
