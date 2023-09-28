
#' @export
nearest_stations <- function(input_df, timestamp_column=NULL, latitude_column=NULL, longitude_column=NULL) {
  params <- list(
    timestamp_column = timestamp_column,
    latitude_column = latitude_column,
    longitude_column = longitude_column
  )
  url <- construct_url('/nearest-station-csv/', params)
  return(post_csv(url, input_df))
}

#' @export
nearest_station <- function(latitude, longitude, timestamp=NULL) {
  if(is.null(timestamp)) {
    timestamp <- Sys.time()
  }
  params <- list(
    latitude = paste(latitude),
    longitude = paste(longitude),
    timestamp = format(timestamp, "%FT%T")
  )
  url <- construct_url('/nearest-station', params);
  cat(url)
}
