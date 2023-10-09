
#' @export
add_nearest_stations <- function(input_df, timestamp_column=NULL, latitude_column=NULL, longitude_column=NULL) {
  params <- list(
    timestamp_column = timestamp_column,
    latitude_column = latitude_column,
    longitude_column = longitude_column
  )
  url <- construct_url('/add-nearest-stations/', params)
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
  url <- construct_url('/nearest-station', params)
  s <- get_json(url)
  return(list(
    name = s$station$name,
    latitude = s$geolocation$latitude,
    longitude = s$geolocation$longitude,
    distance_km = s$distance,
    depth = s$depth,
    comment = s$comment
  ))
}

#' @export
station_list <- function(timestamp=NULL) {
  if(is.null(timestamp)) {
    timestamp <- Sys.time()
  }
  params <- list(
    timestamp = format(timestamp, "%FT%T")
  )
  url <- construct_url('/station-list', params)
  return(get_csv(url))
}
