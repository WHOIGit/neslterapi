#' @importFrom jsonlite fromJSON

construct_url_params <- function(params=NULL) {
  param_strs <- sapply(names(params), function(key) {
    if (!is.null(params[[key]])) {
      paste0(key, '=', URLencode(params[[key]]))
    } else {
      NULL
    }
  })
  param_strs <- param_strs[!sapply(param_strs, is.null)]
  param_string <- paste(param_strs, collapse = '&')
  if(param_string != "") {
    return(paste0("?", param_string))
  } else {
    return("")
  }
}

construct_url <- function(suffix, params=NULL) {
  base_url <- Sys.getenv("NESLTER_API_URL")
  return(paste0(base_url, suffix, construct_url_params(params)))
}

get_json <- function(url) {
  response <- GET(url)
  if(response$status_code == 200) {
    s <- fromJSON(content(response, "text", encoding="UTF-8"))
    return(s)
  } else {
    error_message <- paste("Error:", response$status_code, content(response, "text"))
    stop(error_message)
  }
}

get_csv <- function(url) {
  response = GET(url)
  return(parse_csv(response))
}
