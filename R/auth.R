library(httr)
library(readr)
library(askpass)

#' @export
obtain_auth_token <- function(base_url) {
  username <- readline(prompt = "Username: ")
  password <- askpass("Password: ")
  data <- list(username = username, password = password)
  response <- POST(paste0(base_url, "/api-token-auth/"), body = data, encode = "form")
  content <- content(response, "parsed")
  token <- content$token
  cat("Add the following lines to your .Renviron file:\n")
  cat(paste0('NESLTER_API_URL="', base_url, '"\n'))
  cat(paste0('NESLTER_API_TOKEN="', token, '"\n'))
}

# make an authorized POST request with a CSV body and response
post_csv <- function(url, input_df) {
  file_path <- tempfile(fileext = ".csv")
  write_csv(input_df, file_path)

  auth_token <- Sys.getenv("NESLTER_API_TOKEN")

  response <- POST(
    url,
    add_headers(Authorization = paste("Token", auth_token)),
    body = list(
      csv_file = upload_file(file_path)
    ),
    encode = "multipart"
  )

  if(response$status_code == 200) {
    output_file_path <- tempfile(fileext = ".csv")
    writeBin(response$content, output_file_path)
    output_df <- read_csv(output_file_path)
    return(output_df)
  } else {
    error_message <- paste("Error:", response$status_code, content(response, "text"))
    stop(error_message)
  }
}
