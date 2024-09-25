invalid_file_type <- function(res) {
  res$status <- 400L
  msg <- "Invalid file type; please upload file of type text/csv."
  bad_request_response(msg)
}

duplicate_dataset_name <- function(res, name) {
  res$status <- 400L
  msg <- paste(name, "already exists.",
               "Please choose a unique name for this dataset.")
  bad_request_response(msg)
}

missing_columns <- function(res, missing_cols) {
  res$status <- 400L
  msg <- paste("Missing required columns:",
               paste(missing_cols, collapse = ", "))
  bad_request_response(msg)
}

invalid_xcol <- function(res) {
  res$status <- 400L
  msg <- paste("Invalid x column values:",
               "these should be numbers or dates in a standard format.")
  bad_request_response(msg)
}
