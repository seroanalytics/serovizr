target_get_root <- function() {
  jsonlite::unbox("Welcome to serovizr")
}

target_get_version <- function() {
  jsonlite::toJSON(as.character(utils::packageVersion("serovizr")), auto_unbox= TRUE)
}


target_post_dataset <- function() {
  function(data) {
    # validate CSV and save to file
  }
}
