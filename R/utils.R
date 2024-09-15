with_warnings <- function(expr) {
  my_warnings <- NULL

  w_handler <- function(w) {
    my_warnings <<- c(my_warnings, jsonlite::unbox(conditionMessage(w)))
    invokeRestart("muffleWarning")
  }

  e_handler <- function(e) {
    porcelain::porcelain_stop(jsonlite::unbox(conditionMessage(e)))
  }

  val <- withCallingHandlers(expr, warning = w_handler, error = e_handler)
  list(output = val,
       warnings = my_warnings)
}

validate_scale <- function(scale) {
  if (!(scale %in% c("log", "log2", "natural"))) {
    porcelain::porcelain_stop(
      "'scale' must be one of 'log', 'log2', or 'natural'"
    )
  }
}
