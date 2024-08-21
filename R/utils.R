with_warnings <- function(expr) {
  my_warnings <- NULL

  w_handler <- function(w) {
    my_warnings <<- c(my_warnings, jsonlite::unbox(conditionMessage(w)))
    invokeRestart("muffleWarning")
  }

  val <- withCallingHandlers(expr, warning = w_handler)
  list(output = val,
       warnings = my_warnings)
}
