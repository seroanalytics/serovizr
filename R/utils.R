with_warnings <- function(expr) {
  my_warnings <- NULL

  w_handler <- function(w) {
    my_warnings <<- c(my_warnings, list(w))
    invokeRestart("muffleWarning")
  }

  val <- withCallingHandlers(expr, warning = w_handler)
  list(output = val,
       warnings = lapply(my_warnings,
                         function(w) jsonlite::unbox(conditionMessage(w))))
}
