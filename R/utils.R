with_warnings <- function(expr, stop_on_error = TRUE) {
  my_warnings <- NULL

  w_handler <- function(w) {
    my_warnings <<- unique(c(my_warnings, jsonlite::unbox(conditionMessage(w))))
    invokeRestart("muffleWarning")
  }

  e_handler_warn <- function(e) {
    my_warnings <<- unique(c(my_warnings, jsonlite::unbox(conditionMessage(e))))
    invokeRestart("muffleStop")
  }

  e_handler_stop <- function(e) {
    porcelain::porcelain_stop(jsonlite::unbox(conditionMessage(e)))
  }

  if (stop_on_error) {
    e_handler <- e_handler_stop
  } else {
    e_handler <- e_handler_warn
  }
  val <- withCallingHandlers(withRestarts(expr, muffleStop = function() NULL),
                             warning = w_handler, error = e_handler)
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

parse_date <- function(dat) {
  as.Date(lubridate::parse_date_time(dat,
                                     c("dmy", "mdy", "ymd", "ydm")))
}
