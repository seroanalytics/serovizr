main_args <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
  seroviz [options]

Options:
  --port=PORT       Port to run on [default: 8888]"
  dat <- docopt::docopt(usage, args)
  list(port = as.integer(dat$port))
}


main <- function(args = commandArgs(TRUE)) {
  options(error = rlang::entrace)
  opts <- main_args(args)
  port <- opts$port
  logger::log_info("Starting API")

  build_routes()$run("0.0.0.0", port) # nocov
}
