build_routes <- function(cookie_key = plumber::random_cookie_key(),
                         cache = cachem::cache_mem(max_age = 3600 * 60)) {
  if (!dir.exists("uploads")) {
    dir.create("uploads")
  }
  plumber::options_plumber(trailingSlash = TRUE)
  pr <- porcelain::porcelain$new(validate = TRUE)
  pr$registerHook(stage = "preserialize", preserialize_hook(cache))
  pr$registerHooks(plumber::session_cookie(cookie_key,
                                           name = "serovizr",
                                           path = "/"))

  pr$filter("logger", logging_filter)

  pr$handle(get_root())
  pr$handle(get_version())
  # porcelain doesn't support multipart form content yet; for now wire this
  # endpoint up using plumber arguments instead
  pr$handle("POST", "/api/dataset/", target_post_dataset,
            serializer = plumber::serializer_unboxed_json(null = "null"))
  pr$handle(options_dataset())
  pr$handle(delete_dataset())
  pr$handle(get_dataset())
  pr$handle(get_datasets())
  pr$handle(get_trace())
  pr$handle(get_individual())
  pr$handle(options_session())
  pr$handle(delete_session())
  setup_docs(pr)
}

logging_filter <- function(req, res) {
  logger::log_info(paste(as.character(Sys.time()), "-",
                         req$REQUEST_METHOD, req$PATH_INFO, "-",
                         req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n"))
  plumber::forward()
}

preserialize_hook <- function(cache) {
  function(data, req, res, value) {
    if (!is.null(req$HTTP_ORIGIN) &&
      req$HTTP_ORIGIN %in% c("http://localhost:3000", "http://localhost")) {
      # allow local app and integration tests to access endpoints
      res$setHeader("Access-Control-Allow-Origin", req$HTTP_ORIGIN)
      res$setHeader("Access-Control-Allow-Credentials", "true")
      res$setHeader("Access-Control-Allow-Methods",
                    c("GET, POST, OPTIONS, PUT, DELETE"))
    }

    tryCatch({
      if (!is.null(req$session$id)) {
        logger::log_info("Updating session cache")
        id <- as.character(req$session$id)
        cache$set(id, TRUE)
      }
      logger::log_info("Looking for inactive sessions")
      prune_inactive_sessions(cache)
    }, error = function(e) logger::log_error(conditionMessage(e)))

    value
  }
}

setup_docs <- function(pr) {
  api <- yaml::read_yaml(file.path(system.file("spec.yaml",
                                               package = "serovizr")),
                         eval.expr = FALSE)
  pr$setApiSpec(api)
  # this is a bit annoying, but setDocs fails if the package isn't
  # already loaded
  requireNamespace("redoc")
  pr$setDocs("redoc")
  pr$mount("/schema", plumber::PlumberStatic$new(
    file.path(system.file("schema", package = "serovizr"))))
  pr
}

prune_inactive_sessions <- function(cache) {
  active_sessions <- cache$keys()
  subdirectories <- list.files("uploads")
  old_sessions <- setdiff(subdirectories, active_sessions)
  if (length(old_sessions) > 0) {
    logger::log_info("Cleaning up expired sessions")
    lapply(old_sessions, function(x) unlink(file.path("uploads", x),
                                            recursive = TRUE))
  }
}
