build_routes <- function(cookie_key = plumber::random_cookie_key(),
                         cache = cachem::cache_mem(max_age = 60)) {
  if (!dir.exists("uploads")) {
    dir.create("uploads")
  }
  plumber::options_plumber(trailingSlash = TRUE)
  pr <- porcelain::porcelain$new(validate = TRUE)
  pr$registerHook(stage = "preserialize", function(data, req, res, value) {
    if (!is.null(req$HTTP_ORIGIN) &&
      req$HTTP_ORIGIN %in% c("http://localhost:3000", "http://localhost")) {
      # allow local app and integration tests to access endpoints
      res$setHeader("Access-Control-Allow-Origin", req$HTTP_ORIGIN)
      res$setHeader("Access-Control-Allow-Credentials", "true")
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
  })

  pr$registerHooks(plumber::session_cookie(cookie_key,
                                           name = "serovizr",
                                           path = "/",
                                           expiration = 60))

  pr$handle(get_root())
  pr$handle(get_version())
  pr$handle("POST", "/dataset/",
            function(req, res) target_post_dataset(req, res),
            serializer = plumber::serializer_unboxed_json(null = "null"))
  pr$handle(get_dataset())
  pr$handle(get_datasets())
  pr$handle(get_trace())
}

get_root <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/",
    target_get_root,
    returning = porcelain::porcelain_returning_json())
}

get_version <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/version/",
    target_get_version,
    returning = porcelain::porcelain_returning_json("Version"))
}

get_dataset <- function() {
  porcelain::porcelain_endpoint$new(
    "GET", "/dataset/<name>/",
    target_get_dataset,
    returning = porcelain::porcelain_returning_json("DatasetMetadata"))
}

get_datasets <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/datasets/",
    target_get_datasets,
    returning = porcelain::porcelain_returning_json("DatasetNames"))
}

get_trace <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/dataset/<name>/trace/<biomarker>/",
    target_get_trace,
    porcelain::porcelain_input_query(disaggregate = "string",
                                     filter = "string",
                                     scale = "string"),
    returning = porcelain::porcelain_returning_json("DataSeries"))
}

prune_inactive_sessions <- function(cache) {
  active_sessions <- cache$keys()
  subdirectories <- list.files("uploads")
  old_sessions <- setdiff(subdirectories, active_sessions)
  if (length(old_sessions) > 0) {
    logger::log_info("Cleaning up expired sessions")
    lapply(old_sessions, function(x) fs::dir_delete(file.path("uploads", x)))
  }
}
