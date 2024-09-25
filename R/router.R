build_routes <- function(cookie_key = plumber::random_cookie_key(),
                         cache = cachem::cache_mem(max_age = 3600 * 60)) {
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
  })

  pr$registerHooks(plumber::session_cookie(cookie_key,
                                           name = "serovizr",
                                           path = "/"))

  pr$filter("logger", function(req, res) {
    logger::log_info(paste(as.character(Sys.time()), "-",
                           req$REQUEST_METHOD, req$PATH_INFO, "-",
                           req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n"))
    plumber::forward()
  })

  pr$handle(get_root())
  pr$handle(get_version())
  pr$handle("POST", "/api/dataset/",
            function(req, res) target_post_dataset(req, res),
            serializer = plumber::serializer_unboxed_json(null = "null"))
  pr$handle(options_dataset())
  pr$handle(delete_dataset())
  pr$handle(get_dataset())
  pr$handle(get_datasets())
  pr$handle(get_trace())
  pr$handle(get_individual())
}

get_root <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/api/",
    target_get_root,
    returning = porcelain::porcelain_returning_json())
}

get_version <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/api/version/",
    target_get_version,
    returning = porcelain::porcelain_returning_json("Version"))
}

get_dataset <- function() {
  porcelain::porcelain_endpoint$new(
    "GET", "/api/dataset/<name>/",
    target_get_dataset,
    returning = porcelain::porcelain_returning_json("DatasetMetadata"))
}

delete_dataset <- function() {
  porcelain::porcelain_endpoint$new(
    "DELETE", "/api/dataset/<name>/",
    target_delete_dataset,
    returning = porcelain::porcelain_returning_json())
}

options_dataset <- function() {
  porcelain::porcelain_endpoint$new(
    "OPTIONS", "/api/dataset/<name>/",
    function(name) "OK",
    returning = porcelain::porcelain_returning_json())
}

get_datasets <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/api/datasets/",
    target_get_datasets,
    returning = porcelain::porcelain_returning_json("DatasetNames"))
}

get_trace <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/api/dataset/<name>/trace/<biomarker>/",
    target_get_trace,
    porcelain::porcelain_input_query(disaggregate = "string",
                                     filter = "string",
                                     scale = "string",
                                     method = "string",
                                     span = "numeric",
                                     k = "numeric"),
    returning = porcelain::porcelain_returning_json("DataSeries"))
}

get_individual <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/api/dataset/<name>/individual/<pidcol>/",
    target_get_individual,
    porcelain::porcelain_input_query(scale = "string",
                                     color = "string",
                                     filter = "string",
                                     linetype = "string"),
    returning = porcelain::porcelain_returning_json("Plotly"))
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
