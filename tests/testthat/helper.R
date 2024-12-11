make_req <- function(verb = "GET",
                     path = "/",
                     qs = "",
                     body = "",
                     pr = NULL,
                     ...) {
  req <- as.environment(list(...))
  req$REQUEST_METHOD <- toupper(verb)
  req$PATH_INFO <- path
  req$QUERY_STRING <- qs
  if (is.character(body)) {
    body <- charToRaw(body)
  }
  stopifnot(is.raw(body))
  req$rook.input <- Rook::RhttpdInputStream$new(body)
  req$pr <- pr
  req
}

local_add_dataset <- function(dat, name, session = session_id, env = parent.frame()) {
  filepath <- file.path("uploads", session, name)
  dir.create(filepath, recursive = TRUE)
  write.csv(dat, file.path(filepath, "data"), row.names = FALSE)
  write("day", file.path(filepath, "xcol"))
  write("number", file.path(filepath, "xtype"))
  write("surveillance", file.path(filepath, "series_type"))
  withr::defer({
    if (dir.exists(filepath)) {
      fs::dir_delete(filepath)
    }
  }, envir = env)
  name
}

local_POST_dataset_request <- function(dat, filename,
                                       xcol = "day",
                                       series_type = "surveillance",
                                       env = parent.frame(),
                                       session = session_id,
                                       cookie = "") {
  EOL <- "\r\n"
  boundary <- "------WebKitFormBoundaryvbfCGA1r00d8B0Vv"
  request_body <- paste0(boundary, EOL,
                         sprintf("Content-Disposition: form-data; name=\"file\"; filename=\"%s\"", filename),
                         EOL,
                         "Content-Type: text/csv", EOL, EOL,
                         readr::format_csv(dat, eol = EOL), EOL,
                         boundary, EOL,
                         "Content-Disposition: form-data; name=\"series_type\"", EOL, EOL,
                         series_type, EOL,
                         boundary, EOL,
                         "Content-Disposition: form-data; name=\"xcol\"", EOL, EOL,
                         xcol, EOL,
                         boundary, "--")
  filepath <- file.path("uploads", session, filename)
  withr::defer({
    if (fs::file_exists(filepath)) {
      fs::file_delete(filepath)
    }
  }, envir = env)

  make_req("POST", "/dataset/",
           body = request_body,
           HTTP_COOKIE = cookie,
           CONTENT_LENGTH = nchar(request_body),
           CONTENT_TYPE = "multipart/form-data; boundary=----WebKitFormBoundaryvbfCGA1r00d8B0Vv")
}

local_POST_dataset_request_no_xcol <- function(dat, filename,
                                               env = parent.frame()) {
  EOL <- "\r\n"
  boundary <- "------WebKitFormBoundaryvbfCGA1r00d8B0Vv"
  request_body <- paste0(boundary, EOL,
                         sprintf("Content-Disposition: form-data; name=\"file\"; filename=\"%s\"", filename),
                         EOL,
                         "Content-Type: text/csv", EOL, EOL,
                         readr::format_csv(dat, eol = EOL), EOL,
                         boundary, EOL,
                         "Content-Disposition: form-data; name=\"series_type\"", EOL, EOL,
                         "surveillance", EOL,
                         boundary, "--")
  filepath <- file.path("uploads", session_id, filename)
  withr::defer({
    if (fs::file_exists(filepath)) {
      fs::file_delete(filepath)
    }
  }, envir = env)

  make_req("POST", "/dataset/",
           body = request_body,
           CONTENT_LENGTH = nchar(request_body),
           CONTENT_TYPE = "multipart/form-data; boundary=----WebKitFormBoundaryvbfCGA1r00d8B0Vv")
}

local_POST_dataset_request_with_name <- function(dat,
                                                 filename,
                                                 name,
                                                 xcol = "day",
                                                 series_type = "surveillance",
                                                 env = parent.frame(),
                                                 cookie = cookie) {
  EOL <- "\r\n"
  boundary <- "------WebKitFormBoundaryvbfCGA1r00d8B0Vv"
  request_body <- paste0(boundary, EOL,
                         sprintf("Content-Disposition: form-data; name=\"file\"; filename=\"%s\"", filename),
                         EOL,
                         "Content-Type: text/csv", EOL, EOL,
                         readr::format_csv(dat, eol = EOL), EOL,
                         boundary, EOL,
                         "Content-Disposition: form-data; name=\"xcol\"", EOL, EOL,
                         xcol, EOL,
                         boundary, EOL,
                         "Content-Disposition: form-data; name=\"series_type\"", EOL, EOL,
                         series_type, EOL,
                         boundary, EOL,
                         "Content-Disposition: form-data; name=\"name\"", EOL, EOL,
                         name, EOL,
                         boundary, "--")
  filepath <- file.path("uploads", session_id, filename)
  withr::defer({
    if (fs::file_exists(filepath)) {
      fs::file_delete(filepath)
    }
  }, envir = env)

  make_req("POST", "/dataset/",
           body = request_body,
           CONTENT_LENGTH = nchar(request_body),
           HTTP_COOKIE = cookie,
           CONTENT_TYPE = "multipart/form-data; boundary=----WebKitFormBoundaryvbfCGA1r00d8B0Vv")
}

local_POST_dataset_request_bad_file <- function(env = parent.frame()) {
  filename <- "baddata"
  EOL <- "\r\n"
  boundary <- "------WebKitFormBoundaryvbfCGA1r00d8B0Vv"
  request_body <- paste0(boundary, EOL,
                         sprintf("Content-Disposition: form-data; name=\"file\"; filename=\"%s\"", filename),
                         EOL,
                         "Content-Type: image/png", EOL, EOL,
                         "1234", EOL,
                         boundary, EOL,
                         "Content-Disposition: form-data; name=\"series_type\"", EOL, EOL,
                         "surveillance", EOL,
                         boundary, EOL,
                         "Content-Disposition: form-data; name=\"xcol\"", EOL, EOL,
                         "day", EOL,
                         boundary, "--")
  filepath <- file.path("uploads", session_id, filename)
  withr::defer({
    if (fs::file_exists(filepath)) {
      fs::file_delete(filepath)
    }
  }, envir = env)

  make_req("POST", "/dataset/",
           body = request_body,
           CONTENT_LENGTH = nchar(request_body),
           CONTENT_TYPE = "multipart/form-data; boundary=----WebKitFormBoundaryvbfCGA1r00d8B0Vv")
}

validate_failure_schema <- function(res) {
  porcelain:::porcelain_validator("ResponseFailure",
                                  root = schema_root,
                                  query = NULL)(res)
}
