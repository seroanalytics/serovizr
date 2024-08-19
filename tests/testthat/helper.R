make_req <- function(verb = "GET", path = "/", qs = "", body = "", pr = NULL, ...) {
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

local_add_dataset <- function(dat, name, env = parent.frame()) {
  filepath <- file.path("uploads", name)
  write.csv(dat, file.path("uploads", name))
  withr::defer(fs::file_delete(filepath), envir = env)
  name
}

local_POST_dataset_request <- function(dat, name, env = parent.frame()) {
  EOL <- "\r\n"
  boundary <- "------WebKitFormBoundaryvbfCGA1r00d8B0Vv"
  request_body <- paste0(boundary, EOL,
                sprintf("Content-Disposition: form-data; name=\"file\"; filename=\"%s\"", name),
                EOL,
                "Content-Type: text/csv", EOL, EOL,
                readr::format_csv(dat), EOL, EOL,
                boundary, "--")
  filepath <- file.path("uploads", name)
  withr::defer({
    if (fs::file_exists(filepath)) {
      fs::file_delete(filepath)
    }
  }, envir = env)
  make_req("POST", "/dataset/",
           body = request_body,
           CONTENT_LENGTH = length(request_body),
           CONTENT_TYPE = "multipart/form-data; boundary=----WebKitFormBoundaryvbfCGA1r00d8B0Vv")
}