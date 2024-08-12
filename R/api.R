target_get_root <- function() {
  jsonlite::unbox("Welcome to serovizr")
}

target_get_version <- function() {
  jsonlite::toJSON(as.character(utils::packageVersion("serovizr")), auto_unbox = TRUE)
}

target_post_dataset <- function(req, res) {
  parsed <- Rook::Multipart$parse(req)
  file_body <- read.csv(parsed$file$tempfile)
  filename <- parsed$file$filename
  filename <- stringr::str_remove_all(filename, paste0(".", tools::file_ext(filename)))
  path <- file.path("uploads", filename)
  if (file.exists(path)) {
    res$status <- 400L
    error <- list(error = "BAD_REQUEST", detail = paste("A dataset called", filename,
                                                        "already exists. Please choose a unique name for this dataset."))
    return(list(status = "failure", errors = list(error), data = NULL))
  }
  required_cols <- c("value", "biomarker")
  missing_cols <- required_cols[!(required_cols %in% colnames(file_body))]
  if (length(missing_cols) > 0) {
    res$status <- 400L
    error <- list(error = "BAD_REQUEST",
                  detail = paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    return(list(status = "failure", errors = list(error), data = NULL))
  }

  write.csv(file_body, path, row.names = FALSE)
  return(filename)
}

target_get_dataset <- function(name) {
  dat <- read_dataset(name)
  cols <- setdiff(colnames(dat), c("value", "biomarker", "day"))
  list(variables = cols, data = jsonlite::toJSON(dat))
}

target_get_datasets <- function() {
  files <- list.files("uploads")
  jsonlite::toJSON(files)
}

target_get_trace <- function(name, biomarker, facet, trace = "age") {
  dat <- read_dataset(name)
  cols <- colnames(dat)
  # facet_def <- strsplit(facet, ":")
  # facet_var <- facet_def[[1]][1]
  # facet_level <- facet_def[[1]][2]
  # if (!(facet_var %in% cols)) {
  #   porcelain::porcelain_stop(paste("Column", facet_var, "not found in data"),
  #                             code = "BAD_REQUEST", status_code = 400L)
  # }
  # dat <- dat[dat[facet_var] == facet_level & dat["biomarker"] == biomarker,]
  dat <- dat[dat["biomarker"] == biomarker,]
  dat$value <- log(dat$value)
  groups <- split(dat, eval(parse(text = paste("~", trace))))
  model_result <- lapply(groups, model_out)
  raw <- lapply(groups, data_out)
  list(model = model_result, raw = raw)
}

read_dataset <- function(name) {
  path <- file.path("uploads", name)
  if (!file.exists(path)) {
    porcelain::porcelain_stop(paste("Did not find dataset with name ", name),
                              code = "BAD_REQUEST", status_code = 404L)
  }
  dat <- read.csv(path)
  dat$value <- as.numeric(dat$value)
  dat
}

model_out <- function(dat) {
  if (nrow(dat) > 1000) {
    m <- mgcv::gam(value ~ s(day, bs = "cs"), data = dat, method = "REML")
  } else {
    m <- stats::loess(value ~ day, data = dat, span = 0.75)
  }
  range <- range(dat$day, na.rm = TRUE)
  xseq <- range[1]:range[2]
  list(x = xseq, y = predict(m, tibble::data_frame(day = xseq)))
}

data_out <- function(dat) {
  list(x = dat$day, y = dat$value)
}
