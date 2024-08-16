target_get_root <- function() {
  jsonlite::unbox("Welcome to serovizr")
}

target_get_version <- function() {
  jsonlite::toJSON(as.character(utils::packageVersion("serovizr")),
                   auto_unbox = TRUE)
}

target_post_dataset <- function(req, res) {
  parsed <- Rook::Multipart$parse(req)
  file_body <- utils::read.csv(parsed$file$tempfile)
  filename <- parsed$file$filename
  filename <- stringr::str_remove_all(filename,
                                      paste0(".", tools::file_ext(filename)))
  path <- file.path("uploads", filename)
  if (file.exists(path)) {
    res$status <- 400L
    msg <- paste(filename, "already exists.",
                 "Please choose a unique name for this dataset.")
    error <- list(error = "BAD_REQUEST",
                  detail = msg)
    return(list(status = "failure", errors = list(error), data = NULL))
  }
  required_cols <- c("value", "biomarker")
  missing_cols <- required_cols[!(required_cols %in% colnames(file_body))]
  if (length(missing_cols) > 0) {
    res$status <- 400L
    error <- list(error = "BAD_REQUEST",
                  detail = paste("Missing required columns:",
                                 paste(missing_cols, collapse = ", ")))
    return(list(status = "failure", errors = list(error), data = NULL))
  }

  utils::write.csv(file_body, path, row.names = FALSE)
  return(filename)
}

target_get_dataset <- function(name) {
  dat <- read_dataset(name)
  cols <- setdiff(colnames(dat), c("value", "biomarker", "day"))
  biomarkers <- unique(dat$biomarker)
  variables <- list()
  for (col in cols) {
    lvls <- unique(dat[, col])
    if (length(lvls) < 12) {
      variables[[col]] <- list(name = jsonlite::unbox(col), levels = lvls)
    }
  }
  list(variables = unname(variables), biomarkers = biomarkers)
}

target_get_datasets <- function() {
  files <- list.files("uploads")
  jsonlite::toJSON(files)
}

target_get_trace <- function(name, biomarker, facet = NULL, trace = NULL) {
  logger::log_info(paste("Requesting data from", name,
                         "with biomarker", biomarker))
  logger::log_info(paste("Filtering by facet variables", facet))
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
  if (length(trace) > 0) {
    logger::log_info(paste("Disaggregating by trace variables", trace))
    groups <- split(dat, eval(parse(text = paste("~", trace))))
    return(lapply(seq_along(groups), function(i) {
      list(name = jsonlite::unbox(nms[[g]]),
           model = model_out(groups[[i]]),
           raw = data_out(groups[[i]]))
    }))
  } else {
    logger::log_info("Returning single trace")
    return(list(list(name = jsonlite::unbox("all"),
                     model = model_out(dat),
                     raw = data_out(dat))))
  }
}

read_dataset <- function(name) {
  path <- file.path("uploads", name)
  if (!file.exists(path)) {
    porcelain::porcelain_stop(paste("Did not find dataset with name ", name),
                              code = "BAD_REQUEST", status_code = 404L)
  }
  dat <- utils::read.csv(path)
  dat$value <- as.numeric(dat$value)
  dat
}

model_out <- function(dat) {
  n <- nrow(dat)
  if (n == 0) {
    return(list(x = list(), y = list()))
  }
  if (n > 1000) {
    m <- mgcv::gam(value ~ s(day, bs = "cs"), data = dat, method = "REML")
  } else {
    m <- stats::loess(value ~ day, data = dat, span = 0.75)
  }
  range <- range(dat$day, na.rm = TRUE)
  xseq <- range[1]:range[2]
  list(x = xseq, y = stats::predict(m, tibble::data_frame(day = xseq)))
}

data_out <- function(dat) {
  list(x = dat$day, y = dat$value)
}
