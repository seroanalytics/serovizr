target_get_root <- function() {
  jsonlite::unbox("Welcome to serovizr")
}

target_get_version <- function() {
  jsonlite::unbox(as.character(utils::packageVersion("serovizr")))
}

target_post_dataset <- function(req, res) {
  logger::log_info("Parsing multipart form request")
  parsed <- mime::parse_multipart(req)
  xcol <- parsed$xcol
  if (is.null(parsed$file$type) || parsed$file$type != "text/csv") {
    res$status <- 400L
    msg <- "Invalid file type; please upload file of type text/csv."
    error <- list(error = "BAD_REQUEST",
                  detail = msg)
    return(list(status = "failure", errors = list(error), data = NULL))
  }
  file_body <- utils::read.csv(parsed$file$datapath)
  filename <- parsed$file$name
  file_ext <- tools::file_ext(filename)
  if (nchar(file_ext) > 0) {
    filename <- stringr::str_remove_all(filename,
                                        paste0(".", file_ext))
  }
  path <- file.path("uploads", filename)
  if (file.exists(path)) {
    res$status <- 400L
    msg <- paste(filename, "already exists.",
                 "Please choose a unique name for this dataset.")
    error <- list(error = "BAD_REQUEST",
                  detail = msg)
    return(list(status = "failure", errors = list(error), data = NULL))
  }
  required_cols <- c("value", "biomarker", xcol)
  missing_cols <- required_cols[!(required_cols %in% colnames(file_body))]
  if (length(missing_cols) > 0) {
    res$status <- 400L
    error <- list(error = "BAD_REQUEST",
                  detail = paste("Missing required columns:",
                                 paste(missing_cols, collapse = ", ")))
    return(list(status = "failure", errors = list(error), data = NULL))
  }

  logger::log_info(paste("Saving dataset", filename, "to disk"))
  dir.create(path)
  utils::write.csv(file_body, file.path(path, "data"), row.names = FALSE)
  write(xcol, file.path(path, "xcol"))
  porcelain:::response_success(jsonlite::unbox(filename))
}

target_get_dataset <- function(name) {
  logger::log_info(paste("Requesting metadata for dataset:", name))
  dataset <- read_dataset(name)
  logger::log_info(paste("Found dataset:", name))
  dat <- dataset$data
  xcol <- dataset$xcol
  cols <- setdiff(colnames(dat), c("value", "biomarker", xcol))
  if (length(cols) == 0) {
    logger::log_info("No covariates detected")
  } else {
    logger::log_info(paste("Detected covariates:",
                           paste(cols, collapse = ", ")))
  }
  biomarkers <- unique(dat$biomarker)
  logger::log_info(paste("Detected biomarkers:",
                         paste(biomarkers, collapse = ", ")))
  variables <- list()
  for (col in cols) {
    lvls <- unique(dat[, col])
    if (length(lvls) < 12) {
      variables[[col]] <- list(name = jsonlite::unbox(col), levels = lvls)
    }
  }
  list(variables = unname(variables),
       biomarkers = biomarkers,
       xcol = jsonlite::unbox(xcol))
}

target_get_datasets <- function() {
  list.files("uploads")
}

target_get_trace <- function(name,
                             biomarker,
                             filter = NULL,
                             disaggregate = NULL) {
  logger::log_info(paste("Requesting data from", name,
                         "with biomarker", biomarker))
  dataset <- read_dataset(name)
  dat <- dataset$data
  xcol <- dataset$xcol
  cols <- colnames(dat)
  if (!is.null(filter)) {
    filters <- strsplit(filter, "+", fixed = TRUE)[[1]]
    logger::log_info(paste("Filtering by variables:", paste(filters,
                                                            collapse = ", ")))
   for (f in filters) {
     dat <- apply_filter(f, dat, cols)
   }
  }
  dat <- dat[dat["biomarker"] == biomarker,]
  if (length(disaggregate) > 0) {
    logger::log_info(paste("Disaggregating by variables:", disaggregate))
    groups <- split(dat, eval(parse(text = paste("~", disaggregate))))
    nms <- names(groups)
    return(lapply(seq_along(groups), function(i) {
      model <- with_warnings(model_out(groups[[i]], xcol))
      list(name = jsonlite::unbox(nms[[i]]),
           model = model$output,
           raw = data_out(groups[[i]], xcol),
           warnings = model$warnings)
    }))
  } else {
    logger::log_info("Returning single trace")
    model <- with_warnings(model_out(dat, xcol))
    nm <- ifelse(is.null(filter), "all", filter)
    str(nm)
    return(list(list(name = jsonlite::unbox(nm),
                     model = model$output,
                     raw = data_out(dat, xcol),
                     warnings = model$warnings)))
  }
}

read_dataset <- function(name) {
  path <- file.path("uploads", name)
  if (!file.exists(path)) {
    porcelain::porcelain_stop(paste("Did not find dataset with name:", name),
                              code = "DATASET_NOT_FOUND", status_code = 404L)
  }
  dat <- utils::read.csv(file.path(path, "data"))
  dat$value <- as.numeric(dat$value)
  xcol <- readLines(file.path(path, "xcol"))
  list(data = dat, xcol = xcol)
}

model_out <- function(dat, xcol) {
  n <- nrow(dat)
  if (n == 0) {
    return(list(x = list(), y = list()))
  }
  if (n > 1000) {
    m <- mgcv::gam(value ~ s(eval(parse(text = xcol)), bs = "cs"),
                   data = dat, method = "REML")
  } else {
    m <- stats::loess(value ~ eval(parse(text = xcol)), data = dat, span = 0.75)
  }
  range <- range(dat[, xcol], na.rm = TRUE)
  xseq <- range[1]:range[2]
  xdf <- tibble::data_frame(xcol = xseq)
  names(xdf) <- xcol
  list(x = xseq, y = stats::predict(m, xdf))
}

data_out <- function(dat, xcol) {
  list(x = dat[, xcol], y = dat$value)
}

apply_filter <- function(filter, dat, cols) {
  filter_def <- strsplit(filter, ":")
  filter_var <- filter_def[[1]][1]
  filter_level <- filter_def[[1]][2]
  if (!(filter_var %in% cols)) {
    porcelain::porcelain_stop(paste("Column",
                                    filter_var,
                                    "not found in data"),
                              code = "BAD_REQUEST", status_code = 400L)
  }
  dat[dat[filter_var] == filter_level,]
}
