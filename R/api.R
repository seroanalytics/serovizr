target_get_root <- function(req) {
  get_or_create_session_id(req)
  jsonlite::unbox("Welcome to serovizr")
}

target_get_version <- function(req) {
  get_or_create_session_id(req)
  jsonlite::unbox(as.character(utils::packageVersion("serovizr")))
}

target_post_dataset <- function(req, res) {
  session_id <- get_or_create_session_id(req)
  logger::log_info("Parsing multipart form request")
  parsed <- mime::parse_multipart(req)
  xcol <- get_xcol(parsed)
  name <- get_dataset_name(parsed)
  if (is.null(parsed$file$type) || parsed$file$type != "text/csv") {
    return(invalid_file_type(res))
  }
  file_body <- utils::read.csv(parsed$file$datapath)
  path <- file.path("uploads", session_id, name)
  if (dir.exists(path)) {
    return(duplicate_dataset_name(res, name))
  }
  required_cols <- c("value", "biomarker", xcol)
  missing_cols <- required_cols[!(required_cols %in% colnames(file_body))]
  if (length(missing_cols) > 0) {
    return(missing_columns(res, missing_cols))
  }

  file_body[, xcol] <- get_parsed_values(file_body[, xcol])

  if (all(is.na(file_body[, xcol]))) {
    return(invalid_xcol(res))
  }

  logger::log_info(paste("Saving dataset", name, "to disk"))
  save_dataset(path, file_body, xcol)

  response_success(jsonlite::unbox(name))
}

save_dataset <- function(path, file_body, xcol) {
  xtype <- get_xtype(file_body[, xcol])
  dir.create(path, recursive = TRUE)
  utils::write.csv(file_body, file.path(path, "data"), row.names = FALSE)
  write(xcol, file.path(path, "xcol"))
  write(xtype, file.path(path, "xtype"))
}

get_parsed_values <- function(raw_values) {
  suppressWarnings({
    values <- as.numeric(raw_values)
  })

  if (all(is.na(values))) {
    suppressWarnings({
      values <- parse_date(raw_values)
    })
  }
  values
}

get_xtype <- function(values) {
  if (is.numeric(values)) {
    logger::log_info("Detected numeric values in x column")
    return("number")
  } else {
    logger::log_info("Detected date values in x column")
    return("date")
  }
}

get_dataset_name <- function(parsed) {
  name <- parsed$name
  if (is.null(name)) {
    filename <- parsed$file$name
    file_ext <- tools::file_ext(filename)
    if (nchar(file_ext) > 0) {
      filename <- stringr::str_remove_all(filename,
                                          paste0(".", file_ext))
    }
    name <- filename
  }
  return(name)
}

get_xcol <- function(parsed) {
  xcol <- parsed$xcol
  if (is.null(xcol)) {
    xcol <- "day"
  }
  return(xcol)
}

target_delete_dataset <- function(name, req) {
  session_id <- get_or_create_session_id(req)
  path <- file.path("uploads", session_id, name)
  if (!file.exists(path)) {
    porcelain::porcelain_stop(paste("Did not find dataset with name:", name),
                              code = "DATASET_NOT_FOUND", status_code = 404L)
  }
  logger::log_info(paste("Deleting dataset: ", name))
  fs::dir_delete(path)
  jsonlite::unbox(name)
}

target_get_dataset <- function(name, req) {
  logger::log_info(paste("Requesting metadata for dataset:", name))
  dataset <- read_dataset(req, name, "natural")
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
    variables[[col]] <- list(name = jsonlite::unbox(col), levels = lvls)
  }
  list(variables = unname(variables),
       biomarkers = biomarkers,
       xcol = jsonlite::unbox(xcol))
}

target_get_datasets <- function(req) {
  session_id <- get_or_create_session_id(req)
  list.files(file.path("uploads", session_id))
}

target_get_trace <- function(name,
                             biomarker,
                             req,
                             filter = NULL,
                             disaggregate = NULL,
                             scale = "natural",
                             method = "auto",
                             span = 0.75,
                             k = 10) {
  biomarker <- httpuv::decodeURIComponent(biomarker)
  logger::log_info(paste("Requesting data from", name,
                         "with biomarker", biomarker))
  dataset <- read_dataset(req, name, scale)
  dat <- dataset$data
  xcol <- dataset$xcol
  xtype <- dataset$xtype
  dat <- apply_filters(dat, filter)
  dat <- dat[dat["biomarker"] == biomarker, ]
  if (length(disaggregate) > 0) {
    logger::log_info(paste("Disaggregating by variables:", disaggregate))
    groups <- split(dat, eval(parse(text = paste("~", disaggregate))))
    nms <- names(groups)
    return(lapply(seq_along(groups), function(i) {
      model <- with_warnings(model_out(groups[[i]],
                                       xcol = xcol,
                                       xtype = xtype,
                                       method = method,
                                       span = span,
                                       k = k), stop_on_error = FALSE)
      list(name = jsonlite::unbox(nms[[i]]),
           model = model$output,
           raw = data_out(groups[[i]], xcol),
           warnings = model$warnings)
    }))
  } else {
    logger::log_info("Returning single trace")
    model <- with_warnings(model_out(dat,
                                     xcol = xcol,
                                     xtype = xtype,
                                     method = method,
                                     span = span,
                                     k = k))
    nm <- ifelse(is.null(filter), "all", filter)
    return(list(list(name = jsonlite::unbox(nm),
                     model = model$output,
                     raw = data_out(dat, xcol),
                     warnings = model$warnings)))
  }
}

target_get_individual <- function(req,
                                    name,
                                    pidcol,
                                    scale = "natural",
                                    filter = NULL,
                                    color = NULL,
                                    linetype = NULL,
                                    page = 1) {
  .data <- value <- NULL

  data <- read_dataset(req, name, scale)
  dat <- data$data
  xcol <- data$xcol

  if (!(pidcol %in% colnames(dat))) {
      porcelain::porcelain_stop(sprintf("Id column '%s' not found.", pidcol))
  }

  dat <- apply_filters(dat, filter)
  if (is.null(color)) {
    if (is.null(linetype)) {
      aes <- ggplot2::aes(x = .data[[xcol]], y = value)
    } else {
      aes <- ggplot2::aes(x = .data[[xcol]], y = value,
                          linetype = .data[[linetype]])
    }
  } else {
    if (is.null(linetype)) {
      aes <- ggplot2::aes(x = .data[[xcol]], y = value,
                          color = .data[[color]])
    } else {
      aes <- ggplot2::aes(x = .data[[xcol]], y = value,
                          color = .data[[color]],
                          linetype = .data[[linetype]])
    }
  }

  warnings <- NULL
  ids <- unique(dat[[pidcol]])
  if (length(ids) > 20) {
    msg <- paste(length(ids),
                 "individuals identified; only the first 20 will be shown.")
    warnings <- c(warnings, msg)
    dat <- dat[dat[[pidcol]] %in% ids[1:20], ]
  }

  # Facets in plotlyjs are quite a pain. Using ggplot2 and plotly R
  # packages to generate the plotly data and layout objects is a bit slower
  # than just generating data series in R and letting the front-end handle the
  # presentation logic, but is much easier to get right!
  p <- with_warnings(ggplot2::ggplot(dat) +
                       ggplot2::geom_line(aes) +
                       ggplot2::facet_wrap(pidcol) +
                       ggplot2::theme_bw() +
                       ggplot2::labs(x = xcol, y = "Antibody titre",
                                     linetype = linetype,
                                     color = color))
  warnings <- c(warnings, p$warnings)

  q <- plotly::ggplotly(p$output)
  jsonlite::toJSON(
    list(data = as.list(q$x$data),
         layout = as.list(q$x$layout),
         warnings = warnings),
    auto_unbox = TRUE, null = "null")
}

read_dataset <- function(req, name, scale) {
  validate_scale(scale)
  session_id <- get_or_create_session_id(req)
  path <- file.path("uploads", session_id, name)
  if (!file.exists(path)) {
    porcelain::porcelain_stop(paste("Did not find dataset with name:", name),
                              code = "DATASET_NOT_FOUND", status_code = 404L)
  }
  dat <- utils::read.csv(file.path(path, "data"))
  dat$value <- as.numeric(dat$value)
  if (scale == "log") {
    dat$value <- log(dat$value)
  }
  if (scale == "log2") {
    dat$value <- log2(dat$value)
  }
  xcol <- readLines(file.path(path, "xcol"))
  xtype <- readLines(file.path(path, "xtype"))
  logger::log_info("Parsing x column values")
  if (xtype == "date") {
    dat[, xcol] <- as.Date(dat[, xcol], origin = "1970-01-01")
  }
  list(data = dat, xcol = xcol, xtype = xtype)
}

model_out <- function(dat, xcol,
                      xtype = "number",
                      method = "auto",
                      span = 0.75,
                      k = 10) {
  n <- nrow(dat)
  if (n == 0) {
    return(list(x = list(), y = list()))
  }
  dat[, xcol] <- as.numeric(dat[, xcol])
  if ((n > 1000 && method == "auto") || method == "gam") {
    fmla <- sprintf("value ~ s(%s, bs   = 'cs', k = %f)", xcol, k)
    m <- mgcv::gam(eval(parse(text = fmla)),
                   data = dat, method = "REML")
  } else {
    fmla <- sprintf("value ~ %s", xcol)
    m <- stats::loess(fmla, data = dat, span = span)
  }
  range <- range(dat[, xcol], na.rm = TRUE)
  xseq <- range[1]:range[2]
  xdf <- tibble::tibble(xcol = xseq)
  names(xdf) <- xcol
  if (xtype == "date") {
    xseq <- format(as.Date(xseq, origin = "1970-01-01"))
  }
  list(x = xseq, y = stats::predict(m, xdf))
}

data_out <- function(dat, xcol) {
  list(x = dat[, xcol], y = dat$value)
}

apply_filters <- function(dat, filter) {
  if (!is.null(filter)) {
    filters <- strsplit(filter, "+", fixed = TRUE)[[1]]
    logger::log_info(paste("Filtering by variables:", paste(filters,
                                                            collapse = ", ")))
    cols <- colnames(dat)
    for (f in filters) {
      dat <- apply_filter(f, dat, cols)
    }
  }
  return(dat)
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
  dat[dat[filter_var] == filter_level, ]
}

bad_request_response <- function(msg) {
  error <- list(error = "BAD_REQUEST",
                detail = msg)
  return(list(status = "failure", errors = list(error), data = NULL))
}

get_or_create_session_id <- function(req) {
  if (is.null(req$session$id)) {
    logger::log_info("Creating new session id")
    req$session$id <- generate_session_id()
  }
  as.character(req$session$id)
}

generate_session_id <- function() {
  tolower(rawToChar(sample(c(as.raw(sample(c(65:90, 97:122),
                                           5,
                                           replace = TRUE)),
                             as.raw(sample(48:57,
                                           5,
                                           replace = TRUE))))))
}

response_success <- function(data) {
  list(status = jsonlite::unbox("success"), errors = NULL,
       data = data)
}
