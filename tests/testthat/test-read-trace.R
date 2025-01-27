test_that("GET /trace/<biomarker> returns 404 if dataset not found", {
  router <- build_routes(cookie_key)
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 404)
  validate_failure_schema(res$body)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$errors[1, "detail"],
               "Did not find dataset with name: testdataset")
})

test_that("can get trace for uploaded dataset with xcol", {
  dat <- data.frame(biomarker = c("ab", "ba"),
                    value = 1,
                    time = 1:10,
                    age = "0-5",
                    sex = c("M", "F"))
  router <- build_routes(cookie_key)
  post_request <- local_POST_dataset_request(dat,
                                             "testdataset",
                                             xcol = "time",
                                             cookie = cookie)
  expect_equal(router$call(post_request)$status, 200)

  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  expected_warnings <- list("span too small.   fewer data values than degrees of freedom.",
                            "pseudoinverse used at 0.96",
                            "neighborhood radius 4.04",
                            "reciprocal condition number  0",
                            "There are other near singularities as well. 16.322")
  expected <- jsonlite::toJSON(list(list(name = jsonlite::unbox("all"),
                                         model = list(
                                           x = 1:9,
                                           y = rep(1, 9)
                                         ),
                                         raw = list(
                                           x = c(1, 3, 5, 7, 9),
                                           y = c(1, 1, 1, 1, 1)
                                         ),
                                         warnings = lapply(expected_warnings,
                                                           jsonlite::unbox))
  ))
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data, jsonlite::fromJSON(expected))
})

test_that("GET /trace/<biomarker>?scale= returns 400 if invalid scale", {
  dat <- data.frame(biomarker = "ab",
                    value = 1,
                    day = 1:10,
                    age = "0-5",
                    sex = c("M", "F"))
  local_add_dataset(dat, name = "testdataset")
  router <- build_routes(cookie_key)
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              qs = "scale=bad",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 400)
  validate_failure_schema(res$body)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$errors[1, "detail"],
               "'scale' must be one of 'log', 'log2', or 'natural'")
})

test_that("can get disgagregated traces", {
  dat <- data.frame(biomarker = "ab",
                    value = 1,
                    day = 1:10,
                    age = "0-5",
                    sex = c("M", "F"))
  local_add_dataset(dat, name = "testdataset")
  router <- build_routes(cookie_key)
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              qs = "disaggregate=sex",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data
  expect_equal(nrow(data), 2)
  expect_equal(data$name, c("F", "M"))
  expect_equal(data$raw[1, "x"], list(c(2, 4, 6, 8, 10)))
  expect_equal(data$raw[1, "y"], list(c(1, 1, 1, 1, 1)))
  expect_equal(data$raw[2, "x"], list(c(1, 3, 5, 7, 9)))
  expect_equal(data$raw[2, "y"], list(c(1, 1, 1, 1, 1)))

  expect_equal(data$model[1, "x"], list(2:10))
  expect_equal(data$model[1, "y"], list(rep(1, 9)))
  expect_equal(data$model[2, "x"], list(1:9))
  expect_equal(data$model[2, "y"], list(rep(1, 9)))
})

test_that("can get filtered trace", {
  dat <- data.frame(biomarker = "ab",
                    value = rep(c(0, 1, 2, 3), 5),
                    day = 1:20,
                    age = rep(c("0-5", "0-5", "5+", "5+"), 5),
                    sex = c("M", "F"))
  router <- build_routes(cookie_key)
  local_add_dataset(dat, name = "testdataset")
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              qs = "filter=sex%3AM",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data
  expect_equal(nrow(data), 1)
  expect_equal(data$name, "sex:M")
  expect_equal(data$raw[1, "x"], list(c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)))
  expect_equal(data$raw[1, "y"], list(c(0, 2, 0, 2, 0, 2, 0, 2, 0, 2)))
})

test_that("can get trace filtered by multiple variables", {
  dat <- data.frame(biomarker = "ab",
                    value = rep(c(0, 1, 2, 3), 5),
                    day = 1:20,
                    age = rep(c("0-5", "0-5", "5+", "5+"), 5),
                    sex = c("M", "F"))
  router <- build_routes(cookie_key)
  local_add_dataset(dat, name = "testdataset")
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              qs = "filter=sex%3AM%2Bage%3A0-5",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data
  expect_equal(nrow(data), 1)
  expect_equal(data$name, "sex:M+age:0-5")
  expect_equal(data$raw[1, "x"], list(c(1, 5, 9, 13, 17)))
  expect_equal(data$raw[1, "y"], list(c(0, 0, 0, 0, 0)))
})

test_that("can get disaggregated and filtered traces", {
  dat <- data.frame(biomarker = "ab",
                    value = rep(c(0, 1, 2, 3), 5),
                    day = 1:20,
                    age = rep(c("0-5", "0-5", "5+", "5+"), 5),
                    sex = c("M", "F"))
  router <- build_routes(cookie_key)
  local_add_dataset(dat, name = "testdataset")
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              qs = "disaggregate=age&filter=sex:M",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data
  expect_equal(nrow(data), 2)
  expect_equal(data$name, c("0-5", "5+"))
  expect_equal(data$raw[1, "x"], list(c(1, 5, 9, 13, 17)))
  expect_equal(data$raw[1, "y"], list(c(0, 0, 0, 0, 0)))
  expect_equal(data$raw[2, "x"], list(c(3, 7, 11, 15, 19)))
  expect_equal(data$raw[2, "y"], list(c(2, 2, 2, 2, 2)))
})

test_that("can get log data", {
  dat <- data.frame(biomarker = "ab",
                    value = 1:5,
                    day = 1:5)
  router <- build_routes(cookie_key)
  local_add_dataset(dat, name = "testdataset")
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              qs = "scale=log",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data
  expect_equal(nrow(data), 1)
  expect_equal(data$name, "all")
  expect_equal(data$raw[1, "x"], list(1:5))
  expect_equal(unlist(data$raw[1, "y"]),
               jsonlite::fromJSON(
                 jsonlite::toJSON(log(1:5)) # convert to/from json for consistent rounding
               ))
})

test_that("can get log2 data", {
  dat <- data.frame(biomarker = "ab",
                    value = 1:5,
                    day = 1:5)
  router <- build_routes(cookie_key)
  local_add_dataset(dat, name = "testdataset")
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              qs = "scale=log2",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data
  expect_equal(nrow(data), 1)
  expect_equal(data$name, "all")
  expect_equal(data$raw[1, "x"], list(1:5))
  expect_equal(unlist(data$raw[1, "y"]),
               jsonlite::fromJSON(
                 jsonlite::toJSON(log2(1:5)) # convert to/from json for consistent rounding
               ))
})

test_that("can use loess model options", {
  dat <- data.frame(biomarker = "ab",
                    value = 1:5,
                    day = 1:5)
  router <- build_routes(cookie_key)
  local_add_dataset(dat, name = "testdataset")
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              qs = "method=loess&span=0.5",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data

  suppressWarnings(m <- stats::loess(value ~ day, data = dat, span = 0.5))
  xdf <- tibble::tibble(day = 1:5)
  expected <- stats::predict(m, xdf)
  expect_equal(unlist(data$model[1, "y"]),
               jsonlite::fromJSON(
                 jsonlite::toJSON(expected) # convert to/from json for consistent rounding
               ))
})

test_that("can use gam model options", {
  dat <- data.frame(biomarker = "ab",
                    value = 1:5,
                    day = 1:5)
  router <- build_routes(cookie_key)
  local_add_dataset(dat, name = "testdataset")
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              qs = "method=gam&k=2",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data
  suppressWarnings(m <- mgcv::gam(value ~ s(day, bs = "cs", k = 2),
                                  data = dat, method = "REML"))
  xdf <- tibble::tibble(day = 1:5)
  expected <- stats::predict(m, xdf)
  expect_equal(unlist(data$model[1, "y"]),
               jsonlite::fromJSON(
                 jsonlite::toJSON(expected) # convert to/from json for consistent rounding
               ))
})

test_that("error running the model results in a 400", {
  dat <- data.frame(biomarker = "ab",
                    value = 1:5,
                    day = 1:5)
  router <- build_routes(cookie_key)
  local_add_dataset(dat, name = "testdataset")
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              qs = "method=gam&k=10",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 400)
  validate_failure_schema(res$body)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$errors[1, "detail"],
               "day has insufficient unique values to support 10 knots: reduce k.")
})

test_that("can get dataset with dates", {
  dates <- c("2023/15/01", "2023/16/01", "2023/17/01", "2023/18/01", "2023/20/01")
  dat <- data.frame(biomarker = "ab",
                    value = 1:5,
                    day = dates)
  router <- build_routes(cookie_key)
  post_request <- local_POST_dataset_request(dat,
                                             "testdataset",
                                             cookie = cookie)
  expect_equal(router$call(post_request)$status, 200)
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data
  expect_equal(nrow(data), 1)
  expect_equal(data$name, "all")
  expect_equal(unlist(data$raw[1, "x"]), c("2023-01-15", "2023-01-16", "2023-01-17", "2023-01-18", "2023-01-20"))
  expect_equal(unlist(data$raw[1, "y"]), 1:5)

  full_range_dates <- c("2023-01-15", "2023-01-16", "2023-01-17", "2023-01-18", "2023-01-19", "2023-01-20")
  expect_equal(unlist(data$model[1, "x"]), full_range_dates)
  parsed <- lubridate::parse_date_time(dates, c("dmy", "mdy", "ymd", "ydm"))
  suppressWarnings({ m <- stats::loess(value ~ as.numeric(day), data = data.frame(day = parsed,
                                                                                  value = 1:5)) })
  parsed_full_range <- lubridate::parse_date_time(full_range_dates, c("dmy", "mdy", "ymd", "ydm"))
  expected <- stats::predict(m, parsed_full_range)
  expect_equal(unlist(data$model[1, "y"]), expected)
})

test_that("errors for disaggregated traces are returned as warnings", {
  dat <- data.frame(biomarker = "ab",
                    value = 1:10,
                    day = 1:10,
                    sex = c(rep("M", 9), "F"))
  router <- build_routes(cookie_key)
  local_add_dataset(dat, name = "testdataset")
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              qs = "method=gam&k=10&disaggregate=sex",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data
  expect_equal(data$warnings,
               list("Not enough (non-NA) data to do anything meaningful",
                    "day has insufficient unique values to support 10 knots: reduce k."))
  expect_true(all(is.na(data$model)))
})


test_that("can get public dataset", {
  router <- build_routes(cookie_key)
  res <- router$call(make_req("GET",
                              "/dataset/test/trace/sVNT/",
                              qs = "public=TRUE",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data
  expect_equal(nrow(data), 1)
})
