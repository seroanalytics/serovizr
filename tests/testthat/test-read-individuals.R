test_that("GET /individual/<pidcol> returns 404 if dataset not found", {
  router <- build_routes(cookie_key)
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/individual/pid/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 404)
  validate_failure_schema(res$body)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$errors[1, "detail"],
               "Did not find dataset with name: testdataset")
})

test_that("GET /individual/<pidcol> returns 400 if pidcol not found", {
  dat <- data.frame(biomarker = "ab",
                    value = 1,
                    day = 1:10)
  local_add_dataset(dat, name = "testdataset")
  router <- build_routes(cookie_key)
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/individual/pid/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 400)
  validate_failure_schema(res$body)
  body <- jsonlite::fromJSON(res$body)
  str(body)
  expect_equal(body$errors[1, "detail"],
               "Id column 'pid' not found.")
})

test_that("can get individual trajectories for uploaded dataset with xcol", {
  dat <- data.frame(biomarker = c("ab", "ba"),
                    pid = c(1, 2),
                    value = 11:20,
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
                              "/dataset/testdataset/individual/pid/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(nrow(body$data$data), 2)
  expect_equal(body$data$data$x[[1]], c(1, 3, 5, 7, 9))
  expect_equal(body$data$data$y[[1]], c(11, 13, 15, 17, 19))
  expect_equal(body$data$data$x[[2]], c(2, 4, 6, 8, 10))
  expect_equal(body$data$data$y[[2]], c(12, 14, 16, 18, 20))
})

test_that("GET /individual/<pidcol>?scale= returns 400 if invalid scale", {
  dat <- data.frame(biomarker = "ab",
                    value = 1,
                    day = 1:10,
                    age = "0-5",
                    sex = c("M", "F"))
  local_add_dataset(dat, name = "testdataset")
  router <- build_routes(cookie_key)
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/individual/pid/",
                              qs = "scale=bad",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 400)
  validate_failure_schema(res$body)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$errors[1, "detail"],
               "'scale' must be one of 'log', 'log2', or 'natural'")
})

test_that("can get trajectories with color", {
  dat <- data.frame(pid = c(1, 2),
                    value = 1,
                    day = 1:10,
                    age = "0-5",
                    sex = c("M", "F"))
  local_add_dataset(dat, name = "testdataset")
  router <- build_routes(cookie_key)
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/individual/pid/",
                              qs = "color=sex",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data$data
  expect_equal(nrow(data), 2)
  expect_equal(data$name, c("F", "M"))
  expect_false(data[1, "line"]$color == data[2, "line"]$color)

  expect_equal(data[1, "x"], list(c(2, 4, 6, 8, 10)))
  expect_equal(data[1, "y"], list(c(1, 1, 1, 1, 1)))
  expect_equal(data[1, "line"]$dash, "solid")


  expect_equal(data[2, "x"], list(c(1, 3, 5, 7, 9)))
  expect_equal(data[2, "y"], list(c(1, 1, 1, 1, 1)))
  expect_equal(data[2, "line"]$dash, "solid")
})

test_that("can get trajectoriees with linetype", {
  dat <- data.frame(pid = 1,
                    value = c(1, 2),
                    day = 1:10,
                    subtype = c("M", "F"))
  router <- build_routes(cookie_key)
  local_add_dataset(dat, name = "testdataset")
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/individual/pid/",
                              qs = "linetype=subtype",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data$data
  expect_equal(nrow(data), 2)
  expect_equal(data$name, c("F", "M"))
  expect_equal(data[1, "line"]$color, data[2, "line"]$color)

  expect_equal(data[1, "x"], list(c(2, 4, 6, 8, 10)))
  expect_equal(data[1, "y"], list(rep(2, 5)))
  expect_equal(data[1, "line"]$dash, "solid")

  expect_equal(data[2, "x"], list(c(1, 3, 5, 7, 9)))
  expect_equal(data[2, "y"], list(rep(1, 5)))
  expect_equal(data[2, "line"]$dash, "dash")
})

test_that("can get trajectoriees with linetype and color", {
  dat <- data.frame(pid = 1,
                    value = c(1, 2),
                    day = 1:10,
                    biomarker = rep(c("ab", "ba"), 5),
                    subtype = c("M", "F"))
  router <- build_routes(cookie_key)
  local_add_dataset(dat, name = "testdataset")
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/individual/pid/",
                              qs = "linetype=subtype&color=biomarker",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data$data
  expect_equal(nrow(data), 2)
  expect_equal(data$name, c("(ab,M)", "(ba,F)"))
  expect_false(data[1, "line"]$color == data[2, "line"]$color)
  expect_equal(data[1, "line"]$dash, "dash")
  expect_equal(data[2, "line"]$dash, "solid")
})

test_that("can get filtered individual trajectories", {
  dat <- data.frame(pid = 1,
                    value = 1:10,
                    day = 1:10,
                    sex = c("M", "F"))
  router <- build_routes(cookie_key)
  local_add_dataset(dat, name = "testdataset")
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/individual/pid/",
                              qs = "filter=sex%3AM",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data$data
  expect_equal(nrow(data), 1)
  expect_equal(data[1, "x"], list(c(1, 3, 5, 7, 9)))
  expect_equal(data[1, "y"], list(c(1, 3, 5, 7, 9)))
})

test_that("can get log data", {
  dat <- data.frame(biomarker = "ab",
                    pid = 1,
                    value = 1:5,
                    day = 1:5)
  router <- build_routes(cookie_key)
  local_add_dataset(dat, name = "testdataset")
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/individual/pid/",
                              qs = "scale=log",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data$data
  expect_equal(nrow(data), 1)
  expect_equal(data[1, "x"], list(1:5))
  expect_equal(unlist(data[1, "y"]),
               jsonlite::fromJSON(
                 jsonlite::toJSON(log(1:5)) # convert to/from json for consistent rounding
               ))
})

test_that("can get dataset with dates", {
  dates <- c("2023/15/01", "2023/16/01", "2023/17/01", "2023/18/01", "2023/20/01")
  dat <- data.frame(biomarker = "ab",
                    pid = 1,
                    value = 1:5,
                    day = dates)
  router <- build_routes(cookie_key)
  post_request <- local_POST_dataset_request(dat,
                                             "testdataset",
                                             cookie = cookie)
  expect_equal(router$call(post_request)$status, 200)
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/individual/pid/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data$data
  expect_equal(nrow(data), 1)
  expect_equal(unlist(data[1, "x"]), as.numeric(lubridate::ydm(dates)))
  expect_equal(unlist(data[1, "y"]), 1:5)
})

test_that("only first 20 individuals are returned", {
  dat <- data.frame(biomarker = "ab",
                    pid = 1:25,
                    value = 1,
                    day = 1:25)
  router <- build_routes(cookie_key)
  post_request <- local_POST_dataset_request(dat,
                                             "testdataset",
                                             cookie = cookie)
  expect_equal(router$call(post_request)$status, 200)
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/individual/pid/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  warnings <- body$data$warnings
  expect_equal(warnings, "25 individuals identified; only the first 20 will be shown.")

  data <- body$data$data
  expect_equal(nrow(data), 20)
})
