test_that("uploading data with wrong columns returns 400", {
  router <- build_routes()
  set.seed(1)
  request <- local_POST_dataset_request(data.frame(biomarker = "ab",
                                                   day = 1:10),
                                        "testdataset")
  res <- router$call(request)
  expect_equal(res$status, 400)
  validate_failure_schema(res$body)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$errors[1, "detail"], "Missing required columns: value")

  set.seed(1)
  request <- local_POST_dataset_request(data.frame(biomarker = "ab",
                                                   value = 1:10),
                                        "testdataset")
  res <- router$call(request)
  expect_equal(res$status, 400)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$errors[1, "detail"], "Missing required columns: day")

  set.seed(1)
  request <- local_POST_dataset_request(data.frame(day = 1:10,
                                                   value = 1:10),
                                        "testdataset")
  res <- router$call(request)
  expect_equal(res$status, 400)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$errors[1, "detail"], "Missing required columns: biomarker")
})

test_that("uploading dataset with duplicate name returns 400", {
  router <- build_routes()
  set.seed(1)
  request <- local_POST_dataset_request(data.frame(biomarker = "ab",
                                                   day = 1:10,
                                                   value = 1),
                                        "testdataset")
  res <- router$call(request)
  expect_equal(res$status, 200)

  set.seed(1) #so that same session id gets created
  res <- router$call(request)
  body <- jsonlite::fromJSON(res$body)
  validate_failure_schema(res$body)
  expect_equal(body$errors[1, "detail"],
               "testdataset already exists. Please choose a unique name for this dataset.")
})

test_that("can upload dataset with different xcol", {
  router <- build_routes()
  set.seed(1)
  request <- local_POST_dataset_request(data.frame(biomarker = "ab",
                                                   time = 1:10,
                                                   value = 1),
                                        "testdataset",
                                        xcol = "time")
  res <- router$call(request)
  expect_equal(res$status, 200)
})

test_that("uploading wrong file type returns 400", {
  router <- build_routes()
  request <- local_POST_dataset_request_bad_file()
  res <- router$call(request)
  expect_equal(res$status, 400)
  body <- jsonlite::fromJSON(res$body)
  validate_failure_schema(res$body)
  expect_equal(body$errors[1, "detail"],
               "Invalid file type; please upload file of type text/csv.")
})

test_that("saves file and xcol", {
  router <- build_routes()
  set.seed(1)
  session_id <- generate_session_id()
  set.seed(1) #so that same id generated when processing request
  request <- local_POST_dataset_request(data.frame(biomarker = "ab",
                                                   time = 1:10,
                                                   value = 1),
                                        filename = "testdataset",
                                        xcol = "time")
  res <- router$call(request)
  expect_equal(res$status, 200)
  dat <- utils::read.csv(file.path("uploads", session_id, "/testdataset/data"))
  expect_equal(colnames(dat), c("biomarker", "time", "value"))
  expect_equal(nrow(dat), 10)
  xcol <- readLines(file.path("uploads", session_id, "/testdataset/xcol"))
  expect_equal(xcol, "time")
})

test_that("can get uploaded dataset metadata with default xcol", {
  set.seed(1)
  request <- local_POST_dataset_request(data.frame(biomarker = c("ab", "ba"),
                                                   value = 1,
                                                   day = 1:10,
                                                   age = "0-5",
                                                   sex = c("M", "F")),
                                        "testdata")
  router <- build_routes(cookie_key)
  res <- router$call(request)
  expect_equal(res$status, 200)

  res <- router$call(make_req("GET",
                              "/dataset/testdata/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data$variables$name, c("age", "sex"))
  expect_equal(body$data$variables$levels, list(c("0-5"), c("M", "F")))
  expect_equal(body$data$biomarkers, c("ab", "ba"))
  expect_equal(body$data$xcol, "day")
})

test_that("can get uploaded dataset metadata with xcol", {
  set.seed(1)
  request <- local_POST_dataset_request(data.frame(biomarker = c("ab", "ba"),
                                                   value = 1,
                                                   time = 1:10,
                                                   age = "0-5",
                                                   sex = c("M", "F")),
                                        "testdata",
                                        xcol = "time")
  router <- build_routes(cookie_key)
  res <- router$call(request)
  expect_equal(res$status, 200)
  res <- router$call(make_req("GET",
                              "/dataset/testdata/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data$variables$name, c("age", "sex"))
  expect_equal(body$data$variables$levels, list(c("0-5"), c("M", "F")))
  expect_equal(body$data$biomarkers, c("ab", "ba"))
  expect_equal(body$data$xcol, "time")
})

test_that("can get uploaded dataset without covariates", {
  set.seed(1)
  request <- local_POST_dataset_request(data.frame(biomarker = c("ab", "ba"),
                                                   value = 1,
                                                   time = 1:10),
                                        "testdata",
                                        xcol = "time")
  router <- build_routes(cookie_key)
  res <- router$call(request)
  expect_equal(res$status, 200)

  res <- router$call(make_req("GET",
                              "/dataset/testdata/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(length(body$data$variables), 0)
  expect_equal(body$data$biomarkers, c("ab", "ba"))
  expect_equal(body$data$xcol, "time")
})

test_that("returns 400 if no xcol", {
  request <- local_POST_dataset_request_no_xcol(data.frame(biomarker = c("ab", "ba"),
                                                   value = 1,
                                                   time = 1:10),
                                        "testdata")
  router <- build_routes()
  res <- router$call(request)
  expect_equal(res$status, 400)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$errors[1, "detail"],
               "Missing required field: xcol.")
})
