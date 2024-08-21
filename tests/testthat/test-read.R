test_that("GET /dataset<name> returns 404 if dataset not found", {
  router <- build_routes()
  res <- router$request("GET", "/dataset/testdataset/")
  expect_equal(res$status, 404)
  validate_failure_schema(res$body)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$errors[1, "detail"],
               "Did not find dataset with name: testdataset")
})

test_that("GET /trace/<biomarker> returns 404 if dataset not found", {
  router <- build_routes()
  res <- router$request("GET", "/dataset/testdataset/trace/ab/")
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
  request <- local_POST_dataset_request(dat,
                                        "testdata",
                                        xcol = "time")
  router <- build_routes()
  res <- router$call(request)
  expect_equal(res$status, 200)
  res <- router$request("GET", "/dataset/testdata/trace/ab/")
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
  str(res$body)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data, jsonlite::fromJSON(expected))
})

test_that("can get disgagregated traces", {
  dat <- data.frame(biomarker = "ab",
                    value = 1,
                    day = 1:10,
                    age = "0-5",
                    sex = c("M", "F"))
  local_add_dataset(dat,
                    "testdataset")
  router <- build_routes()
  res <- router$request("GET", "/dataset/testdataset/trace/ab/",
                        query = list(disaggregate = "sex"))
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
  local_add_dataset(dat,
                    "testdataset")
  router <- build_routes()
  res <- router$request("GET", "/dataset/testdataset/trace/ab/",
                        query = list(filter = "sex:M"))
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
  local_add_dataset(dat,
                    "testdataset")
  router <- build_routes()
  res <- router$request("GET", "/dataset/testdataset/trace/ab/",
                        query = list(filter = "sex%3AM%2Bage%3A0-5"))
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
  local_add_dataset(dat,
                    "testdataset")
  router <- build_routes()
  res <- router$request("GET", "/dataset/testdataset/trace/ab/",
                        query = list(disaggregate = "age", filter = "sex:M"))
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
