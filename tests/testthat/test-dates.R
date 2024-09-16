test_that("d/m/y", {
  router <- build_routes(cookie_key)
  request <- local_POST_dataset_request(data.frame(biomarker = "ab",
                                                   day = c("01/01/2024",
                                                           "02/01/2024"),
                                                   value = 1:10),
                                        "testdataset",
                                        cookie = cookie)
  upload_res <- router$call(request)
  expect_equal(upload_res$status, 200)

  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              HTTP_COOKIE = cookie))

  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data
  expect_equal(unlist(data$raw[1, "x"]), rep(c("2024-01-01", "2024-01-02"), 5))
})

test_that("d-m-y", {
  router <- build_routes(cookie_key)
  request <- local_POST_dataset_request(data.frame(biomarker = "ab",
                                                   day = c("01-01-2024",
                                                           "02-01-2024"),
                                                   value = 1:10),
                                        "testdataset",
                                        cookie = cookie)
  upload_res <- router$call(request)
  expect_equal(upload_res$status, 200)

  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              HTTP_COOKIE = cookie))

  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data
  expect_equal(unlist(data$raw[1, "x"]), rep(c("2024-01-01", "2024-01-02"), 5))
})

test_that("y-m-d", {
  router <- build_routes(cookie_key)
  request <- local_POST_dataset_request(data.frame(biomarker = "ab",
                                                   day = c("2024-01-13",
                                                           "2024-01-14"),
                                                   value = 1:10),
                                        "testdataset",
                                        cookie = cookie)
  upload_res <- router$call(request)
  expect_equal(upload_res$status, 200)

  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              HTTP_COOKIE = cookie))

  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data
  expect_equal(unlist(data$raw[1, "x"]), rep(c("2024-01-13", "2024-01-14"), 5))
})

test_that("y/d/m", {
  router <- build_routes(cookie_key)
  request <- local_POST_dataset_request(data.frame(biomarker = "ab",
                                                   day = c("2024/14/01",
                                                           "2024/15/01"),
                                                   value = 1:10),
                                        "testdataset",
                                        cookie = cookie)
  upload_res <- router$call(request)
  expect_equal(upload_res$status, 200)

  res <- router$call(make_req("GET",
                              "/dataset/testdataset/trace/ab/",
                              HTTP_COOKIE = cookie))

  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  data <- body$data
  expect_equal(unlist(data$raw[1, "x"]), rep(c("2024-01-14", "2024-01-15"), 5))
})
