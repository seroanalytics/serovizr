test_that("GET /", {
  res <- target_get_root()
  expect_equal(res, jsonlite::unbox("Welcome to serovizr"))

  endpoint <- get_root()
  res_endpoint <- endpoint$run()
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)

  router <- build_routes()
  res_api <- router$request("GET", "/")
  expect_equal(res_api$status, 200)
  expect_equal(res_api$body, res_endpoint$body)
})

test_that("GET /version", {
  res <- target_get_version()
  expect_equal(res, jsonlite::unbox(as.character(packageVersion("serovizr"))))

  router <- build_routes()
  res_api <- router$request("GET", "/version")
  expect_equal(res_api$status, 200)
  body <- jsonlite::fromJSON(res_api$body)
  expect_equal(unclass(res), unclass(body$data))
})

test_that("POST /dataset/", {
  router <- build_routes()
  request <- local_POST_dataset_request(data.frame(biomarker = "ab", value = 1, day = 1:10),
                                             "testdataset")
  res_upload <- router$call(request)
  expect_equal(res_upload$status, 200)
  body <- jsonlite::fromJSON(res_upload$body)
  expect_equal(body$data, "testdataset")
})

test_that("GET /datasets/", {
  local_add_dataset(data.frame(biomarker = "ab", value = 1, day = 1:10),
                    "testdataset")
  local_add_dataset(data.frame(biomarker = "ab", value = 1, day = 1:10),
                    "anotherdataset")
  router <- build_routes()
  res <- router$request("GET", "/datasets/")
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data, c("anotherdataset", "testdataset"))
})
