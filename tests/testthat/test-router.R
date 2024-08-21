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

test_that("POST /dataset", {
  router <- build_routes()
  request <- local_POST_dataset_request(data.frame(biomarker = "ab",
                                                   value = 1,
                                                   day = 1:10),
                                        "testdataset")
  res <- router$call(request)
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data, "testdataset")
  porcelain:::porcelain_validator("ResponseSuccess",
                                  root = schema_root,
                                  query = NULL)(res$body)
  porcelain:::porcelain_validator("UploadResult",
                                  root = schema_root,
                                  query = NULL)(
    jsonlite::toJSON(body$data, auto_unbox = TRUE)
  )
})

test_that("GET /datasets", {
  local_add_dataset(data.frame(biomarker = "ab", value = 1, day = 1:10),
                    "testdataset")
  local_add_dataset(data.frame(biomarker = "ab", value = 1, day = 1:10),
                    "anotherdataset")
  router <- build_routes()
  res <- router$request("GET", "/datasets")
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data, c("anotherdataset", "testdataset"))
})

test_that("GET /dataset", {
  local_add_dataset(data.frame(biomarker = c("ab", "ba"),
                               value = 1,
                               day = 1:10,
                               age = "0-5",
                               sex = c("M", "F")),
                    "testdataset")
  router <- build_routes()
  res <- router$request("GET", "/dataset/testdataset")
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  str(body$data$variables)
  expect_equal(body$data$variables$name, c("age", "sex"))
  expect_equal(body$data$variables$levels, list(c("0-5"), c("M", "F")))
  expect_equal(body$data$biomarkers, c("ab", "ba"))
  expect_equal(body$data$xcol, "day")
})
