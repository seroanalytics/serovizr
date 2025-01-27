test_that("GET /", {
  mock_req <- as.environment(list(id = 1234))
  res <- target_get_root(mock_req)
  expect_equal(res, jsonlite::unbox("Welcome to serovizr"))

  endpoint <- get_root()
  res_endpoint <- endpoint$run(mock_req)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)

  router <- build_routes()
  res_api <- router$request("GET", "/")
  expect_equal(res_api$status, 200)
  expect_equal(res_api$body, res_endpoint$body)
})

test_that("GET /version", {
  res <- target_get_version(as.environment(list(id = 1234)))
  expect_equal(res, jsonlite::unbox(as.character(packageVersion("serovizr"))))

  router <- build_routes()
  res_api <- router$request("GET", "/version/")
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

test_that("DELETE /dataset", {
  router <- build_routes(cookie_key)
  local_add_dataset(data.frame(biomarker = "ab", value = 1, day = 1:10),
                    "testdataset")
  res <- router$call(make_req("DELETE",
                              "/dataset/testdataset/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data, "testdataset")
})

test_that("DELETE /dataset returns 200 if dataset doesn't exist", {
  router <- build_routes(cookie_key)
  res <- router$call(make_req("DELETE",
                              "/dataset/testdataset/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data, "testdataset")
})

test_that("GET /datasets", {
  local_add_dataset(data.frame(biomarker = "ab", value = 1, day = 1:10),
                    "testdataset")
  local_add_dataset(data.frame(biomarker = "ab", value = 1, day = 1:10),
                    "anotherdataset")
  router <- build_routes(cookie_key)
  res <- router$call(make_req("GET",
                              "/datasets/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data, c("anotherdataset", "testdataset"))
})

test_that("GET /public/datasets", {
  router <- build_routes(cookie_key)
  res <- router$call(make_req("GET",
                              "/public/datasets/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data$name, "test")
  expect_equal(body$data$description, "test description")
})

test_that("GET /dataset<name>", {
  local_add_dataset(data.frame(biomarker = c("ab", "ba"),
                               value = 1,
                               day = 1:10,
                               age = "0-5",
                               sex = c("M", "F")),
                    "testdataset")
  router <- build_routes(cookie_key)
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data$variables$name, c("age", "sex"))
  expect_equal(body$data$variables$levels, list(c("0-5"), c("M", "F")))
  expect_equal(body$data$biomarkers, c("ab", "ba"))
  expect_equal(body$data$xcol, "day")
})

test_that("GET /dataset/<name>/trace/<biomarker>", {
  dat <- data.frame(biomarker = c("ab", "ba"),
                    value = c(1, 1.5, 2, 3, 3.2, 4, 5, 6, 6.1, 7),
                    day = 1:10,
                    age = "0-5",
                    sex = c("M", "F"))
  local_add_dataset(dat,
                    "testdataset")
  suppressWarnings({
    m <- stats::loess(value ~ day, data = dat[dat["biomarker"] == "ab",], span = 0.75)
    model <- list(x = 1:9, y = stats::predict(m, tibble::data_frame(day = 1:9)))
  })
  router <- build_routes(cookie_key)
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
                                         model = model,
                                         raw = list(
                                           x = c(1, 3, 5, 7, 9),
                                           y = c(1, 2, 3.2, 5, 6.1)),
                                         warnings = lapply(expected_warnings,
                                                           jsonlite::unbox))
  ))
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data, jsonlite::fromJSON(expected))
})

test_that("requests without trailing slash are redirected", {
  router <- build_routes()
  res_api <- router$request("GET", "/version")
  expect_equal(res_api$status, 307)
})

test_that("DELETE /session", {
  router <- build_routes(cookie_key)
  local_add_dataset(data.frame(biomarker = c("ab", "ba"),
                               value = 1,
                               day = 1:10,
                               age = "0-5",
                               sex = c("M", "F")),
                    "testdataset")
  res <- router$call(make_req("GET",
                              "/datasets/",
                              HTTP_COOKIE = cookie))
  # expect the session cookie to be set and upload dir created
  expect_true(grepl("serovizr=[a-zA-Z0-9_%]+;", res$headers[["Set-Cookie"]]))
  expect_true(fs::dir_exists(file.path("uploads", session_id)))

  res <- router$call(make_req("DELETE",
                              "/session/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 200)

  # expect the session cookie to be unset and upload dir deleted
  expect_true(grepl("serovizr=;", res$headers[["Set-Cookie"]]))
  expect_false(fs::dir_exists(file.path("uploads", session_id)))
})
