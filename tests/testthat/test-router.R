test_that("root endpoint", {
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

test_that("version endpoint", {
  res <- jsonlite::fromJSON(target_get_version())
  expect_equal(res, as.character(packageVersion("serovizr")))

  router <- build_routes()
  res_api <- router$request("GET", "/version")
  expect_equal(res_api$status, 200)
  body <- jsonlite::fromJSON(res_api$body)
  expect_equal(res, body$data)
})
