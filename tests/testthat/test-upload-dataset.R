test_that("returns error", {
  res <- jsonlite::fromJSON(target_post_dataset())
  expect_equal(res, as.character(packageVersion("serovizr")))

  router <- build_routes()
  res_api <- router$request("POST", "/dataset/")
  expect_equal(res_api$status, 200)
  body <- jsonlite::fromJSON(res_api$body)
  expect_equal(res, body$data)
})
