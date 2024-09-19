test_that("GET /dataset<name> returns 404 if no session cookie present", {
  router <- build_routes(cookie_key)
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/"))
  expect_equal(res$status, 404)
  validate_failure_schema(res$body)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$errors[1, "detail"],
               "Did not find dataset with name: testdataset")
})

test_that("GET /dataset<name> returns 404 if dataset not found", {
  router <- build_routes(cookie_key)
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/",
                              HTTP_COOKIE = cookie))
  expect_equal(res$status, 404)
  validate_failure_schema(res$body)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$errors[1, "detail"],
               "Did not find dataset with name: testdataset")
})
