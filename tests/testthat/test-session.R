test_that("new session id is set if not present on POST /dataset", {
  key <- plumber::random_cookie_key()
  router <- build_routes(key)
  set.seed(1)
  request <- local_POST_dataset_request(data.frame(biomarker = "ab",
                                                   time = 1:10,
                                                   value = 1),
                                        "testdataset",
                                        xcol = "time")
  res <- router$call(request)
  expect_equal(res$status, 200)
  cookie <- res$headers[["Set-Cookie"]]
  session <- plumber:::decodeCookie(plumber:::parseCookies(cookie)[[1]],
                                    plumber:::asCookieKey(key))
  expect_equal(session$id, session_id)
  expect_true(fs::file_exists(file.path("uploads", session_id, "testdataset")))
})

test_that("existing session id is used if present on POST /dataset", {
  key <- plumber::random_cookie_key()
  router <- build_routes(key)

  session <- list(id = "1234")

  cookie <- plumber:::cookieToStr("serovizr",
                                  plumber:::encodeCookie(session,
                                                         plumber:::asCookieKey(key)))
  request <- local_POST_dataset_request(data.frame(biomarker = "ab",
                                                   time = 1:10,
                                                   value = 1),
                                        "testdataset",
                                        xcol = "time",
                                        session = "1234",
                                        cookie = cookie)
  res <- router$call(request)
  expect_equal(res$status, 200)
  cookie <- res$headers[["Set-Cookie"]]
  session <- plumber:::decodeCookie(plumber:::parseCookies(cookie)[[1]],
                                    plumber:::asCookieKey(key))
  expect_equal(session$id, "1234")
  expect_true(fs::file_exists("uploads/1234/testdataset"))
})

test_that("existing session id is used if present on GET /dataset", {
  key <- plumber::random_cookie_key()
  router <- build_routes(key)
  session <- list(id = "1234")
  cookie <- plumber:::cookieToStr("serovizr",
                                  plumber:::encodeCookie(session,
                                                         plumber:::asCookieKey(key)))
  request <- local_POST_dataset_request(data.frame(biomarker = "ab",
                                                   time = 1:10,
                                                   value = 1),
                                        "testdataset",
                                        xcol = "time",
                                        session = "1234",
                                        cookie = cookie)
  res <- router$call(request)
  expect_equal(res$status, 200)

  get_request_without_cookie <- make_req("GET",
                          "/dataset/testdataset/trace/ab/")

  res <- router$call(get_request_without_cookie)
  expect_equal(res$status, 401)

  get_request_with_cookie <- make_req("GET",
                          "/dataset/testdataset/trace/ab/",
                          HTTP_COOKIE = cookie)

  res <- router$call(get_request_with_cookie)
  expect_equal(res$status, 200)
})
