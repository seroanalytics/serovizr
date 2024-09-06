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

test_that("new session id is set if not present on GET /dataset", {
  key <- plumber::random_cookie_key()
  router <- build_routes(key)
  get_request_without_cookie <- make_req("GET",
                                         "/dataset/testdataset/")
  res <- router$call(get_request_without_cookie)
  cookie <- res$headers[["Set-Cookie"]]
  session <- plumber:::decodeCookie(plumber:::parseCookies(cookie)[[1]],
                                    plumber:::asCookieKey(key))
  expect_true(is.character(session$id))
  expect_equal(nchar(session$id), 10)
})

test_that("new session id is set if not present on GET /", {
  key <- plumber::random_cookie_key()
  router <- build_routes(key)
  get_request_without_cookie <- make_req("GET",
                                         "/")
  res <- router$call(get_request_without_cookie)
  cookie <- res$headers[["Set-Cookie"]]
  session <- plumber:::decodeCookie(plumber:::parseCookies(cookie)[[1]],
                                    plumber:::asCookieKey(key))
  expect_true(is.character(session$id))
  expect_equal(nchar(session$id), 10)
})

test_that("new session id is set if not present on GET /version/", {
  key <- plumber::random_cookie_key()
  router <- build_routes(key)
  get_request_without_cookie <- make_req("GET",
                                         "/version/")
  res <- router$call(get_request_without_cookie)
  cookie <- res$headers[["Set-Cookie"]]
  session <- plumber:::decodeCookie(plumber:::parseCookies(cookie)[[1]],
                                    plumber:::asCookieKey(key))
  expect_true(is.character(session$id))
  expect_equal(nchar(session$id), 10)
})

test_that("new session id is set if not present on GET /dataset/trace/", {
  key <- plumber::random_cookie_key()
  router <- build_routes(key)
  set.seed(1)
  get_request_without_cookie <- make_req("GET",
                                         "/dataset/testdataset/trace/ab/")
  res <- router$call(get_request_without_cookie)
  cookie <- res$headers[["Set-Cookie"]]
  session <- plumber:::decodeCookie(plumber:::parseCookies(cookie)[[1]],
                                    plumber:::asCookieKey(key))
  expect_equal(session$id, session_id)
})

test_that("existing session id is used if present on GET /dataset/trace/", {
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
  expect_equal(res$status, 404)

  get_request_with_cookie <- make_req("GET",
                                      "/dataset/testdataset/trace/ab/",
                                      HTTP_COOKIE = cookie)

  res <- router$call(get_request_with_cookie)
  expect_equal(res$status, 200)
})

test_that("cache is updated on each request", {
  key <- plumber::random_cookie_key()
  cache <- cachem::cache_mem(max_age = 1)
  router <- build_routes(key, cache)
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
  expect_equal(cache$get("1234"), TRUE)

  Sys.sleep(1)

  # expect session to have expired from cache
  expect_equal(length(cache$keys()), 0)

  get_request_with_cookie <- make_req("GET",
                                      "/dataset/testdataset/trace/ab/",
                                      HTTP_COOKIE = cookie)

  res <- router$call(get_request_with_cookie)
  expect_equal(res$status, 200)

  # expect session to have been re-added to cache
  expect_equal(cache$get("1234"), TRUE)
})


test_that("inactive uploads are purged", {
  key <- plumber::random_cookie_key()
  cache <- cachem::cache_mem(max_age = 1)
  router <- build_routes(key, cache)
  old_session <- list(id = "1234")
  old_cookie <- plumber:::cookieToStr("serovizr",
                                      plumber:::encodeCookie(old_session,
                                                             plumber:::asCookieKey(key)))
  request <- local_POST_dataset_request(data.frame(biomarker = "ab",
                                                   time = 1:10,
                                                   value = 1),
                                        "testdataset",
                                        xcol = "time",
                                        session = "1234",
                                        cookie = old_cookie)
  expect_true(dir.exists("uploads/1234"))

  Sys.sleep(1)

  new_session <- list(id = "5678")
  new_cookie <- plumber:::cookieToStr("serovizr",
                                      plumber:::encodeCookie(new_session,
                                                             plumber:::asCookieKey(key)))
  request <- local_POST_dataset_request(data.frame(biomarker = "ab",
                                                   time = 1:10,
                                                   value = 1),
                                        "testdataset",
                                        xcol = "time",
                                        session = "1234",
                                        cookie = new_cookie)
  res <- router$call(request)
  expect_equal(res$status, 200)

  Sys.sleep(1)

  # expect both sessions to have expired from cache
  expect_equal(length(cache$keys()), 0)

  get_request_with_new_cookie <- make_req("GET",
                                      "/dataset/testdataset/trace/ab/",
                                      HTTP_COOKIE = new_cookie)

  res <- router$call(get_request_with_new_cookie)
  expect_equal(res$status, 200)

  get_request_with_old_cookie <- make_req("GET",
                                          "/dataset/testdataset/trace/ab/",
                                          HTTP_COOKIE = old_cookie)

  res <- router$call(get_request_with_old_cookie)
  expect_equal(res$status, 404)
})
