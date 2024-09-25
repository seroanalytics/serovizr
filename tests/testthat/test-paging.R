test_that("page 1 is first n ids if n < length(ids)", {
  ids <- c("a", "b", "c", "d", "e")
  result <- get_paged_ids(ids, 1, 2)
  expect_equal(result, c("a", "b"))
})

test_that("page 1 is all ids if n >= length(ids)", {
  ids <- c("a", "b", "c", "d", "e")
  result <- get_paged_ids(ids, 1, 10)
  expect_equal(result, ids)
})

test_that("page 2 is second n ids", {
  ids <- c("a", "b", "c", "d", "e")
  result <- get_paged_ids(ids, 2, 2)
  expect_equal(result, c("c", "d"))
})

test_that("last page is last m ids where m <= n", {
  ids <- c("a", "b", "c", "d", "e")
  result <- get_paged_ids(ids, 3, 2)
  expect_equal(result, c("e"))
})

test_that("first page of results returned by default", {
  dat <- data.frame(biomarker = "ab",
                    pid = 1:25,
                    value = 1,
                    day = 1:25)
  router <- build_routes(cookie_key)
  post_request <- local_POST_dataset_request(dat,
                                             "testdataset",
                                             cookie = cookie)
  expect_equal(router$call(post_request)$status, 200)
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/individual/pid/",
                              HTTP_COOKIE = cookie))

  expect_equal(res$status, 200)

  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data$page, 1)
  expect_equal(body$data$numPages, 2)

  data <- body$data$data
  expect_equal(nrow(data), 20)
})

test_that("correct page of results returned", {
  dat <- data.frame(biomarker = "ab",
                    pid = 1:25,
                    value = 1,
                    day = 1:25)
  router <- build_routes(cookie_key)
  post_request <- local_POST_dataset_request(dat,
                                             "testdataset",
                                             cookie = cookie)
  expect_equal(router$call(post_request)$status, 200)
  res <- router$call(make_req("GET",
                              "/dataset/testdataset/individual/pid/",
                              qs = "page=2",
                              HTTP_COOKIE = cookie))

  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data$page, 2)
  expect_equal(body$data$numPages, 2)

  data <- body$data$data
  expect_equal(nrow(data), 5)
})
