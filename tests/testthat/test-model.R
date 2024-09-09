test_that("model is gam if specified", {
  dat <- data.frame(day = 1:100, value = rnorm(100))
  res <- model_out(dat, xcol = "day", method = "gam")

  m <- mgcv::gam(value ~ s(day, bs = "cs"),
                         data = dat, method = "REML")
  xdf <- tibble::tibble(day = 1:100)
  expected <- stats::predict(m, xdf)

  expect_true(all(res$y == expected))
})

test_that("model is loess if specified", {
  dat <- data.frame(day = 1:2000, value = rnorm(2000))
  res <- model_out(dat, xcol = "day", method = "loess")

  m <- stats::loess(value ~ day, data = dat, span = 0.75)
  xdf <- tibble::tibble(day = 1:2000)
  expected <- stats::predict(m, xdf)

  expect_true(all(res$y == expected))
})

test_that("model is loess if not specified and n <= 1000", {
  dat <- data.frame(day = 1:1000, value = rnorm(1000))
  res <- model_out(dat, xcol = "day")

  m <- stats::loess(value ~ day, data = dat, span = 0.75)
  xdf <- tibble::tibble(day = 1:1000)
  expected <- stats::predict(m, xdf)

  expect_true(all(res$y == expected))
})

test_that("model is gam if not specified and n > 1000", {
  dat <- data.frame(day = 1:1001, value = rnorm(1001))
  res <- model_out(dat, xcol = "day")

  m <- mgcv::gam(value ~ s(day, bs = "cs"),
                 data = dat, method = "REML")
  xdf <- tibble::tibble(day = 1:1001)
  expected <- stats::predict(m, xdf)

  expect_true(all(res$y == expected))
})

test_that("model uses gam options", {
  dat <- data.frame(day = 1:1001, value = rnorm(1001))
  res <- model_out(dat, xcol = "day", k = 5)

  m <- mgcv::gam(value ~ s(day, bs = "cs", k = 5),
                 data = dat, method = "REML")
  xdf <- tibble::tibble(day = 1:1001)
  expected <- stats::predict(m, xdf)

  expect_true(all(res$y == expected))
})

test_that("model uses loess options", {
  dat <- data.frame(day = 1:100, value = rnorm(100))
  res <- model_out(dat, xcol = "day", span = 0.5)

  m <- stats::loess(value ~ day, data = dat, span = 0.5)
  xdf <- tibble::tibble(day = 1:100)
  expected <- stats::predict(m, xdf)

  expect_true(all(res$y == expected))
})
