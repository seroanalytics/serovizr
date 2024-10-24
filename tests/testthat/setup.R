# Run before any test
set.seed(1)
dir.create("uploads")
schema_root <- file.path(system.file("schema", package = "serovizr"))

# test fixtures
session_id <- generate_session_id()
cookie_key <- plumber::random_cookie_key()
session <- list(id = session_id)
encoded_cookie_val <- plumber:::encodeCookie(session,
                                             plumber:::asCookieKey(cookie_key))
cookie <- plumber:::cookieToStr("serovizr", encoded_cookie_val)

# Run after all tests
#withr::defer(fs::dir_delete("uploads"), teardown_env())
