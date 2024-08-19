# Run before any test
dir.create("uploads")

# Run after all tests
withr::defer(fs::dir_delete("uploads"), teardown_env())
