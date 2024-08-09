build_routes <- function() {
  pr <- porcelain::porcelain$new(validate=TRUE)
  pr$handle(get_root())
  pr$handle(get_version())
}

get_root <- function() {
  porcelain::porcelain_endpoint$new("GET",
                                    "/",
                                    target_get_root,
                                    returning = porcelain::porcelain_returning_json())
}

get_version <- function() {
  porcelain::porcelain_endpoint$new("GET",
                                    "/version",
                                    target_get_version,
                                    returning = porcelain::porcelain_returning_json("Version"))
}

post_dataset <- function() {
  porcelain::porcelain_endpoint$new(
    "POST", "/dataset/",
    target_post_dataset(),
    porcelain::porcelain_input_body_binary("data", "application/csv"),
    returning = porcelain::porcelain_returning_json("Data"))
}
