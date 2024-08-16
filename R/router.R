build_routes <- function() {
  pr <- porcelain::porcelain$new(validate = TRUE)
  pr$registerHook(stage = "preserialize", function(data, req, res, value) {
    res$setHeader("Access-Control-Allow-Origin", "http://localhost:3000")
    value
  })

  pr$handle(get_root())
  pr$handle(get_version())
  pr$handle("POST", "/dataset/",
            function(req, res) target_post_dataset(req, res),
            serializer = plumber::serializer_unboxed_json())
  pr$handle(get_dataset())
  pr$handle(get_datasets())
  pr$handle(get_trace())
}


get_root <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/",
    target_get_root,
    returning = porcelain::porcelain_returning_json())
}

get_version <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/version",
    target_get_version,
    returning = porcelain::porcelain_returning_json("Version"))
}

get_dataset <- function() {
  porcelain::porcelain_endpoint$new(
    "GET", "/dataset/<name>",
    target_get_dataset,
    returning = porcelain::porcelain_returning_json("DatasetMetadata"))
}

get_datasets <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/datasets/",
    target_get_datasets,
    returning = porcelain::porcelain_returning_json("DatasetNames"))
}

get_trace <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/dataset/<name>/<biomarker>/",
    target_get_trace,
    porcelain::porcelain_input_query(facet = "string", trace = "string"),
    returning = porcelain::porcelain_returning_json("DataSeries"))
}
