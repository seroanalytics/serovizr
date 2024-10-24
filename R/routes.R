get_root <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/api/",
    target_get_root,
    returning = porcelain::porcelain_returning_json())
}

get_version <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/api/version/",
    target_get_version,
    returning = porcelain::porcelain_returning_json("Version"))
}

get_dataset <- function() {
  porcelain::porcelain_endpoint$new(
    "GET", "/api/dataset/<name>/",
    target_get_dataset,
    returning = porcelain::porcelain_returning_json("DatasetMetadata"))
}

delete_dataset <- function() {
  porcelain::porcelain_endpoint$new(
    "DELETE", "/api/dataset/<name>/",
    target_delete_dataset,
    returning = porcelain::porcelain_returning_json())
}

options_dataset <- function() {
  porcelain::porcelain_endpoint$new(
    "OPTIONS", "/api/dataset/<name>/",
    function(name) "OK",
    returning = porcelain::porcelain_returning_json())
}

get_datasets <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/api/datasets/",
    target_get_datasets,
    returning = porcelain::porcelain_returning_json("DatasetNames"))
}

get_trace <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/api/dataset/<name>/trace/<biomarker>/",
    target_get_trace,
    porcelain::porcelain_input_query(disaggregate = "string",
                                     filter = "string",
                                     scale = "string",
                                     method = "string",
                                     span = "numeric",
                                     k = "numeric"),
    returning = porcelain::porcelain_returning_json("DataSeries"))
}

get_individual <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/api/dataset/<name>/individual/<pidcol>/",
    target_get_individual,
    porcelain::porcelain_input_query(scale = "string",
                                     color = "string",
                                     filter = "string",
                                     linetype = "string",
                                     page = "numeric"),
    returning = porcelain::porcelain_returning_json("Plotly"))
}

options_session <- function() {
  porcelain::porcelain_endpoint$new(
    "OPTIONS", "/api/session/",
    function() "OK",
    returning = porcelain::porcelain_returning_json())
}

delete_session <- function() {
  porcelain::porcelain_endpoint$new(
    "DELETE", "/api/session/",
    target_delete_session,
    returning = porcelain::porcelain_returning_json())
}
