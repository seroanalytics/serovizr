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
    "/version/",
    target_get_version,
    returning = porcelain::porcelain_returning_json("Version"))
}

get_dataset <- function() {
  porcelain::porcelain_endpoint$new(
    "GET", "/dataset/<name>/",
    target_get_dataset,
    porcelain::porcelain_input_query(public = "logical"),
    returning = porcelain::porcelain_returning_json("DatasetMetadata"))
}

delete_dataset <- function() {
  porcelain::porcelain_endpoint$new(
    "DELETE", "/dataset/<name>/",
    target_delete_dataset,
    returning = porcelain::porcelain_returning_json())
}

options_dataset <- function() {
  porcelain::porcelain_endpoint$new(
    "OPTIONS", "/dataset/<name>/",
    function(name) "OK",
    returning = porcelain::porcelain_returning_json())
}

get_datasets <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/datasets/",
    target_get_datasets,
    returning = porcelain::porcelain_returning_json("DatasetNames"))
}

get_public_datasets <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/public/datasets/",
    target_get_public_datasets,
    returning = porcelain::porcelain_returning_json("PublicDatasets"))
}

get_trace <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/dataset/<name>/trace/<biomarker>/",
    target_get_trace,
    porcelain::porcelain_input_query(disaggregate = "string",
                                     filter = "string",
                                     scale = "string",
                                     method = "string",
                                     span = "numeric",
                                     k = "numeric",
                                     public = "string"),
    returning = porcelain::porcelain_returning_json("DataSeries"))
}

get_individual <- function() {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/dataset/<name>/individual/<pidcol>/",
    target_get_individual,
    porcelain::porcelain_input_query(scale = "string",
                                     color = "string",
                                     filter = "string",
                                     linetype = "string",
                                     page = "numeric",
                                     public = "string"),
    returning = porcelain::porcelain_returning_json("Plotly"))
}

options_session <- function() {
  porcelain::porcelain_endpoint$new(
    "OPTIONS", "/session/",
    function() "OK",
    returning = porcelain::porcelain_returning_json())
}

delete_session <- function() {
  porcelain::porcelain_endpoint$new(
    "DELETE", "/session/",
    target_delete_session,
    returning = porcelain::porcelain_returning_json())
}
