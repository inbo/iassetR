#' Title
#'
#' @param inspection_name name of the custom inspection to return records for
#' @param access_token access token from `get_access_token()`
#'
#' @return
#' @export
#'
#' @examples
get_records <- function(inspection_name = "Vespa-Watch",
                        access_token = get_access_token(quiet = TRUE)) {
  # check input params
  assertthat::assert_that(assertthat::is.string(access_token))
  assertthat::assert_that(assertthat::is.string(inspection_name))
  # get the inspection_id for the custom inspection
  inspection_fields <- get_fields(
    access_token = access_token,
    name = inspection_name
  )
  # build a request and perform it
  get_records_request <-
    httr2::request("https://api.iasset.nl/getCustomInspections") %>%
    httr2::req_body_form(
      access_token = access_token,
      "inspection_ids[0]" = inspection_fields$id,
      version = "9.7"
    ) %>%
    # retry 3 times if it fails, try for 60 seconds per attempt
    httr2::req_retry(
      max_tries = 3,
      max_seconds = 60
    )
  # perform the request
  records_response <- httr2::req_perform(get_records_request)

  # parse the returned JSON
  records <- httr2::resp_body_json(records_response, check_type = FALSE)
records %>%
  purrr::chuck("returndata") %>%
  # get the data object for every element
  purrr::map(~ purrr::chuck(.x, "data")) %>%
  # create a table per record
  purrr::map_dfr(~ purrr::discard(.x, function(x) all(x == ""))) %>%
  # rename field with values from `get_fields()`
  dplyr::rename_with(~ inspection_fields$fields$fieldlabel[
    match(., inspection_fields$fields$id)
  ])
}
