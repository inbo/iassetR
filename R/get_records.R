#' Title
#'
#' @param inspection_name name of the custom inspection to return records for
#' @param access_token access token from `get_access_token()`
#'
#' @return a tibble with the records from the selected inspection.
#' @export
#'
#' @examples \dontrun{get_records("Vespa-Watch")}
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
  records <-
    httr2::resp_body_json(records_response, check_type = FALSE) %>%
    ## convert into tibble
    purrr::chuck("returndata") %>%
    # get the data object for every element
    purrr::map(~ purrr::chuck(.x, "data")) %>%
    # create a table per record
    purrr::map_dfr(~ purrr::discard(.x, function(x) all(x == "")))

  # parse the API response so it's usable in analysis

  ## Select fields to be recoded based on their fieldtype
  fields_type_select <-
    inspection_fields$fields %>%
    dplyr::filter(fieldtype == "select") %>%
    dplyr::pull(id) %>%
    unique()
  ## Recode values from id to the value returned in inspection_fields
  records %>%
    dplyr::mutate(
      dplyr::across(
        all_of(fields_type_select),
        .names = "{.col}",
        ~recode_by_field(
          .x,
          inspection_fields = inspection_fields
          )
        )
      )
    # rename field with values from `get_fields()`
    dplyr::rename_with(~ inspection_fields$fields$fieldlabel[
      match(., inspection_fields$fields$id)
    ]) %>%
    janitor::clean_names()

  # output a tibble with the requested records
  return(records)
}
