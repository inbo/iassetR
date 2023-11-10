#' Get information about a custom inspection
#'
#' @param access_token access token from `get_access_token()`
#' @param name name of the custom inspection to return fields for
#'
#' @return a list object with the name, id and fields (data.frame) of the custom
#' inspection
#' @export
#'
#' @examples
get_fields <- function(name = "Vespa-Watch", access_token = get_access_token()) {
  assertthat::assert_that(assertthat::is.string(access_token))
  get_custom_inspection_fields_request <-
    httr2::request("https://api.iasset.nl/getCustomInspectionFields/") %>%
    httr2::req_body_form(
      access_token = access_token
    )
  custom_inspections <-
    httr2::req_perform(get_custom_inspection_fields_request) %>%
    httr2::resp_body_json(check_type = FALSE)

  selected_inspection <-
    custom_inspections$returndata[
      purrr::map_lgl(
        custom_inspections$returndata,
        ~ purrr::pluck(.x, "name") == name
      )
    ]

  list(
    id = purrr::chuck(selected_inspection, 1, "id"),
    name = purrr::chuck(selected_inspection, 1, "name"),
    fields = selected_inspection %>%
      purrr::chuck(1, "fields") %>%
      purrr::map_dfr(~.x)
  ) %>%
    return()
}
