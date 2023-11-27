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
    purrr::map(~purrr::chuck(.x, "data")) %>%
    # flatten list
    purrr::map(~purrr::list_flatten(.x)) %>%
    # create a table per record
    purrr::map_dfr(~ purrr::discard(.x, function(x) all(x == ""))) %>%
    # drop value fields (paths to local images)
    dplyr::select(-dplyr::ends_with("_value")) %>%
    # drop url suffix, now no longer neccesairy, breaks renaming later
    dplyr::rename_with(.fn = ~stringr::str_remove(.x, "_url"),
                       .cols = dplyr::ends_with("_url"))

  # parse the API response so it's usable in analysis

  ## Parse the list columns out to character columns
  ### Check that none of the list columns have elements with length > 1
  records %>%
    dplyr::select(dplyr::where(is.list)) %>%
    # calculate the length of every element
    dplyr::mutate(dplyr::across(everything(),
                                .names = "{col}_len",
                                .fns = ~purrr::map_int(.x,\(x) length(x)))) %>%

    dplyr::select(dplyr::ends_with("_len")) %>%
    # check if any list column has elements with a length >= 1
    dplyr::summarise_all(~max(.x) <= 1) %>%
    purrr::map_lgl(~.x) %>%
    all() %>%
    assertthat::assert_that(
      msg = glue::glue("The API returned multivalue columns of type select, ",
                       "these are currently not supported: ",
                       "Please create an issue on Github!")
    )
  ### convert the list columns into character columns using purrr magic
  records_no_lists <-
    records %>%
    dplyr::mutate(
      dplyr::across(dplyr::where(is.list),
        .names = "{.col}",
        ~ purrr::map_chr(.x, function(element) {
          purrr::pluck(
            element,
            1,
            .default = NA
          )
        })
      )
    )

  ## Select fields to be recoded based on their fieldtype
  fields_type_select <-
    inspection_fields$fields %>%
    dplyr::filter(fieldtype == "select") %>%
    dplyr::pull(id) %>%
    unique()

  ## Recode values from id to the value returned in inspection_fields
  records_recoded <-
    records_no_lists %>%
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
  records_renamed <-
    records_recoded %>%
    dplyr::rename_with(~ inspection_fields$fields$fieldlabel[
      match(., inspection_fields$fields$id)
    ]) %>%
    janitor::clean_names()

  # output a tibble with the requested records
  return(records)
}
