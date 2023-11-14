#' Helper to check if a Request was successful
#'
#' @param parsed_response
#'
#' @return `TRUE` for a successful request, otherwise, an error.
#' @family helper functions
#' @noRd
check_request_success <- function(parsed_response) {
  # If the request is successful, the response will have `error$FALSE`otherwise,
  # it'll contain a message
  assertthat::assert_that(isFALSE(purrr::pluck(parsed_response, "error")),
    msg = paste0(
      "Something went wrong while accessing the API:\n",
      purrr::pluck(parsed_response, "error", "error_msg")
    )
  )
  # This will only get evaluated if the above assertion passes.
  return(TRUE)
}

#' Helper to assert that the access_token has the right shape
#'
#' @param access_token
#'
#' @return `TRUE` for a string in the shape of an access token,
#' otherwise an error
#' @family helper functions
#' @noRd
is_access_token <- function(access_token) {
  assertthat::assert_that(assertthat::is.string(access_token))
  assertthat::assert_that(nchar(access_token) == 120)
  assertthat::assert_that(grepl("^[-A-Za-z0-9+\\/]*={0,3}$", access_token))
}
