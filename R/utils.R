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
