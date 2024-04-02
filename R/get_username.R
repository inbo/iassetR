#' Get the username associated with the iAsset credentials in the system credentials store.
#'
#' @param service Character. The keyring service to fetch the stored username for.
#'
#' @return A character vector of stored usernames for the service. NA if none.
#'
#' @export
#'
#' @examples
#' \dontrun{get_username("iasset_password")}
get_username <- function(service = "iasset_password") {
  # check input params
  assertthat::assert_that(assertthat::is.string(service))

  # check that the system supports keyring
  assertthat::assert_that(keyring::has_keyring_support())

  # check that the queried service is in the keyring
  assertthat::assert_that(service %in% keyring::key_list()$service,
    msg = glue::glue("{service} not found in keyring")
  )

  # fetch the associated usernames
  keyring::key_list(service = service)$username
}
