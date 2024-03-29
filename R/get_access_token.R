#' Login to iAsset and retreive access token
#'
#' @param username the iAsset username
#' @param quiet Should the function return verbose messages
#'
#' @return Invisibily, an access token upon succes
#' @export
#'
#' @details
#' This function uses keyring to retrieve the password. If no password has been
#' set using keyring you'll be prompted to enter your password using askpass.
#' Setting the password using keyring should best be done interactively
#' (in the console) using `keyring::key_set("iasset_password")`.
#' Keyring uses secret environment variables on GitHub Actions.
#'
#' @import keyring
#' @examples \dontrun{get_access_token("my_username")}
get_access_token <-
  function(username, quiet = FALSE) {
    # check input params
    assertthat::assert_that(assertthat::is.string(username))
    assertthat::assert_that(assertthat::is.flag(quiet))
    # build a request and perform it
    login_request <-
      httr2::request(base_url = "https://api.iasset.nl/login/")

    login_response <- login_request %>%
      httr2::req_body_form(
        username = username,
        password = openssl::md5(keyring::key_get("iasset_password")),
        domain = "riparias",
        version = "9.7"
      ) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json(check_type = FALSE)
    # print success
    if (!quiet) {
      message(login_response$returndata[[1]]$success_msg)
    }
    # return access token
    invisible(purrr::chuck(login_response, "returndata", 1, "access_token"))
  }
