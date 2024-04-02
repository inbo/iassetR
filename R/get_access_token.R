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
#' (in the console) using
#' `keyring::key_set("iasset_password", username = "my_username")`.
#' Keyring uses secret environment variables on GitHub Actions.
#'
#' @import keyring
#' @examples \dontrun{
#' get_access_token("my_username")
#' }
get_access_token <-
  function(username, quiet = FALSE) {
    # check input params
    assertthat::assert_that(assertthat::is.string(username))
    assertthat::assert_that(assertthat::is.flag(quiet))
    # check for keyring support
    assertthat::assert_that(keyring::has_keyring_support())
    # check if a keyring exists
    iasset_keyring_exists <-
      "iasset_password" %in% dplyr::pull(keyring::key_list(), "service")
    ## check if a username is set
    iasset_username_missing <-
      dplyr::filter(
        keyring::key_list(),
        service == "iasset_password"
      ) %>%
      dplyr::pull("username") %>%
      is.na()

    # prompt user for credentials if password or username is missing
    if(!iasset_keyring_exists | iasset_username_missing){
      message(
        paste(
          "iasset credentials are missing, please enter your credentials or",
          "contact your domain admin to aquire some."
        )
      )
      keyring::key_set(
        service = "iasset_password",
        username =
          askpass::askpass(prompt = "Please enter your iasset username: "),
        prompt = "Please enter your iasset password: "
        )
    }

    # check that only one keyring is set
    number_of_keyrings <- nrow(keyring::key_list(service = "iasset_password"))
    assertthat::assert_that(number_of_keyrings <= 1,
      msg = paste(
        "iassetR currently only supports storing one iAsset account at a time.",
        "Delete any other accounts using",
        'keyring::key_delete(service = "iasset_password",',
        'username = "username_to_delete")'
      )
    )

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
