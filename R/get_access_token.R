#' Login to iAsset and retreive access token
#'
#' @param username the iAsset username
#' @param quiet Should the function return verbose messages
#'
#' @return Invisibily, an access token upon succes
#' @export
#'
#' @examples \dontrun{get_access_token("my_username")}
get_access_token <-
  function(username, quiet = FALSE) {
    # check input params
    assertthat::assert_that(assertthat::is.string(username))
    assertthat::assert_that(assertthat::is.flag(quiet))
    # build a request and perform it
    login_request <-
      httr2::request(base_url = "https://api.iasset.nl/login/")

    pwd <- Sys.getenv("iasset_password") %>%
      openssl::md5()

    if(pwd == openssl::md5("")){
      hash <- askpass::askpass() %>%
        openssl::md5()
    }else{
      hash <- pwd
    }

    login_response <- login_request %>%
      httr2::req_body_form(
        username = username,
        password = hash,
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
