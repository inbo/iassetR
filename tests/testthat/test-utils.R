test_that("check_request_success() returns error on bad access token", {
  bad_request <-
    httr2::request("https://api.iasset.nl/getCustomInspectionFields/") %>%
    httr2::req_body_form(
      access_token = "NOT AN ACCESS TOKEN"
    ) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json(check_type = F)
  expect_error(
    check_request_success(bad_request),
    regexp = "Something went wrong while accessing the API:"
  )
})

test_that("check_request_success() returns TRUE on a correct request", {
  # TODO: caching some responses without any secrets in them
})
