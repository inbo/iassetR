# Helper functions go in here

#' Helper function to replace field id's in the API output with their labels
#'
#' To be used in for example `dplyr::rename_with()`
#'
#' @param field_id The field id as returned by the iAsset API
#' @param inspection_fields The result of `get_fields()`
#'
#' @return The field label as used in the iAsset interface
#'
#' @examples
lookup_field_name <- function(field_id, inspection_fields) {
  purrr::chuck("fields") %>%
  dplyr::filter(id == .data$field_id) %>%
    dplyr::pull("fieldlabel") %>%
    return()
}
