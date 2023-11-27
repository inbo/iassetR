#' Recode based on field and field name
#'
#' To be used in across to recode values based on the `get_fields()` request
#'
#' @param field field to be recoded, tidyselect syntax
#' @param field_name Character. Name of field to be recoded, within across `cur_column()`
#' @param inspection_fields named list, output of `get_fields()`
#'
#' @return
recode_by_field <- function(field,
                            field_name = dplyr::cur_column(),
                            inspection_fields = get_fields(
                              access_token = get_access_token(),
                              name = NULL # we expect inspection_fields to always be provided
                            )) {
  dplyr::recode(field,
                !!!dplyr::filter(inspection_fields$fields,
                                 id == field_name)$options)
}

#' Rename API columns with the label from `get_fields()`
#'
#' During rectangling, a suffix might be added to the column name before
#' renaming. This suffix is to be retained.
#'
#' @param id Column name as returned by the API.
#' @param inspection_fields named list, output of `get_fields()`
#'
#' @return character vector of matching fieldlabel for id + suffix
rename_by_id <- function(id,
                         inspection_fields = get_fields(
                           access_token = get_access_token(),
                           name = NULL # we expect inspection_fields to always be provided
                         )) {

  purrr::map_chr(id, function(id) {
    pure_id <- stringr::str_remove(id, "_[0-9]+")
    suffix <- stringr::str_extract(id, "_[0-9]+") %>%
      stringr::str_replace_na(replacement = "")

    dplyr::filter(inspection_fields$fields, id == pure_id) %>%
      pull(fieldlabel) %>%
      unique() %>%
      paste0(suffix)
  })
}
