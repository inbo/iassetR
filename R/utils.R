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
