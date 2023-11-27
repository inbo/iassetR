recode_by_field <- function(field,
                            field_name,
                            inspection_fields = get_fields(
                              access_token = get_access_token(),
                              name = NULL # we expect inspection_fields to always be provided
                            )) {
  dplyr::recode(field,
                !!!dplyr::filter(inspection_fields$fields,
                                 id == field_name)$options)
}
