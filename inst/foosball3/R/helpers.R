strip_ansi <- function(x) gsub("(\\x9B|\\x1B\\[)[0-?]*[ -/]*[@-~]",
                               "", x, perl = TRUE)

get_description_field <- function(primary_key, field_names) {
  ## TODO check if this description field is correctly found for all editable tables
  object <- stringr::str_replace_all(primary_key, "_ID$|_CODE", "")
  field_names[
    grepl(
      sprintf("(?=.*(NAME|DESCR))^%s", object),
      field_names, perl = TRUE
    ) & field_names != primary_key
  ][1]
}
