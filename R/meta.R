#' TODO
#' 
#' TODO
#' @param ... TODO
#' @returns TODO
#' @examples
#' foosball3_meta_data()
#' @export
foosball3_meta_data <- function(...) {
  sql_text <- system.file(
    "creation_script.sql",
    package = "foosball3") |>
    readLines()
  dplyr::tibble(
    is_view =
      ifelse(grepl("CREATE VIEW ", sql_text), TRUE, NA),
    table =
      stringr::str_extract_all(
        sql_text,
        "(?i)(?<=CREATE TABLE )\\s*(?:IF NOT EXISTS\\s+)?(?:[\"\'`]?\\w+[\"\'`]?\\.)?[\"\'`]?\\w+[\"\'`]?"
      ) |>
      lapply(\(x) if (length(x) == 0) NA_character_ else x) |>
      unlist(),
    field_name =
      stringr::str_extract_all(sql_text, "(?m)^\\s*[\"\'`]?(\\w+)[\"\'`]?") |>
      lapply(\(x) if (length(x) == 0) NA_character_ else x) |>
      lapply(\(x) if (length(x) == 0) NA_character_ else trimws(x)) |>
      unlist(),
    description =
      stringr::str_extract_all(sql_text, "(?<=---)(?s).*") |>
      lapply(\(x) if (length(x) == 0) NA_character_ else trimws(x)) |>
      unlist()
  ) |>
    tidyr::fill(.data$is_view) |>
    dplyr::filter(
      is.na(.data$is_view) &
        !.data$field_name %in% c("PRAGMA", "FOREIGN", "CONSTRAINT", 
                                 "UNIQUE") & !is.na(.data$field_name)) |>
    tidyr::fill(.data$table) |>
    dplyr::select(-"is_view")
}