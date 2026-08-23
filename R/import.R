#' TODO
#' 
#' TODO
#' @param file TODO
#' @param target TODO
#' @returns TODO
#' @examples
#' # TODO
#' 
#' @export
foosball3_import_db <- function(file, target, ...) {
  foosball3_create_db(target)
  con_new <- RSQLite::dbConnect(RSQLite::SQLite(), target)
  on.exit({RSQLite::dbDisconnect(con_new)}, add = TRUE)
  con_imp <- RSQLite::dbConnect(RSQLite::SQLite(), file)
  on.exit({RSQLite::dbDisconnect(con_imp)}, add = TRUE)
  sql_code <-
    "SELECT name 
     FROM sqlite_schema 
     WHERE type = 'table' 
     AND name NOT LIKE 'sqlite_%';"
  tb_nms <- RSQLite::dbGetQuery(con_imp, sql_code) |>
    dplyr::pull("name")
  tb_known <- RSQLite::dbGetQuery(con_new, sql_code) |>
    dplyr::pull("name")
  unknown <- setdiff(tb_nms, tb_known)
  if (length(unknown) > 0)
    warning("Skipping unknown tables")
  tb_imp <- intersect(tb_nms, tb_known)
  for (tb in tb_imp) {
    col_imp   <- dplyr::tbl(con_imp, tb) |> colnames()
    col_known <- dplyr::tbl(con_new, tb) |> colnames()
    dat <-
      dplyr::tbl(con_imp, tb) |>
      dplyr::collect()
    if (any(duplicated(dat))) "TODO" #add warning about duplicates
    dat <- dplyr::distinct(dat)
    dat_expected <-
      dplyr::tbl(con_new, tb) |>
      dplyr::collect()
    new_tb <-
      col_known |>
      lapply(\(cl) {
        d <- dat[[cl]]
        if (is.null(d)) {
          default <-
            RSQLite::dbGetQuery(
              con_new, sprintf(
                "SELECT dflt_value FROM pragma_table_info('%s') WHERE name = '%s';",
                tb, cl))[["dflt_value"]]
          if (is.character(default)) {
            default <- stringr::str_replace_all(default, "^'|'$", "")
          }
          # TODO warn that missing values were replaced with default!
          d <- .as_sqlite(rep(default, nrow(dat)),
                          class(dat_expected[[cl]]))
        }
        d <- .as_sqlite(d, class(dat_expected[[cl]]))
        d
      }) |>
      stats::setNames(col_known)
    new_tb <- dplyr::as_tibble(new_tb)

    RSQLite::dbExecute(con_new, sprintf("DELETE FROM %s;", tb))
    dplyr::copy_to(con_new, new_tb, tb, append = TRUE)

  }
}

.as_sqlite <- function(object, Class, ...) {
  if ("blob" %in% Class) {
    object <- blob::as_blob(object)
  } else {
    if (is.character(object) && !all(is.na(object)) &&
        any(object == ""))
      object[object == ""] <- NA_character_
    object <- as(object, Class) |>
      suppressWarnings() # TODO should only suppress 'introduced NAs'
  }
  object
}