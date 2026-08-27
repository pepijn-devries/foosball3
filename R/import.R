#' TODO
#' 
#' TODO
#' @param file TODO
#' @param target TODO
#' @param ... TODO
#' @returns TODO
#' @examples
#' # TODO
#' 
#' @export
foosball3_import_db <- function(file, target, ...) {
  foosball3_create_db(target)
  warning_messages <- character()
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
  if (length(unknown) > 0) {
    warning_messages <- c(warning_messages,
                          sprintf("Skipping unknown tables: '%s'.",
                                  paste(unknown, collapse = ", ")))
  }
  tb_imp <- intersect(tb_nms, tb_known)
  for (tb in tb_imp) {
    col_imp   <- dplyr::tbl(con_imp, tb) |> colnames()
    col_known <- dplyr::tbl(con_new, tb) |> colnames()
    dat <-
      dplyr::tbl(con_imp, tb) |>
      dplyr::collect()
    if (any(duplicated(dat))) {
      warning_messages <- c(
        warning_messages,
        sprintf("Removed duplicated records from '%s'.", tb))
    }
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
          warning_messages <<- c(
            warning_messages,
            sprintf("Missing field ('%s') was added with default values ('%s') in '%s'.",
                    cl, as.character(default), tb))
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
  if (length(warning_messages) > 0)
    warning(paste(warning_messages, collapse = " "))
  invisible()
}

.as_sqlite <- function(object, Class, ...) {
  if ("blob" %in% Class) {
    object <- blob::as_blob(object)
  } else {
    if (is.character(object) && !all(is.na(object)))
      object[!is.na(object) & object == ""] <- NA_character_
    object <- methods::as(object, Class) |>
      suppressWarnings() # TODO should only suppress 'introduced NAs'
  }
  object
}