#' Import Foosball Tournament Data from a File
#' 
#' If you have an older file with tournament results, you can try to
#' import those results with this function. It will create a new copy
#' of the database (using [foosball3_create_db()]), making sure it
#' complies with the latest database scheme specification used by this package.
#' @param file TODO
#' @param target Target file path where the clean database will be stored.
#' Existing files at this location may be overwritten.
#' @param ... Ignored
#' @returns Creates a new clean copy of the database. Returns `NULL` invisibly.
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
    # Rename old field names for backward compatibility
    rename_map <- stats::setNames(c("LOC_CODE", "PS_DESCRIPTION",
                                    "TOURN_PHASE_CODE", "SIDE"),
                                  c("LOCATION_CODE", "POINT_SYSTEM_DESCRIPTION",
                                    "TOURNAMENT_PHASE_CODE", "SIDE_DESCRIPTION"))
    matches <- match(names(dat), rename_map)
    if (!all(is.na(matches))) {
      names(dat)[!is.na(matches)] <- names(rename_map)[stats::na.omit(matches)]
      warning_messages <- c(
        warning_messages,
        sprintf("Renaming fields for backward compatibility '%s'.",
                paste(rename_map, collapse = ", ")))
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
    
    tryCatch({
      RSQLite::dbExecute(con_new, sprintf("DELETE FROM %s;", tb))
      dplyr::copy_to(con_new, new_tb, tb, append = TRUE)
    }, error = \(e) {
      pk <-
        RSQLite::dbGetQuery(con_new, sprintf("PRAGMA table_info('%s');", tb)) |>
        dplyr::filter(pk == 1L)
      
      if (grepl("This record is protected and cannot be altered", e$message)) {
        new_tb <-
          dplyr::anti_join(
            new_tb,
            dat_expected,
            pk$name
          )
        if (nrow(new_tb) > 0L) {
          dplyr::copy_to(con_new, new_tb, tb, append = TRUE)
        }
      } else {
        stop(e$message)
      }
    })
    
  }
  if (length(warning_messages) > 0)
    warning(paste(unique(warning_messages), collapse = " "))
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
