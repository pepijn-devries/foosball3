#' Import Foosball Tournament Data from a File
#' 
#' If you have an older file with tournament results, you can try to
#' import those results with this function. It will create a new copy
#' of the database (using [foosball3_create_db()]), making sure it
#' complies with the latest database scheme specification used by this package.
#' @param file An SQLite file or a zipped collection of csv files (created
#' with [foosball3_export_db()]), to be imported into the standardised database
#' structure.
#' @param target Target file path where the clean database will be stored.
#' Existing files at this location may be overwritten.
#' @param ... Ignored
#' @returns Creates a new clean copy of the database. Returns `NULL` invisibly.
#' @examples
#' tf <- tempfile()
#' export <- tempfile(fileext = ".zip")
#' import <- tempfile()
#' foosball3_create_db(tf)
#' foosball3_export_db(tf, export)
#' foosball3_import_db(export, import)
#' 
#' # Clean up example files
#' unlink(tf, TRUE, TRUE)
#' unlink(export, TRUE, TRUE)
#' unlink(import, TRUE, TRUE)
#' @export
foosball3_import_db <- function(file, target, ...) {
  foosball3_create_db(target)
  warning_messages <- character()
  is_zip <- endsWith(tolower(file), ".zip")
  con_new <- RSQLite::dbConnect(RSQLite::SQLite(), target)
  on.exit({RSQLite::dbDisconnect(con_new)}, add = TRUE)
  if (is_zip) {
    temp_storage <- tempfile()
    if (!dir.exists(temp_storage)) dir.create(temp_storage, recursive = TRUE)
    utils::unzip(file, exdir = temp_storage)
  } else {
    con_imp <- RSQLite::dbConnect(RSQLite::SQLite(), file)
    on.exit({RSQLite::dbDisconnect(con_imp)}, add = TRUE)
  }
  
  sql_code <-
    "SELECT name 
     FROM sqlite_schema 
     WHERE type = 'table' 
     AND name NOT LIKE 'sqlite_%';"
  tb_nms <- if (is_zip) {
    list.files(temp_storage, pattern = "\\.csv$") |>
      stringr::str_replace_all("\\.csv$", "")
  } else {
    RSQLite::dbGetQuery(con_imp, sql_code) |>
      dplyr::pull("name")
  }
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
    col_known <- dplyr::tbl(con_new, tb) |> colnames()
    pk <-
      RSQLite::dbGetQuery(con_new, sprintf("PRAGMA table_info('%s');", tb)) |>
      dplyr::filter(pk == 1L)
    
    if (is_zip) {
      dat <- utils::read.csv(file.path(temp_storage, sprintf("%s.csv", tb)))
      if (dir.exists(file.path(temp_storage, tb))) {
        blob_cols <-
          dplyr::tbl(con_new, tb) |>
          dplyr::filter(dplyr::row_number() == -1) |>
          dplyr::collect() |>
          dplyr::select(
            dplyr::where(blob::is_blob)
          ) |> colnames()
        blob_cols <- intersect(names(dat), blob_cols)
        pk_vals <- dat[[pk$name]]
        for (bc in blob_cols) {
          fileext <- if (startsWith(bc, "JPG")) ".jpg" else ""
          bcf <-
            if (startsWith(bc, "JPG") &&
                !any(startsWith(bc, list.files(file.path(temp_storage, tb))))) "pic" else
                  bc
          blob_dat <-
            lapply(
              pk_vals, \(id) {
                tryCatch({
                  fn <- file.path(temp_storage, tb, paste0(bcf, id, fileext))
                  fz <- file.size(fn)
                  readBin(fn, "raw", fz) |>
                    blob::as_blob()
                }, error = \(e) blob::as_blob(NA))
              })
          blob_dat <- do.call(c, blob_dat)
          dat[[bc]] <- blob_dat
        }
      }
    } else {
      dat <-
        dplyr::tbl(con_imp, tb) |>
        dplyr::collect()
    }
    col_imp   <- colnames(dat)
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
    if (length(object) == 0) blob::blob() else
      object <- blob::as_blob(object)
  } else {
    if (is.character(object) && !all(is.na(object)))
      object[!is.na(object) & object == ""] <- NA_character_
    object <- withCallingHandlers(
      methods::as(object, Class),
      warning = function(w) {
        if (grepl("NAs introduced by coercion", w$message)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }
  object
}
