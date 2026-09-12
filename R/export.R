#' Export an Foosball SQLite Database to csv Format
#' 
#' Export a database to a collection of csv-files. Note that you will
#' lose all views, relational information and constraints from the database.
#' @param source File path, where the source SQLite database can be found
#' @param destination File path, where the zipped collection of csv-files
#' should be stored
#' @param validate A `logical` value. If `TRUE`, the `source` will be imported
#' to check for validity (slower). If `FALSE`, `source` will be used directly without
#' checks.
#' @param ... Ignored.
#' @returns `NULL` invisible
#' @examples
#' new_db <- tempfile()
#' foosball3_create_db(new_db)
#' new_export <- tempfile(fileext = ".zip")
#' foosball3_export_db(new_db, new_export)
#' 
#' ## Clean up the files created in this example
#' unlink(new_db, TRUE, TRUE)
#' unlink(new_export, TRUE, TRUE)
#' @export
foosball3_export_db <- function(source, destination, validate = FALSE, ...) {
  if (validate) {
    validated_file <- tempfile()
    ## Use import to ensure the source file is valid
    foosball3_import_db(source, validated_file)
  } else {
    validated_file <- source
  }
  con <- RSQLite::dbConnect(RSQLite::SQLite(), validated_file)
  tbls <-
    dplyr::tbl(con, "sqlite_master") |>
    dplyr::filter(.data$type == "table" &
                    !substr(.data$name, 0, 7) == "sqlite_") |>
    dplyr::pull("name")
  temp_storage <- tempfile()
  if (!dir.exists(temp_storage)) dir.create(temp_storage, recursive = TRUE)
  for (tab in tbls) {
    data <- dplyr::tbl(con, tab) |> dplyr::collect()
    blobs <- data |>
      dplyr::select(dplyr::where(blob::is_blob))
    if (ncol(blobs) > 0) {
      pk <-
        RSQLite::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", tab)) |>
        dplyr::filter(.data$pk == 1) |>
        dplyr::pull("name")
      pk <- data[[pk]]
      if (!dir.exists(tab)) dir.create(file.path(temp_storage, tab), recursive = TRUE)
      for (cn in colnames(blobs)) {
        fileext <- if (startsWith(cn, "JPG")) ".jpg" else ""
        for (i in seq_len(nrow(data))) {
          dat <- blobs[[cn]][[i]]
          if (length(dat) > 0) {
            writeBin(
              blobs[[cn]][[i]],
              file.path(temp_storage, tab, paste0(cn, pk[[i]], fileext))
            )
          }
        }
      }
    }
    data <-
      data |>
      dplyr::mutate( dplyr::across( dplyr::where( blob::is_blob ),
          ~ "<raw>"
        )
      )
    utils::write.csv(
      data,
      file.path(temp_storage, sprintf("%s.csv", tab))
    )
  }
  RSQLite::dbDisconnect(con)
  old_wd <- getwd()
  setwd(temp_storage)
  utils::zip(destination, list.files(temp_storage, recursive = TRUE),
             extras = "-q", flags = "-q")
  setwd(old_wd)
  unlink(temp_storage, TRUE, TRUE)
  unlink(validated_file, TRUE, TRUE)
  invisible()
}