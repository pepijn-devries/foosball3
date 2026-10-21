#' Get foosball3 Schema Data
#' 
#' Returns information about the relationships between tables in the
#' database used by `foosball3`
#' @param con A connection to a copy of a foosball3 database.
#' Can be created with [foosball3_create_db()] or [foosball3_import_db()].
#' @param include_views If `TRUE` it will include 'view' tables, otherwise
#' it will exclude them. Default is `FALSE`
#' @param ... Ignored.
#' @returns A `dm` class object with relational information.
#' @examples
#' if (requireNamespace("dm")) {
#'   db_file <- tempfile(fileext = ".db")
#'   foosball3_create_db(db_file)
#'   con <- RSQLite::dbConnect(RSQLite::SQLite(), db_file)
#'   on.exit({
#'     RSQLite::dbDisconnect(con)
#'     unlink(db_file, TRUE, TRUE)
#'   }, add = TRUE)
#'   
#'   foosball3_get_schema(con, TRUE)
#' }
#' @export
foosball3_get_schema <- function(con, include_views = FALSE, ...) {
  tbls <- RSQLite::dbListTables(con)
  
  if (requireNamespace("dm")) {
    my_dm <- dm::dm_from_con(con, learn_keys = TRUE)
    if (include_views) {
      all_tbls <- RSQLite::dbListTables(con)
      unreferenced <- setdiff(all_tbls, names(my_dm))
      unreferenced <- unreferenced[!grepl("^sqlite_", unreferenced)]
      
      unreferenced <-
        lapply(unreferenced, dplyr::tbl, src = con) |>
        stats::setNames(unreferenced)
      for (i in seq_along(unreferenced)) {
        my_dm <-
          my_dm |>
          dm::dm(!!names(unreferenced)[[i]] := unreferenced[[i]])
      }
    }
    return(my_dm)
  } else {
    rlang::abort(c(
      x = "This function needs the 'dm' package",
      i = "Please install 'dm' and try again"
    ))
  }
}
