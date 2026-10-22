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
#' @include create.R
#' @export
foosball3_get_schema <- function(con, include_views = FALSE, ...) {
  .foosball3_get_schema(con, include_views, ...)
}
