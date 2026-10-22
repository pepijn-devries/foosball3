#' Create a New Foosball Database
#'
#' Creates an SQLite file at the specified location. It will contain
#' all required relationships and constraints and some non-trivial data.
#' @param file File path where the database needs to be stored
#' @param ... Ignored
#' @returns Returns `NULL` invisibly.
#' @examples
#' fl <- tempfile(fileext = ".sqlite")
#' foosball3_create_db(fl)
#' 
#' ## Clean up:
#' unlink(fl, TRUE, TRUE)
#' @export
foosball3_create_db <- function(file, ...) {
  .foosball3_create_db(file, ...)
}
