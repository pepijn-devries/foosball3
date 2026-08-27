#' Start the Foosball App
#' 
#' Renders the Graphical User Interface and starts the server
#' to interact with a Foosball SQLite database.
#' @param ... Ignored
#' @returns Returns `NULL` invisibly.
#' @export
foosball3 <- function(...) {
  appdir <- system.file("foosball3", package = "foosball3")
  shiny::runApp(appDir = appdir, ...)
}
