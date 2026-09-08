#' Start the Foosball App
#' 
#' Renders the Graphical User Interface and starts the server
#' to interact with a Foosball SQLite database.
#' @param ... Ignored
#' @returns Returns `NULL` invisibly.
#' @include suggests.R
#' @export
foosball3 <- function(...) {
  if (foosball3_suggests_ok()) {
    appdir <- system.file("foosball3", package = "foosball3")
    shiny::runApp(appDir = appdir, ...)
  } else {
    stop(
      "Not all required suggests available. Call `foosball3_install_suggests()` and try again."
    )
  }
}
