#' Start the Foosball App
#' 
#' TODO
#' @param ... TODO
#' @returns Returns NULL silently.
#' @export
foosball3 <- function(...) {
  appdir <- system.file("foosball3", package = "foosball3")
  shiny::runApp(appDir = appdir, ...)
}
