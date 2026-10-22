#' Start the Foosball App
#' 
#' Renders the Graphical User Interface and starts the server
#' to interact with a Foosball SQLite database.
#' @param ... Ignored
#' @returns Returns a `shiny.appobj` object.
#' @include suggests.R
#' @export
foosball3 <- function(...) {
  if (foosball3_suggests_ok()) {
    if (requireNamespace("mirai", quietly = TRUE))
      mirai::daemons(2)
    
    appdir <- system.file("foosball3", package = "foosball3")
    shiny::addResourcePath("foosball3", file.path(appdir, "www"))
    shiny::shinyApp(ui("main", appdir), server, ...)
  } else {
    stop(
      "Not all required suggests available. Call `foosball3_install_suggests()` and try again."
    )
  }
}
