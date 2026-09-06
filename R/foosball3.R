#' Start the Foosball App
#' 
#' Renders the Graphical User Interface and starts the server
#' to interact with a Foosball SQLite database.
#' @param ... Ignored
#' @returns Returns `NULL` invisibly.
#' @export
foosball3 <- function(...) {
  suggests <- utils::packageDescription("foosball3")$Suggests |>
    strsplit(",\n", perl = TRUE) |> unlist()
  suggests <- suggests[suggests != "mirai"]
  state <- lapply(suggests, requireNamespace) |> unlist()
  if (all(state)) {
    appdir <- system.file("foosball3", package = "foosball3")
    shiny::runApp(appDir = appdir, ...)
  } else {
    stop(
      sprintf("Install required packages and try again: %s",
              paste(suggests[!state], collapse = ", "))
    )
  }
}
