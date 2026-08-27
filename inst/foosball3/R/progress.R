foosball_progress <- function(id, text) {
  bslib::show_toast(
    bslib::toast(
      shiny::div(
        class = "d-flex flex-column gap-2 w-100",
        shiny::div(text, class = "foosball-progress-text", style = "margin-bottom: 8px;"),
        shiny::div(
          class = "progress",
          shiny::div(
            class = "foosball-progress-bar progress-bar-striped progress-bar-animated bg-primary",
            style = "width: 0%;",
            " 0%"
          )
        )
      ),
      duration_s = NA,
      id = id,
      type = NULL,
      position = "bottom-right")
  )
}
