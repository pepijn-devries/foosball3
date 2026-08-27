coinUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    shiny::absolutePanel(
      id = ns("coinPanel"),
      bottom = 20, left = 425, width = 400,
      draggable = TRUE, style = "z-index: 10000;",
      style = "display: none;",
      bslib::card(
        class = "foosball-drag-card", #TODO remain to draggable card?,
        "TODO flip a coin here"
      )
    )
  )
}

coinServer <- function(id, show) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::observe({
        shinyjs::toggle(
          "coinPanel", condition = show(),
          anim = TRUE,
          time = 0.25,
          animType = "slide")
      })
      
    }
  )
}