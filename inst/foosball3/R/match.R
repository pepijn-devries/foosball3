matchUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::toolbar(
      shiny::actionButton(ns("btnPenalty"), "Assign Penalty"),
      "TODO toolbar"
    ),
    bslib::layout_columns(
      matchSideUI(ns("mod_side_1"), 1L),
      matchSideUI(ns("mod_side_2"), 2L)
    )
  )
}

matchServer <- function(id, match, avatars) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      mod_side_1 <- matchSideServer("mod_side_1", match, 1L, avatars)
      mod_side_2 <- matchSideServer("mod_side_2", match, 2L, avatars)
      
      output$txtTODO <- shiny::renderText({
        m <- match()
        paste("TODO match container", mod_side_1(), mod_side_2())
      })
      
      shiny::observeEvent(input$btnPenalty, {
        #TODO
      })
      
      return(
        shiny::reactive({ }) #TODO
      )
    }
  )
}