tableArrangementUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(2, 2),
    bslib::card(
      bslib::card_body(
        personPickerUI(ns("mod_d1"), "d1"),
        personPickerUI(ns("mod_s1"), "s1"),
        "TODO left"
      )
    ),
    bslib::card(
      bslib::card_body(
        personPickerUI(ns("mod_s2"), "s2"),
        personPickerUI(ns("mod_d2"), "d1"),
        "TODO right"
      )
    )
    
  )
}

tableArrangementServer <- function(id, tournaments, avatars) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      mod_d1 <-
        personPickerServer(
          "mod_d1", tournaments, \() NULL, avatars, NULL, 1L, TRUE)
      mod_s1 <-
        personPickerServer(
          "mod_s1", tournaments, \() NULL, avatars, NULL, 1L, TRUE)
      mod_d2 <-
        personPickerServer(
          "mod_d2", tournaments, \() NULL, avatars, NULL, 1L, TRUE)
      mod_s2 <-
        personPickerServer(
          "mod_s2", tournaments, \() NULL, avatars, NULL, 1L, TRUE)
      
      return(shiny::reactive({}))
    }
  )
}