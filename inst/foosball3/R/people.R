peopleUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(
    bslib::nav_panel(
      "Edit records",
      icon = bsicons::bs_icon("pencil-square"),
      personPickerUI(ns("mod_peop_pick"),
                     label = "People",
                     multiple = FALSE,
                     allowNewOption = TRUE),
      recordUI(ns("mod_peop_rec"))
    ),
    bslib::nav_panel(
      "Statistics",
      icon = bsicons::bs_icon("graph-up-arrow"),
      "TODO"
    )
  )
}

peopleServer <- function(id, tournaments, avatars) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      mod_peop_pick <-
        personPickerServer("mod_peop_pick", tournaments,
                           \() NULL, avatars, NULL, 1L)
      mod_peop_rec <-
        recordServer("mod_peop_rec", tournaments, mod_peop_pick,
                     \() "persons")
      
      shiny::observe({ mod_peop_rec }) #TODO
      
      return( shiny::reactive({ }) )
    }
  )
}