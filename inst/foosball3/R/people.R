peopleUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(
    full_screen = TRUE,
    title =
      personPickerUI(ns("mod_peop_pick"),
                     label = "People",
                     multiple = FALSE,
                     allowNewOption = TRUE),
    bslib::nav_panel(
      "Edit records",
      icon = bsicons::bs_icon("pencil-square"),
      recordUI(ns("mod_peop_rec"), "persons")
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
      
      record_pick <- shiny::reactive({
        shiny::req(mod_peop_pick())
        
        list(
          update = function(val) {
            mod_peop_pick()$update(val)
          },
          id = mod_peop_pick()$id
        )
      })
      
      mod_peop_rec <-
        recordServer("mod_peop_rec", tournaments, record_pick, "persons")
      
      return( shiny::reactive({ }) )
    }
  )
}