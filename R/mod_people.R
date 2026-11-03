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
      personStatUI(ns("mod_stat"))
    )
  )
}

peopleServer <- function(id, tournaments, avatars, picture) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      mods <- shiny::reactiveValues(
        picker = NULL,
        stat = NULL,
        rec = NULL
      )
      
      shiny::observe({
        shiny::req(avatars())
        shiny::req(!is.null(avatars()$options))
        
        if (is.null(mods$picker) && length(avatars()$options) > 0L) {
          mods$picker <- personPickerServer(
            "mod_peop_pick",
            tournaments,
            avatars,
            NULL,
            1L,
            allow_new = TRUE
          )
        }
      })
      
      shiny::observeEvent(mods$picker, {
        shiny::req(!is.null(mods$picker))
        avatars()$refresh_options()
      }, ignoreNULL = FALSE)
      
      shiny::observeEvent(tournaments(), {
        shiny::req(tournaments())
        shiny::req(!is.null(tournaments()$database))
        if (!is.null(avatars())) {
          avatars()$refresh_options()
        }
      }, ignoreNULL = TRUE)
      
      return( shiny::reactive({ }) )
    }
  )
}