matchGeneratorUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      bslib::toolbar(
        shiny::actionButton(
          ns("btnStart"), "Start generator", icon = bsicons::bs_icon("play-fill")
        ),
        shiny::actionButton(
          ns("btnRemove"), "Remove matches", icon = bsicons::bs_icon("trash3-fill")
        )
      )
    ),
    bslib::card_body(
      bslib::navset_hidden(
        id = ns("phase-generator"),
        selected = "Qualification",
        bslib::nav_panel_hidden(
          "Qualification",
          "Qualification"
        ),
        bslib::nav_panel_hidden(
          "Semi final",
          "TODO"
        ),
        bslib::nav_panel_hidden(
          "Final",
          "TODO"
        ),
        bslib::nav_panel_hidden(
          "Consolation final",
          "TODO"
        ),
        bslib::nav_panel_hidden(
          "Practice",
          "TODO"
        ),
      )
    )
  )
}

matchGeneratorServer <- function(id, matches) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::observeEvent(matches(), {
        bslib::nav_select("phase-generator", matches()$selected_phase)
      })
      
      return( shiny::reactive({ "TODO" }) )
    }
  )
}