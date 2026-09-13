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
          qualGeneratorUI(ns("mod_qual"))
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
      start_qual <- shiny::reactiveVal()
      
      shiny::observeEvent(matches(), {
        bslib::nav_select("phase-generator", matches()$phase$selected)
      })
      
      shiny::observeEvent(input$btnStart, {
        msg <- matches()$phase$message
        if (!is.null(msg)) {
          shinyWidgets::show_alert(
            "Can't generate matches", msg, type = "error"
          )
        } else {
          #TODO add other phases
          switch(
            matches()$phase$selected,
            Qualification = start_qual(input$btnStart)
          )
        }
      })

      mod_qual <- qualGeneratorServer("mod_qual", matches, start_qual)

      shiny::observe({ mod_qual() })
      
      return( shiny::reactive({ }) )
    }
  )
}