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
          finalGeneratorUI(ns("mod_semi"), "semi")
        ),
        bslib::nav_panel_hidden(
          "Final",
          finalGeneratorUI(ns("mod_final"), "final")
        ),
        bslib::nav_panel_hidden(
          "Consolation final",
          finalGeneratorUI(ns("mod_consol"), "consol")
        ),
        bslib::nav_panel_hidden(
          "Practice",
          "TODO"
        ),
      )
    )
  )
}

matchGeneratorServer <- function(id, matches, avatars, phases) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      start_qual   <- shiny::reactiveVal()
      start_semi   <- shiny::reactiveVal()
      start_final  <- shiny::reactiveVal()
      start_consol <- shiny::reactiveVal()
      mod_semi     <- finalGeneratorServer(
        "mod_semi", "Semi final", matches, start_semi, avatars, phases)
      mod_final    <- finalGeneratorServer(
        "mod_final", "Final", matches, start_final, avatars, phases)
      mod_consol   <- finalGeneratorServer(
        "mod_consol", "Consolation final", matches, start_consol, avatars, phases)

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
          switch(
            matches()$phase$selected,
            `Qualification`     = start_qual(input$btnStart),
            `Final`             = start_final(input$btnStart),
            `Semi final`        = start_semi(input$btnStart),
            `Consolation final` = start_consol(input$btnStart)
          )
        }
      })

      do_delete <- function() {
        m <- matches()$matches
        con <- matches()$tournament$database$connect()
        on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
        RSQLite::dbBegin(con)

        tryCatch({
          lapply(c("match_results", "match_players", "matches"), \(x) {
            RSQLite::dbExecute(
              con,
              sprintf("DELETE FROM %s WHERE MATCH_ID IN ('%s')",
                      x, paste(m$MATCH_ID, collapse = "', '")
              )
            )
          })
          RSQLite::dbCommit(con)
          matches()$tournament$trigger_refresh()
          
        }, error = \(e) {
          
          RSQLite::dbRollback(con)
          shinyWidgets::show_alert(
            "Failed to delete matches", e$msg, type = "error")
        })
        
      }
      
      shiny::observeEvent(input$btnRemove, {
        m <- matches()$matches
        if (nrow(m) == 0) {
          shinyWidgets::show_alert(
            "Cannot delete",
            "There are no matches to delete",
            type = "warning"
          )
        } else if (!(all(is.na(m$SCORE_1)) && all(is.na(m$SCORE_2)))) {
          shinyWidgets::ask_confirmation(
            ns("confirmDelete"),
            "Are you sure?",
            "The matches already have results. All will be lost.")
        } else {
          do_delete()
        }
      })
      
      shiny::observeEvent(input$confirmDelete, {
        if (input$confirmDelete) do_delete()
      })

      mod_qual <- qualGeneratorServer("mod_qual", matches, start_qual)

      shiny::observe({ mod_qual() })
      
      return( shiny::reactive({ }) )
    }
  )
}