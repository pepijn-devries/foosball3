qualGeneratorUI <- function(id) {
  ns <- shiny::NS(id)
  make_slider <- \(id, lab) shiny::sliderInput(ns(id), lab, 0, 1, 1, step = 0.01)
  tagList(
    bslib::layout_columns(
      colwidths = c(6,6),
      lookupUI(ns("mod_table"), "Table", "On which the tournament is played"),
      lookupUI(ns("mod_ball"), "Ball", "Ball that will be used in tournament"),
      shiny::numericInput(ns("numRevs"), "Number of revolutions", 2L, 1L, step = 1L),
      shiny::numericInput(ns("numSeed"), "Random seed", NA)
    ),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Tweak match balance",
        bslib::layout_columns(
          colwidths = c(6,6),
          make_slider("slide_tvar", "Team variation"),
          make_slider("slide_ovar", "Opponent variation"),
          make_slider("slide_mvar", "Match variation"),
          make_slider("slide_mbal", "Match balance"),
          make_slider("slide_mbalvar", "Match balance variation"),
          make_slider("slide_mbalextr", "Match balance extremes"),
          make_slider("slide_pbal", "Participant balance"),
          make_slider("slide_pbalvar", "Participant balance variation"),
          make_slider("slide_pbalextr", "Participant balance extremes"),
        ),
        icon = bsicons::bs_icon("sliders")
      )
    )
  )
}

qualGeneratorServer <- function(id, matches, btnStart) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      validator <-
        shinyvalidate::InputValidator$new()
      validator$add_rule(
        "numRevs",
        shinyvalidate::compose_rules(
          shinyvalidate::sv_integer(),
          shinyvalidate::sv_gte(1)
        )
      )

      validator$add_rule(
        "numSeed",
        shinyvalidate::sv_numeric(allow_na = TRUE)
      )
      
      #id, label, db, initial, table, field_id, name = NULL, fmt = "%s", default = NA, validator = NULL
      mod_table <- lookupServer(
        "mod_table", "Table",
        shiny::reactive({ matches()$tournament$database }),
        shiny::reactive({ matches()$tournament$selected }),
        "tables", "TABLE_CODE", "TABLE_NAME", "%s", NA, validator)
      mod_ball <- lookupServer(
        "mod_ball", "Ball",
        shiny::reactive({ matches()$tournament$database }),
        shiny::reactive({ matches()$tournament$selected }),
        "balls", "BALL_ID", "BALL_DESCRIPTION", "%s", -1L, validator)
      
      validator$enable()
      
      shiny::observeEvent(btnStart(), {
        sel <- matches()$tournament$selected()
        if (length(sel$TOURNAMENT_ID) == 0 || sel$TOURNAMENT_STATE_CODE != "ACT" ||
            nrow(matches()$matches) != 0) {
          shinyWidgets::show_alert(
            "Can't generate matches",
            "Can only generate matches for an active tournament without any existing matches",
            type = "error"
          )
        } else if (!validator$is_valid()) {
          msgs <- lapply(validator$validate(), `[[`, "message") |> shiny::tags$p()
          msgs <- do.call(tagList, msgs)
          shinyWidgets::show_alert(
            "Invalid input",
            "Can only generate matches with invalid input",
            type = "error"
          )
        } else {
          browser() #TODO start generator
        }
      })

      return( shiny::reactive({ }) )
    }
  )
}