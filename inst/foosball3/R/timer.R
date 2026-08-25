timerUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$audio(id = "foosball-aud-precount", src = "audio/precount.mp3", type = "audio/mp3"),
    tags$audio(id = "foosball-aud-midmatch", src = "audio/midmatch.mp3", type = "audio/mp3"),
    shiny::absolutePanel(
      id = ns("timerPanel"),
      bottom = 20, left = 20, width = 400,
      draggable = TRUE, style = "z-index: 10000;",
      style = "display: none;",
      bslib::card(
        class = "foosball-timer-card",
        bslib::accordion(
          bslib::accordion_panel(
            class = "foosball-timer-card",
            title = "Draggable Timer",
            icon = bsicons::bs_icon("stopwatch-fill"),
            bslib::toolbar(
              gap = "5px", align = "left",
              shiny::actionButton(ns("btnStart"), bsicons::bs_icon("play-fill")),
              shiny::actionButton(ns("btnStop"),  bsicons::bs_icon("stop-fill"))
            ),
            div(
              class = "foosball-timer",
              id = ns("foosball-timer"),
              shiny::tags$span("\u200700:00"))
          )
        )
      )
    )
  )
}

timerServer <- function(id, tournament, matches, show) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      shiny::observe({
        shinyjs::toggle(
          "timerPanel", condition = show(),
          anim = TRUE,
          time = 0.25,
          animType = "slide")
      })
      
      get_match_duration <- shiny::reactive({
        shiny::req(tournament())
        shiny::req(matches())
        total_duration <- tournament()$selected$TOURNAMENT_DURATION
        phase <- matches()$selected_phase
        n_matches <- matches()$matches |> nrow()
        if (length(total_duration) == 0 || phase != "Qualification" || n_matches <= 0) {
          120000
        } else {
          round(total_duration*60*60*1000 / n_matches)
        }
      })
      
      shiny::observeEvent( input$btnStart, {
        session$sendCustomMessage("foosball_timer", list(
          operator = "start",
          milliseconds = get_match_duration(),
          timer_id = ns("foosball-timer")
        ))
      })

      shiny::observeEvent( input$btnStop, {
        session$sendCustomMessage("foosball_timer", list(
          operator = "stop",
          milliseconds = get_match_duration(),
          timer_id = ns("foosball-timer")
        ))
      })
      
      return( shiny::reactive({ }) )
    }
  )
}
