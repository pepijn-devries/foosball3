matchUI <- function(id) {
  ns <- shiny::NS(id)
  opts <- list(
    animation = TRUE,
    delay = list(show = 750, hide = 100))
  shiny::tagList(
    bslib::toolbar(
      gap = "5px",
      bslib::tooltip(
        shiny::actionButton(ns("btnDown"), bsicons::bs_icon("arrow-down")),
        "Go to next match",
        options = opts, placement = "auto"
      ),
      bslib::tooltip(
        shiny::actionButton(ns("btnUp"), bsicons::bs_icon("arrow-up")),
        "Go to previous match",
        options = opts, placement = "auto"
      ),
      bslib::tooltip(
        shiny::actionButton(ns("btnAnnounce"), bsicons::bs_icon("megaphone-fill")),
        "Announce match players",
        options = opts, placement = "auto"
      ),
      bslib::tooltip(
        shiny::actionButton(ns("btnPenalty"), bsicons::bs_icon("heartbreak-fill")),
        "Assign penalty points to a player",
        options = opts, placement = "auto"
      )
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
      shinyjs::js$speech_supported(id = session$ns("speech_supported"))
      
      shiny::observe({
        shinyjs::toggle("btnAnnounce", condition = input$speech_supported)
      })
      
      shiny::observeEvent(input$btnAnnounce, {
        m <- match()$matches |>
          dplyr::filter(.data$MATCH_ID == !!match()$selected_id)
        msg <- if (nrow(m) == 0) {
          "Select a match first"
        } else {
          coupling1 <- "en" #TODO depend on language
          coupling2 <- "versus"  #TODO depend on language
          paste(m$PLAYER_DEFENSE_1, coupling1, m$PLAYER_STRIKE_1, coupling2,
                m$PLAYER_STRIKE_2, coupling1, m$PLAYER_DEFENSE_2)
        }
        shinyjs::js$announce(
          speech = msg,
          lang = "EN",
          pitch = 0.9,
          rate = 0.8
        )
        
      })
      
      shiny::observeEvent(input$btnUp, {
        match()$move_match(list(direction = -1L, button = input$btnUp))
      })

      shiny::observeEvent(input$btnDown, {
        match()$move_match(list(direction = 1L, button = input$btnDown))
      })
      
      shiny::observeEvent(input$btnPenalty, {
        #TODO
      })
      
      return(
        shiny::reactive({
          list(
            side1 = mod_side_1(),
            side2 = mod_side_2()
          )
        })
      )
    }
  )
}