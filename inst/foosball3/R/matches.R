matchesUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(3, 9),
    
    bslib::card(
      bslib::card_body(
        shiny::selectInput(
          ns("selectPhase"), "Tournament Phase",
          character()),
        shinyWidgets::virtualSelectInput(
          ns("selectMatch"), "Select match", character(),
          keepAlwaysOpen = TRUE, search = TRUE, optionsCount = 9,
          noOptionsText = "No matches in selected tournament")
      )
    ),
    bslib::navset_card_tab(
      id = ns("match-tab"),
      bslib::nav_panel(
        "Match Details", matchUI(ns("mod_match")),
        icon = bsicons::bs_icon("hammer")
      ),
      bslib::nav_panel(
        "Generate Matches", matchGeneratorUI(ns("mod_gen")),
        icon = bsicons::bs_icon("database-fill-gear")
      )
    )
  )
}

matchesServer <- function(id, tournament, avatars) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      phases <- NULL
      mod_match <- matchServer("mod_match", get_selected_match, avatars)
      mod_gen   <- matchGeneratorServer("mod_gen", get_selected_match)
      
      get_matches <- shiny::reactive({
        shiny::req(input$selectPhase)
        tnmt <- tournament()
        con <- tnmt$database$connect()
        on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
        dplyr::tbl(con, "matches_view") |>
          dplyr::filter(
            .data$TOURNAMENT_PHASE == input$selectPhase &
              .data$TOURNAMENT_ID == !!max(c(-1,  tnmt$selected$TOURNAMENT_ID))
          ) |>
          dplyr::collect() |>
          dplyr::mutate(
            match_name = paste0(
              dplyr::row_number(), " - ",
              .data$PLAYER_DEFENSE_1, " + ",
              .data$PLAYER_STRIKE_1, " vs. ",
              .data$PLAYER_DEFENSE_2, " + ",
              .data$PLAYER_STRIKE_2
            )
          )
      })
      
      observeEvent(mod_match(), {
        current <- input$selectMatch
        mtchs <- get_matches()
        if (!is.null(current) && nrow(mtchs) > 0) {
          move <- mod_match()
          if (is.na(current) || current == "") {
            if (move$direction == 1L) {
              new_id <- mtchs$MATCH_ID[[1L]]
            } else {
              new_id <- mtchs$MATCH_ID[[nrow(mtchs)]]
            }
          } else {
            row_id <- which(mtchs$MATCH_ID == as.integer(current)) + move$direction
            if (row_id > nrow(mtchs) || row_id < 1L) {
              new_id <- NA_character_
            } else {
              new_id <- mtchs$MATCH_ID[[row_id]]
            }
          }
          shinyWidgets::updateVirtualSelect(
            "selectMatch", selected = new_id)
        }
      })
      
      shiny::observeEvent(get_matches(), {
        mtchs       <- get_matches()
        match_names <- dplyr::pull(mtchs, "match_name")
        bslib::nav_select(
          "match-tab",
          ifelse(nrow(mtchs) == 0, "Generate Matches", "Match Details")
        )
        selected <-
          if (!is.null(input$selectMatch) &&
              as.integer(input$selectMatch) %in% mtchs$MATCH_ID) {
            input$selectMatch
          } else {
            NA_character_
          }
        shinyWidgets::updateVirtualSelect(
          inputId = "selectMatch",
          choices = mtchs$MATCH_ID |> stats::setNames(match_names),
          selected = selected
        )
      })
      
      shiny::observe({
        tnmt <- tournament()
        con <- tnmt$database$connect()
        on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
        new_phases <- dplyr::tbl(con, "tournament_phases") |>
          dplyr::collect()
        new_phases <- new_phases$TOURNAMENT_PHASE
        if (!identical(new_phases, phases)) {
          phases <<- new_phases
          shiny::updateSelectInput(
            inputId = "selectPhase",
            choices = phases,
            selected = phases[[1]]
          )
        }
      })

      get_selected_match <- shiny::reactive({
        # shiny::req(input$selectMatch) #TODO this blocks a lot of triggers
        list(
          tournament     = tournament(),
          matches        = get_matches(),
          selected_phase = input$selectPhase,
          selected_id    = input$selectMatch
        )
      })
      
      return(
        get_selected_match
      )
    }
  )
}