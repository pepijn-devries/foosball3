penaltyUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    "Someone commited a foul?",
    personPickerUI(
      ns("mod_naughty"), "Who is getting a penalty?",
      multiple = FALSE,
      optionsCount = 5,
      allowNewOption = FALSE,
      dropboxWrapper = "body"),
    shiny::numericInput(ns("numPenalty"), "Penalty points",
                        min = 0L, value = 0L, step = 1L),
  )
}

penaltyServer <- function(id, tournaments, avatars) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      current_participant <- shiny::reactiveVal()
      current_penalty     <- shiny::reactiveVal(0)

      validator <-
        shinyvalidate::InputValidator$new()
      validator$add_rule(
        "numPenalty",
        shinyvalidate::compose_rules(
          shinyvalidate::sv_integer(),
          shinyvalidate::sv_gte(0)
        )
      )
      validator$enable()
      
      observe({
        shinyjs::toggleState(
          "numPenalty", condition = "ACT" %in% tournaments()$selected$TOURNAMENT_STATE_CODE)
      })
      
      mod_naughty <-
        personPickerServer(
          "mod_naughty", tournaments,
          \() NULL, avatars, NULL, 1L, TRUE)

      get_participant <- shiny::reactive({
        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con) }, add = TRUE)
        person <- mod_naughty()$id_all
        result <- dplyr::tbl(con, "participants") |>
          dplyr::filter(.data$PARTICIPANT_ID %in% person) |>
          dplyr::collect()
        if (!identical(result$PARTICIPANT_ID, current_participant()))
          current_participant(result$PARTICIPANT_ID)
        if (length(result$PENALTY_POINTS) > 0 &&
            !identical(result$PENALTY_POINTS, current_penalty()) &&
            validator$is_valid()) {
          current_penalty(result$PENALTY_POINTS)
        }
        result
      })
      
      shiny::observeEvent(current_penalty(), {
        shiny::updateNumericInput(inputId = "numPenalty", value = current_penalty())
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      shiny::observeEvent(current_penalty(), {
        ptcp <- get_participant()
        
        if (nrow(ptcp) > 0 && "ACT" %in% tournaments()$selected$TOURNAMENT_STATE_CODE &&
            !identical(ptcp$PENALTY_POINTS, current_penalty())) {
          con <- tournaments()$database$connect()
          on.exit({ RSQLite::dbDisconnect(con) }, add = TRUE)
          RSQLite::dbExecute(
            con,
            "UPDATE participants SET PENALTY_POINTS = ? WHERE PARTICIPANT_ID = ?",
            params = list(as.integer(input$numPenalty), as.integer(ptcp$PARTICIPANT_ID))
          )
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      shiny::observe(get_participant())

      return(shiny::reactive({}))
    }
  )
}