finalGeneratorUI <- function(id, type) {
  ns <- shiny::NS(id)
  shiny::tagList(
    tableArrangementUI(ns("mod_table"))
  )
}

finalGeneratorServer <- function(id, type, matches, btnStart, avatars, phases) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      person_filter <- shiny::reactiveVal()
      
      get_match_id <- shiny::reactive({
        tnmt <- matches()$tournament
        con <- tnmt$database$connect()
        on.exit( { RSQLite::dbDisconnect(con) }, add = TRUE)
        
        dplyr::tbl(con, "matches") |>
          dplyr::left_join(
            dplyr::tbl(con, "tournament_phases") |>
              dplyr::select(dplyr::any_of(c("TOURNAMENT_PHASE_CODE",
                                           "TOURNAMENT_PHASE"))),
            by = "TOURNAMENT_PHASE_CODE"
          ) |>
          dplyr::filter(.data$TOURNAMENT_ID %in% !!tnmt$selected$TOURNAMENT_ID &
                          .data$TOURNAMENT_PHASE %in% !!type) |>
          dplyr::collect()
        })
      
      get_match_config <- shiny::reactive({
        m <- get_match_id()
        tnmt <- matches()$tournament
        con <- tnmt$database$connect()
        on.exit( { RSQLite::dbDisconnect(con) }, add = TRUE)
        dplyr::tbl(con, "match_players") |>
          dplyr::filter(.data$MATCH_ID %in% !!m$MATCH_ID) |>
          dplyr::left_join(
            dplyr::tbl(con, "participants"),
            by = "PARTICIPANT_ID"
          ) |>
          dplyr::select(dplyr::any_of(c(
            "PERSON_ID", "POSITION_CODE"))) |>
          dplyr::collect()
      })
      
      get_previous_results <- shiny::reactive({
        m <- matches()
        prev <- m$phase$get_previous(type, m$phase$available)
        if (is.null(prev)) return(NULL)
        con <- m$tournament$database$connect()
        on.exit({ RSQLite::dbDisconnect(con) }, add = TRUE)
        dplyr::tbl(con, "participant_tournament_results") |>
          dplyr::filter(.data$TOURNAMENT_ID == !!m$tournament$selected$TOURNAMENT_ID &
                          .data$TOURNAMENT_PHASE == !!prev$TOURNAMENT_PHASE) |>
          dplyr::collect()
      })
      
      get_candidates <- shiny::reactive({
        prev <- get_previous_results()
        if (is.null(prev)) return(integer())
        rank_nr <-
          switch(
            type,
            `Final`             = 1:2, ## Players ranking 1 and 2 from semi final
            `Consolation final` = 3:4, ## Players ranking 3 and 4 from semi final
            `Semi final`        = 1:4) ## Players ranking 1 to 4 from qualification
        prev |>
          dplyr::arrange(-.data$TOURNAMENT_POINTS) |>
          dplyr::filter(dplyr::row_number() %in% !!rank_nr) |>
          dplyr::pull("PERSON_ID")
      })

      shiny::observe({
        cand <- get_candidates()
        if (!identical(cand, person_filter())) person_filter(cand)
      })

      teams_filter <- shiny::reactive({
        pf <- person_filter()
        if (length(pf) == 4L) {
          list(
            `first and fourth` = pf[c(1L, 4L)],
            `second and third` = pf[2L:3L])
        } else NULL
      })

      mod_table <- tableArrangementServer(
        "mod_table", shiny::reactive({ matches()$tournament }),
        avatars, person_filter, person_filter, teams_filter, TRUE,
        get_match_config)
      
      shiny::observeEvent(btnStart(), {
        m <- mod_table()
        browser() #TODO
      })
      
      return(shiny::reactive({}))
    }
  )
}