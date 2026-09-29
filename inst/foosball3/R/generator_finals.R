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
        msg <-
          m$validate |> lapply(`[[`, "message") |> unlist() |>
          paste(collapse = "\n")
        if (msg != "") {
          shinyWidgets::show_alert(
            "Cannot generate final match",
            msg, "error"
          )
        } else {
          tnmt <- matches()$tournament
          con <- tnmt$database$connect()
          on.exit( { RSQLite::dbDisconnect(con) }, add = TRUE)
          new_id <-
            dplyr::tbl(con, "matches") |>
            dplyr::summarise(MATCH_ID = max(.data$MATCH_ID, na.rm = TRUE) + 1L) |>
            dplyr::pull(MATCH_ID)
          dplyr::tbl(con, "matches")
          phase_code <-
            dplyr::tbl(con, "tournament_phases") |>
            dplyr::filter(.data$TOURNAMENT_PHASE %in% type) |>
            dplyr::pull("TOURNAMENT_PHASE_CODE")
          new_match <-
            dplyr::tibble(
              MATCH_ID              = new_id,
              TOURNAMENT_ID         = tnmt$selected$TOURNAMENT_ID,
              TOURNAMENT_PHASE_CODE = phase_code,
              TABLE_CODE            = mod_table()$table$id,
              BALL_ID               = as.integer(mod_table()$ball$id),
            )
          match_results <-
            dplyr::tibble(
              MATCH_ID = new_id,
              RESULT = NA_integer_,
              SIDE_ID = 1L:2L
            )
          parts <-
            dplyr::tbl(con, "participants") |>
            dplyr::filter(
              .data$TOURNAMENT_ID == !!tnmt$selected$TOURNAMENT_ID,
              .data$PERSON_ID %in% !!unlist(mod_table()$config)
            ) |>
            dplyr::collect()
          parts <-
            dplyr::tibble(
              PERSON_ID = as.integer(unlist(mod_table()$config))
            ) |>
            dplyr::left_join(parts, by = "PERSON_ID") |>
            dplyr::pull("PARTICIPANT_ID")
          match_players <-
            dplyr::tibble(
              MATCH_ID       = new_id,
              PARTICIPANT_ID = parts,
              POSITION_CODE  = mod_table()$config |> names()
            )
          dplyr::copy_to(con, new_match, "matches", append = TRUE, temporary = FALSE)
          dplyr::copy_to(con, match_players, "match_players", append = TRUE, temporary = FALSE)
          dplyr::copy_to(con, match_results, "match_results", append = TRUE, temporary = FALSE)
          
          tnmt$trigger_refresh()
        }
      })
      
      return(shiny::reactive({}))
    }
  )
}