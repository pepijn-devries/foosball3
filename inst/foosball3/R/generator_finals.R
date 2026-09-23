finalGeneratorUI <- function(id, type) {
  ns <- shiny::NS(id)
  shiny::tagList(
    tableArrangementUI(ns("mod_table")),
    "TODO"
  )
}

finalGeneratorServer <- function(id, type, matches, btnStart, avatars, phases) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      person_filter <- shiny::reactiveVal()
      
      get_match_id <- shiny::reactive({
        matches()$matches$MATCH_ID
      })
      
      shiny::observe({
        get_match_id()#TODO
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
          dplyr::arrange(-.data$RESULT) |>
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
        avatars, person_filter, person_filter, teams_filter)
      
      shiny::observe({mod_table()}) #TODO
      
      return(shiny::reactive({}))
    }
  )
}