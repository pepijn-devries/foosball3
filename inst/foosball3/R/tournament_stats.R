tournamentStatsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      shiny::selectizeInput(
        ns("selectStatType"), "Stat Type", c("Tournament points")),
      shiny::selectizeInput(
        ns("selectPlayerOrder"), "Arrange by",
        c("Name", "Position", "Potential position"))
    ),
    plotUI(ns("mod_stat"))
  )
}

tournamentStatsServer <- function(id, tournaments) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      get_data <- shiny::reactive({
        tnmt <- tournaments()
        con <- tnmt$database$connect()
        on.exit( { RSQLite::dbDisconnect(con) }, add = TRUE)
        sel <- tnmt$selected$TOURNAMENT_ID
        
        shinyjs::toggleElement(
          "selectPlayerOrder",
          condition = input$selectStatType == "Tournament points")

        switch(
          input$selectStatType,
          `Tournament points` = {
            dat <-
              dplyr::tbl(con, "participant_tournament_results") |>
              dplyr::filter(.data$TOURNAMENT_ID %in% !!sel &
                              .data$TOURNAMENT_PHASE == "Qualification") |>
              dplyr::collect() |>
              dplyr::rename(
                Remaining   = "REMAINING_POINTS",
                Secured     = "TOURNAMENT_POINTS",
                Participant = "PARTICIPANT"
              ) |>
              tidyr::pivot_longer(
                c("Remaining", "Secured"),
                names_to = "State",
                values_to = "Points"
              ) |>
              dplyr::mutate(
                `Tournament points` = floor(.data$Points)
              )
            name_order <-
              switch(
                input$selectPlayerOrder,
                Name = {
                  dat$Participant[order(toupper(dat$Participant))] |>
                    unique()
                },
                Position = {
                  dat |>
                    dplyr::filter(.data$State == "Secured") |>
                    dplyr::arrange(-.data$Points) |>
                    dplyr::pull("Participant") |>
                    unique()                  
                },
                `Potential position` = {
                  dat |>
                    dplyr::group_by(.data$Participant) |>
                    dplyr::summarise(Points = sum(.data$Points)) |>
                    dplyr::arrange(-.data$Points) |>
                    dplyr::pull("Participant") |>
                    unique()                  
                }
              )
            dat |>
              dplyr::mutate(Participant = factor(.data$Participant,
                                                 name_order))
          },
          data.frame()
          )
      })
      
      mod_stat <- plotServer(
        "mod_stat", get_data,
        ggplot2::aes(x       = .data$Participant,
                     y       = .data$`Tournament points`,
                     fill    = .data$State,
                     tooltip = .data$Points,
                     data_id = .data$Participant))
      
    }
  )
}