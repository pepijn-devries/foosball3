tournamentStatsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectizeInput(
      ns("selectStatType"), "Stat Type", c("Tournament points")),
    ggiraph::girafeOutput(ns("plotStat"))
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
        
        switch(
          input$selectStatType,
          `Tournament points` = {
            dplyr::tbl(con, "participant_tournament_results") |>
              dplyr::filter(.data$TOURNAMENT_ID %in% !!sel &
                              .data$TOURNAMENT_PHASE == "Qualification") |> #TODO make this dynamic
              dplyr::collect() |>
              tidyr::pivot_longer(
                c("REMAINING_POINTS", "TOURNAMENT_POINTS"),
                names_to = "State",
                values_to = "Points"
              )
            
          })
      })

      output$plotStat <- ggiraph::renderGirafe({
        dat <- get_data()
        if (nrow(dat) == 0) {
          plot() #TODO
        } else {
          ggobj <-
            ggplot2::ggplot(data = dat) +
            ggplot2::aes(x = .data$PARTICIPANT, y = floor(.data$Points), fill = .data$State) +
            ggiraph::geom_bar_interactive(stat = "identity") +
            ggplot2::ylab("Tournament points") +
            ggplot2::xlab("Participant") +
            ggplot2::scale_fill_brewer(
              palette = "Pastel1", name = "Points",
              labels = c("Remaining", "Secured")) +
            ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45)) +
            ggplot2::theme_light()
          ggiraph::girafe(ggobj)
        }
      })      
    }
  )
}