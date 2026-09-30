personStatUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotUI(ns("mod_plot"))
  )
}

personStatServer <- function(id, tournaments, person_picker) {
  moduleServer(
    id,
    function(input, output, session) {
      get_person_hist_data <- shiny::reactive({
        tnmt <- tournaments()
        con <- tnmt$database$connect()
        on.exit( { RSQLite::dbDisconnect(con) }, add = TRUE)
        
        dat <-
          dplyr::tbl(con, "participant_match_results") |>
          dplyr::filter(.data$TOURNAMENT_PHASE == "Qualification") |>
          dplyr::select(dplyr::any_of(c(
            "TOURNAMENT_ID", "PERSON_ID", "PARTICIPANT", "SUCCESS_RATE"))) |>
          dplyr::collect()
        dat <-
          dplyr::bind_rows(
            dat,
            dat |>
              dplyr::filter(
                .data$PERSON_ID %in% !!person_picker()$id &
                  .data$TOURNAMENT_ID %in% !!tournaments()$selected$TOURNAMENT_ID) |>
              dplyr::mutate(PERSON_ID = -1L)
          ) |>
          dplyr::mutate(
            Who = dplyr::recode_values(
              .data$PERSON_ID,
              -1 ~ paste(.data$PARTICIPANT, "this tournament"),
              as.integer(person_picker()$id) ~ paste(.data$PARTICIPANT, "all tournaments"),
              default = "All players"
            )
          ) |>
          dplyr::rename(
            Participant = "PARTICIPANT",
            `Success Rate` = "SUCCESS_RATE"
          )
        
      })
      
      get_layers <- shiny::reactive({
        nbreaks <- 15
        breaks <- seq(-50/nbreaks, 100 + 50/nbreaks, 100/nbreaks)
        ggplot2::ggplot(
          ggplot2::aes(x       = 100*.data$`Success Rate`,
                       fill    = .data$Who,
                       group   = .data$Who,
                       tooltip = ggplot2::after_stat(.data$density)),
          data = get_person_hist_data()
        ) +
          ggiraph::geom_histogram_interactive(
            ggplot2::aes(y = .data$..density..),
            alpha = 0.8, breaks = breaks,
            position = ggplot2::position_dodge(3)) +
          ggplot2::stat_density(
            kernel = "gaussian", alpha = 0.3,
            position = "identity") +
          ggplot2::xlim(c(-50/nbreaks, 100 + 50/nbreaks)) +
          ggplot2::xlab("Likelihood of winning (%)")
        
      })

      mod_plot <- plotServer(
        "mod_plot", get_layers)
    }
  )
}