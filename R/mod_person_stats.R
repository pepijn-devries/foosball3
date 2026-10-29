personStatUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectizeInput(
      ns("selectStatType"), "Stat Type", list(
        `Histogram` = "hist",
        `Conclusions barplot` = "barconc",
        `Tournament points barplot` = "barpoint",
        `Ranking barplot` = "barrank",
        `Success rate over time` = "timeline"
      )),
    plotUI(ns("mod_plot"))
  )
}

.get_person_hist_data <- function(con, tournament_id, person_id, ...) {
  filt1 <- \(x) dplyr::filter(x, .data$PERSON_ID %in% !!person_id)
  filt2 <- \(x) dplyr::filter(x, .data$TOURNAMENT_ID %in% !!tournament_id)
  
  dat <-
    dplyr::tbl(con, "participant_match_results") |>
    dplyr::filter(.data$TOURNAMENT_PHASE == "Qualification")
  
  dat
  col_types <-
    dat |>
    utils::head(1) |>
    dplyr::collect() |>
    sapply(class) |>
    c(Who = "character")
  fix_types <- \(x){
    x |>
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          \(y) {
            tp <- col_types[dplyr::cur_column()]
            methods::as(y, tp)
          }
        )
      )
  }
  dat |> dplyr::mutate(Who = "All players") |> dplyr::collect() |>
    fix_types()
  dplyr::bind_rows(
    dat |> dplyr::mutate(Who = "All players") |> dplyr::collect() |>
      fix_types(),
    dat |> filt1() |> dplyr::mutate(Who = paste(.data$PARTICIPANT, "all tournaments")) |>
      dplyr::collect() |> fix_types(),
    dat |> filt2() |> filt1() |> dplyr::mutate(Who = paste(.data$PARTICIPANT, "this tournament")) |>
      dplyr::collect() |> fix_types()
  ) |>
    dplyr::rename(
      Participant = "PARTICIPANT",
      `Success Rate` = "SUCCESS_RATE"
    )
}

.get_person_time_line <- function(con, tournament_id, person_id, ...) {
  .get_person_hist_data(con, tournament_id, person_id, ...) |>
    dplyr::filter(grepl(" all tournaments$", .data$Who)) |>
    dplyr::left_join(
      dplyr::tbl(con, "tournaments_view") |>
        dplyr::select(c("TOURNAMENT_ID", "CITY")) |>
        dplyr::collect(),
      "TOURNAMENT_ID"
    ) |>
    dplyr::group_by(.data$CITY, .data$TOURNAMENT_DATE) |>
    dplyr::summarise(
      n                   = dplyr::n(),
      `Success Rate Mean` = sum(.data$RESULT, na.rm = TRUE) /
        sum(.data$RESULT + .data$RESULT_OPPONENT, na.rm = TRUE),
      `Success Rate Min`  = min(.data$`Success Rate`, na.rm = TRUE) |>
        suppressWarnings(),
      `Success Rate Max`  = max(.data$`Success Rate`, na.rm = TRUE) |>
        suppressWarnings(),
      .groups = "keep"
    ) |>
    dplyr::mutate(
      Date                = lubridate::as_date(.data$TOURNAMENT_DATE)
    )
}

.get_person_quantile <- function(con, person_id, ...) {
  dat <- .get_tournament_stats(con, NULL, NULL, person_id) |>
    dplyr::mutate(
      TOURNAMENT_QUANTILE =
        100*(.data$TOURNAMENT_RANK - 0.5)/.data$NUMBER_OF_PARTICIPANTS,
      POINTS_PER_MATCH = floor(.data$Secured)/.data$MATCHES_PLAYED
    )
}

#' @include mod_tournament_stats.R
#' @noRd
personStatServer <- function(id, tournaments, person_picker) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      get_data <- shiny::reactive({
        tnmt <- tournaments()
        con <- tnmt$database$connect()
        on.exit( { RSQLite::dbDisconnect(con) }, add = TRUE)
        
        switch(
          input$selectStatType,
          hist =
            .get_person_hist_data(
              con,
              tournaments()$selected$TOURNAMENT_ID,
              person_picker()$id),
          barconc =
            .get_tournament_player_stats(
              con,
              tournaments()$selected$TOURNAMENT_ID,
              NULL,
              person_picker()$id),
          barpoint = {
            .get_person_quantile(con, person_picker()$id)
          },
          barrank = {
            .get_person_quantile(con, person_picker()$id)
          },
          timeline = {
            .get_person_time_line(
              con,
              tournaments()$selected$TOURNAMENT_ID,
              person_picker()$id)
          },
          data.frame()
        )
      })
      
      get_layer <- shiny::reactive({
        switch(
          input$selectStatType,
          hist = {
            \(obj) {
              density <- NULL
              nbreaks <- 15
              breaks <- seq(-50/nbreaks, 100 + 50/nbreaks, 100/nbreaks)
              obj +
                ggiraph::geom_histogram_interactive(
                  mapping =
                    ggplot2::aes(
                      x       = 100*.data$`Success Rate`,
                      y       = ggplot2::after_stat(density),
                      fill    = .data$Who,
                      group   = .data$Who,
                      tooltip = ggplot2::after_stat(.data$density)),
                  alpha = 0.8, breaks = breaks,
                  position = ggplot2::position_dodge(3)) +
                ggplot2::stat_density(
                  ggplot2::aes(
                    x       = 100*.data$`Success Rate`,
                    fill    = .data$Who,
                    group   = .data$Who,
                  ),
                  kernel = "gaussian", alpha = 0.3,
                  position = "identity") +
                ggplot2::xlim(c(-50/nbreaks, 100 + 50/nbreaks)) +
                ggplot2::xlab("Likelihood of winning (%)")
            }
          },
          barconc = {
            \(obj) {
              count <- NULL
              conc <- levels(obj$data$Conclusion)
              pal  <- ggplot2::scale_fill_brewer(palette = "Pastel1")
              pal$train(letters[1:length(conc)])
              pal  <- pal$palette(length(conc))
              pal[c(1, 3)] <- pal[c(3, 1)]
              fill_map <- stats::setNames(pal, conc)
              
              obj +
                ggiraph::geom_bar_interactive(
                  ggplot2::aes(
                    x       = .data$Tournament_name,
                    fill    = .data$Conclusion,
                    data_id = paste(.data$Tournament_name, .data$Conclusion, sep = "|"),
                    tooltip = sprintf("%i matches", ggplot2::after_stat(count))
                  ),
                  position = "fill"
                ) +
                ggplot2::scale_fill_manual(
                  values = fill_map, drop = FALSE, limits = names(fill_map)
                ) +
                ggplot2::labs(x = "Tournament", y = "Percentage of matches") +
                ggplot2::scale_y_continuous(labels = \(x) sprintf("%.0f%%", x * 100)) +
                ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45))
            }
          },
          barpoint = {
            \(obj) {
              
              obj +
                ggplot2::facet_grid(Point_system~., scales = "free") +
                ggiraph::geom_bar_interactive(
                  ggplot2::aes(
                    x = .data$Tournament_name,
                    y = .data$POINTS_PER_MATCH,
                    data_id = .data$Tournament_name,
                    tooltip = sprintf(
                      "Total of %i points in %i matches",
                      floor(.data$Secured),
                      .data$MATCHES_PLAYED)),
                  stat = "identity") +
                ggplot2::scale_fill_brewer(palette = "Pastel1") +
                ggplot2::labs(
                  x = "Tournament",
                  y = "Average points per match"
                ) +
                ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45))
            }
          },
          barrank = {
            \(obj) {
              
              obj +
                ggiraph::geom_bar_interactive(
                  ggplot2::aes(
                    x = .data$Tournament_name,
                    y = .data$TOURNAMENT_QUANTILE,
                    data_id = .data$Tournament_name,
                    tooltip = sprintf(
                      "Rank %i of %i during qualification",
                      .data$TOURNAMENT_RANK,
                      .data$NUMBER_OF_PARTICIPANTS)),
                  stat = "identity") +
                ggplot2::scale_fill_brewer(palette = "Pastel1") +
                ggplot2::labs(
                  x = "Tournament",
                  y = "Tournament qualification quantile\n(Top x% rank)"
                ) +
                ggplot2::scale_y_continuous(labels = \(x) sprintf("%.0f%%", x),
                                            lim = c(0, 101)) +
                ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45))
            }
          },
          timeline = {
            \(obj) {
              a <- ggplot2::aes(
                x = .data$Date, y = .data$`Success Rate Mean`,
                col = .data$CITY)
              obj +
                ggiraph::geom_line_interactive(mapping = a) +
                ggiraph::geom_point_interactive(
                  mapping = utils::modifyList(
                    a, 
                    ggplot2::aes(
                      data_id = paste(.data$Date, .data$CITY, sep = "|"),
                      tooltip = sprintf("%.0f%% Success", 100*.data$`Success Rate Mean`)
                    )
                  )
                ) +
                ggiraph::geom_ribbon_interactive(
                  ggplot2::aes(
                    x = .data$Date,
                    fill = .data$CITY,
                    ymin = .data$`Success Rate Min`,
                    ymax = .data$`Success Rate Max`), alpha = 0.1, colour = NA) +
                ggplot2::labs(
                  y = "Success (%)"
                ) +
                ggplot2::scale_y_continuous(labels = \(x) sprintf("%.0f%%", 100*x),
                                            lim = c(0, 1.01))
            }
          },
          \(obj) obj + ggplot2::geom_blank()
        )
        
      })
      
      get_layers <- shiny::reactive({
        layer <- get_layer()
        ggplot2::ggplot(data = get_data()) |>
          layer()
      })
      
      mod_plot <- plotServer(
        "mod_plot", get_layers)
    }
  )
}