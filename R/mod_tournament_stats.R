tournamentStatsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      shiny::selectizeInput(
        ns("selectStatType"), "Stat Type", c(
          "Tournament points",
          "Box whiskers",
          "Conclusions",
          "Heat map",
          "Player experience")),
      shiny::selectizeInput(
        ns("selectPlayerOrder"), "Arrange by",
        c("Name", "Position", "Potential position")),
      shiny::selectizeInput(
        ns("selectCountsFor"), "Counts for",
        c("This tournament only", "All tournaments")),
      shiny::selectizeInput(
        ns("selectWhom"), "Show whom?",
        c("Any opponent", "Opponent, face to face", "Together, side by side")
      )
    ),
    plotUI(ns("mod_stat"))
  )
}

.get_tournament_stats <- function(con, tournament_id, player_order = NULL, ...) {
  dat <- 
    dplyr::tbl(con, "participant_tournament_results") |>
    dplyr::filter(.data$TOURNAMENT_ID %in% !!tournament_id &
                    .data$TOURNAMENT_PHASE == "Qualification") |>
    dplyr::collect() |>
    dplyr::rename(
      Remaining   = "REMAINING_POINTS",
      Secured     = "TOURNAMENT_POINTS",
      Participant = "PARTICIPANT"
    )
    
  if (is.null(player_order)) return(dat)
  name_order <-
    .order_parts(.get_tournament_points_from_dat(dat), player_order)
  dat |>
    dplyr::mutate(Participant = factor(.data$Participant,
                                       name_order))
}

.order_parts <- function(dat, player_order, ...) {
  switch(
    player_order,
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
}

.get_tournament_points_from_dat <- function(dat, ...) {
  dat |>
    tidyr::pivot_longer(
      c("Remaining", "Secured"),
      names_to = "State",
      values_to = "Points"
    ) |>
    dplyr::mutate(
      `Tournament points` = floor(.data$Points)
    )
}

.get_tournament_points <- function(con, tournament_id, ...) {
  dat <-
    .get_tournament_stats(con, tournament_id, ...) |>
    .get_tournament_points_from_dat()
}

.get_tournament_points_order <- function(con, tournament_id, player_order, ...) {
  dat <- .get_tournament_points(con, tournament_id, ...)
  name_order <- .order_parts(dat, player_order, ...)
  dat |>
    dplyr::mutate(Participant = factor(.data$Participant,
                                       name_order))
}

.get_tournament_player_stats <- function(con, tournament_id, player_order, ...) {
  name_order <-
    .order_parts(.get_tournament_points(con, tournament_id), player_order)
  
  dplyr::tbl(con, "participant_match_results") |>
    dplyr::filter(.data$TOURNAMENT_ID %in% !!tournament_id &
                    .data$TOURNAMENT_PHASE == "Qualification") |>
    dplyr::rename(
      Participant = "PARTICIPANT"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      `Role` = dplyr::recode_values(
        .data$ROLE_CODE,
        "D" ~ "Defender",
        "S" ~ "Striker",
        default = "Unknown"
      ),
      Conclusion = dplyr::recode_values(
        .data$CONCLUSION,
        "WIN" ~ "Win",
        "DRAW" ~ "Draw",
        "LOSE" ~ "Lost",
        default = "Undecided"
      ) |>
        factor(c("Win", "Draw", "Lost", "Undecided")),
      `Success rate (%)` = 100*.data$SUCCESS_RATE,
      Participant = factor(.data$Participant,
                           name_order))
}

.get_tournament_combo <- function(con, tournament_id, player_order, whom, what, ...) {
  combos <-
    dplyr::tbl(con, "player_combos")
  combos <- switch(whom,
                   `Any opponent` = {
                     dplyr::filter(combos, substr(.data$COMBI, 2, 2) == "o")
                   },
                   `Opponent, face to face` = {
                     dplyr::filter(combos, .data$COMBI %in% c("DoS", "SoD"))
                   },
                   `Together, side by side` = {
                     dplyr::filter(combos, substr(.data$COMBI, 2, 2) == "w")
                   },
                   combos)
  current <- 
    combos |>
    dplyr::filter(.data$TOURNAMENT_ID == !!tournament_id &
                    .data$TOURNAMENT_PHASE_CODE == "Q")
  combos <- if (what == "This tournament only") {
    current
  } else {
    players <-
      current |>
      dplyr::pull("PLAYER_1") |>
      unique()
      
    combos |>
      dplyr::filter(
        .data$PLAYER_1 %in% !!players &
        .data$PLAYER_2 %in% !!players)
  }
  combos <-
    combos |>
    dplyr::group_by(.data$PLAYER_1, .data$PLAYER_2) |>
    dplyr::summarise(
      Times = dplyr::n()) |>
    dplyr::collect() |>
    dplyr::mutate(
      Times = factor(
        ifelse(.data$Times >= 10, ">=10", as.character(.data$Times)),
        c(0:9, ">=10")
      )
    )
  if (is.null(player_order)) return(combos)
  name_order <- .order_parts(.get_tournament_points(con, tournament_id),
                             player_order)
  combos |>
    dplyr::mutate(
      PLAYER_1 = factor(.data$PLAYER_1, name_order),
      PLAYER_2 = factor(.data$PLAYER_2, name_order)
    )
  
}

.get_tournament_experience <- function(con, tournament_id, ...) {
  dat_tour <-
    .get_tournament_points(con, tournament_id)
  dat <-
    dplyr::tbl(con, "player_statistics") |>
    dplyr::filter(
      .data$TOURNAMENT_PHASE == "Qualification" &
        .data$PERSON_ID %in% !!dat_tour$PERSON_ID
    ) |>
    dplyr::select(
      c("PARTICIPANT", "SUCCESS_RATE", "CITIES_PLAYED", "TOURNAMENTS_PLAYED")
    ) |>
    dplyr::collect() |>
    as.data.frame()
  rownames(dat) <- dat$PARTICIPANT
  pca_res            <- tryCatch({
    stats::prcomp(dat[,names(dat) != "PARTICIPANT", drop = FALSE],
                  center = TRUE, scale. = TRUE)
  }, error = function(e) NULL)
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
          condition = input$selectStatType != "Player experience")

        shinyjs::toggleElement(
          "selectWhom",
          condition = input$selectStatType == "Heat map")

        shinyjs::toggleElement(
          "selectCountsFor",
          condition = input$selectStatType == "Heat map")
        
        switch(
          input$selectStatType,
          `Tournament points` =
            .get_tournament_points_order(con, sel, input$selectPlayerOrder),
          `Box whiskers` =
            .get_tournament_player_stats(con, sel, input$selectPlayerOrder),
          Conclusions =
            .get_tournament_player_stats(con, sel, input$selectPlayerOrder),
          `Heat map` =
            .get_tournament_combo(con, sel, input$selectPlayerOrder,
                                  input$selectWhom, input$selectCountsFor),
          `Player experience` =
            .get_tournament_experience(con, sel),
          data.frame()
          )
      })
      
      get_layer <- shiny::reactive({
        switch(
          input$selectStatType,
          `Tournament points` = {
            \(obj) {
              obj +
              ggiraph::geom_bar_interactive(
                mapping =
                  ggplot2::aes(x       = .data$Participant,
                               y       = .data$`Tournament points`,
                               fill    = .data$State,
                               tooltip = .data$Points,
                               data_id = .data$Participant),
                stat = "identity") +
                ggplot2::scale_fill_brewer(palette = "Pastel1", name = "Points") +
                ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45))
            }
          },
          `Box whiskers` = {
            \(obj) {
              ymax <- upper <- middle <- lower <- ymin <- NULL
              obj +
                ggiraph::geom_boxplot_interactive(
                  ggplot2::aes(
                    x    = .data$Participant,
                    y    = .data$`Success rate (%)`,
                    fill = .data$Role,
                    data_id = paste(.data$Participant, .data$Role, sep = "|"),
                    tooltip = ggplot2::after_stat(paste0(
                      "Max: ",    round(ymax, 1), "%\n",
                      "Q3: ",     round(upper, 1), "%\n",
                      "Median: ", round(middle, 1), "%\n",
                      "Q1: ",     round(lower, 1), "%\n",
                      "Min: ",    round(ymin, 1), "%"
                    ))
                  )
                ) +
                ggplot2::lims(y = c(0, 100)) +
                ggplot2::scale_fill_brewer(palette = "Pastel1") +
                ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45))
              
            }
          },
          Conclusions = {
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
                    x       = .data$Participant,
                    fill    = .data$Conclusion,
                    data_id = paste(.data$Participant, .data$Conclusion, sep = "|"),
                    tooltip = ggplot2::after_stat(count)
                  ),
                  show.legend = TRUE
                ) +
                ggplot2::scale_fill_manual(
                  values = fill_map, drop = FALSE, limits = names(fill_map)
                ) +
                ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45))
            }
          },
          `Heat map` = {
            \(obj) {
              obj +
                ggiraph::geom_tile_interactive(
                  ggplot2::aes(
                    x = .data$PLAYER_1,
                    y = .data$PLAYER_2,
                    fill = .data$Times,
                    data_id = paste(.data$PLAYER_1, .data$PLAYER_2, sep = "|"),
                    tooltip = paste(
                      .data$PLAYER_1,
                      "played",
                      switch(
                        input$selectWhom,
                        `Any opponent` = "against",
                        `Opponent, face to face` = "directly facing",
                        `Together, side by side` = "together with",
                        "against"
                      ),
                      "\n",
                      .data$PLAYER_2,
                      .data$Times,
                      "times"
                    ))
                ) +
                ggplot2::labs(x = "Player side 1", y = "Player side 2") +
                ggplot2::geom_abline(intercept = 0, slope = 1) +
                ggplot2::coord_equal() +
                ggplot2::scale_fill_brewer(palette = "Set3", drop = FALSE) +
                ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 90))
            }
          },
          `Player experience` = {
            \(obj) {
              pca_result <- get_data()
              pca_scores <- as.data.frame(pca_result$x)
              pca_scores$label <- rownames(pca_scores)
              pca_loadings <- as.data.frame(pca_result$rotation)
              pca_loadings$variable <- rownames(pca_result$rotation)
              scaling_factor <- max(abs(pca_scores$PC1)) / max(abs(pca_loadings$PC1)) * 0.7
              pca_loadings$PC1_scaled <- pca_loadings$PC1 * scaling_factor
              pca_loadings$PC2_scaled <- pca_loadings$PC2 * scaling_factor
              
              ggplot2::ggplot() +
                ggplot2::labs(
                  x = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "%)"),
                  y = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "%)")
                ) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
                ggiraph::geom_point_interactive(
                  data = pca_scores,
                  ggplot2::aes(
                    x = .data$PC1,
                    y = .data$PC2,
                    tooltip = .data$label,
                    data_id = .data$label
                  ),
                  size = 3, 
                  color = "steelblue"
                ) +
                ggiraph::geom_segment_interactive(
                  data = pca_loadings,
                  ggplot2::aes(
                    x = 0, y = 0, 
                    xend = .data$PC1_scaled, 
                    yend = .data$PC2_scaled,
                    tooltip = .data$variable,
                    data_id = .data$variable
                  ),
                  arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")), 
                  color = "firebrick", 
                  linewidth = 0.8
                ) +
                ggiraph::geom_text_interactive(
                  data = pca_loadings,
                  ggplot2::aes(
                    x = .data$PC1_scaled * 1.1, 
                    y = .data$PC2_scaled * 1.1, 
                    label = .data$variable,
                    tooltip = .data$variable,
                    data_id = .data$variable
                  ),
                  color = "firebrick", 
                  size = 4
                ) +
                ggplot2::coord_equal()
            }
          },
          \(obj) {
            obj +
              ggplot2::geom_blank()
          }
        )
      })
      
      get_layers <- shiny::reactive({
        layer <- get_layer()
        ggplot2::ggplot(data = get_data()) |>
          layer()
      })
      
      mod_stat <- plotServer("mod_stat", get_layers)
      
    }
  )
}