news_tree     <- readLines("www/data/newstree.json") |>
  jsonlite::fromJSON(FALSE)
news_messages <- utils::read.csv("www/data/news.csv")

newsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "foosball-news-flash",
    id = ns("foosball-news-bar"),
    shiny::tags$div(
      id = ns("foosball-news"),
      class = "foosball-news-text",
      "News will appear here")
  )
}

newsServer <- function(id, matches, mod_avatar, show) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      shiny::observe({
        shinyjs::toggle(
          "foosball-news-bar",
          condition = show()
        )
      })
      
      all_input <- shiny::reactive({
        list(
          matches = matches,
          avatars = mod_avatar
        )
      })

      get_chance_fun <- function(fun_name) {
        switch(
          fun_name,
          length_gt = \(x, a, b) {
            ifelse(length(x) > a, b, 1 - b)
          },
          I
        )
      }
      
      get_param <- function(param_name) {
        inp <- all_input()
        m <- inp$matches()
        sel_tour <- m$tournament$selected
        
        switch(
          param_name,
          selected_tournament = sel_tour$TOURNAMENT_ID,
          NULL
        )
      }
      
      get_chance <- function(fun_name, fun_params) {
        fun <- get_chance_fun(fun_name)
        fun_params[[1]] <- get_param(fun_params[[1]])
        do.call(fun, fun_params)
      }

      pick_person <- function(con, tour_id) {
        dplyr::tbl(con, "participants") |>
          dplyr::select(c("PERSON_ID", "TOURNAMENT_ID")) |>
          dplyr::collect() |>
          dplyr::mutate(
            pick_weight = ifelse(
              ## A person enrolled in current tournament should
              ## have 10x higher chance of being picked:
              .data$TOURNAMENT_ID %in% !!tour_id, 10, 1
            ),
            pick_weight = cumsum(.data$pick_weight) / sum(.data$pick_weight),
            threshold = runif(1)
          ) |>
          dplyr::filter(.data$pick_weight > .data$threshold) |>
          dplyr::slice_head(n = 1)
      }
      
      pick_participant <- function(con, tour_id) {
        dplyr::tbl(con, "participants") |>
          dplyr::filter(.data$TOURNAMENT_ID %in% !!tour_id) |>
          dplyr::slice_sample(n = 1L) |>
          dplyr::collect()
      }
      
      pivot_results <- function(tab_name, con, current) {
        tab <- dplyr::tbl(con, tab_name)
        tab <- 
          dplyr::union(
            tab,
            tab |> dplyr::mutate(ROLE_CODE = "ANY")
          ) |>
          dplyr::mutate(
            WHEN = "ANYTIME"
          )
        current_tab <-
          tab |>
          dplyr::filter(.data$TOURNAMENT_ID == !!current) |>
          dplyr::mutate(WHEN = "NOW")
        tab <-
          dplyr::union(
            tab |> dplyr::mutate(PARTICIPANT_ID = NA),
            current_tab
          )
        
        tab |>
          dplyr::left_join(
            current_tab |>
              dplyr::select(ID_lookup = "PARTICIPANT_ID", "PARTICIPANT") |>
              dplyr::distinct(),
            "PARTICIPANT"
          ) |>
          dplyr::mutate(PARTICIPANT_ID = dplyr::coalesce(
            .data$PARTICIPANT_ID, .data$ID_lookup)) |>
          dplyr::group_by(
            .data$PERSON_ID, .data$TOURNAMENT_PHASE,
            .data$PARTICIPANT_ID, .data$PARTICIPANT,
            .data$ROLE_CODE, .data$WHEN) |>
          dplyr::summarise(
            RESULT              = sum(.data$RESULT, na.rm = TRUE),
            RESULT_OPPONENT     = sum(.data$RESULT_OPPONENT, na.rm = TRUE),
            RESULT_TOTAL        = sum(.data$RESULT + .data$RESULT_OPPONENT, na.rm = TRUE),
            RESULT_SD           = sd(.data$RESULT, na.rm = TRUE),
            RESULT_OPPPONENT_SD = sd(.data$RESULT_OPPONENT, na.rm = TRUE),
            SUCCESS_SD          = sd(100*.data$RESULT / (.data$RESULT + .data$RESULT_OPPONENT), na.rm = TRUE),
            WINS                = sum(.data$CONCLUSION == "WIN", na.rm = TRUE),
            LOSSES              = sum(.data$CONCLUSION == "LOSE", na.rm = TRUE),
            DRAWS               = sum(.data$CONCLUSION == "DRAW", na.rm = TRUE),
            N_MATCHES           = dplyr::n(),
            .groups = "keep"
            ) |>
          dplyr::ungroup() |>
          dplyr::mutate(
            SUCCESS_RATE = ifelse(.data$RESULT_TOTAL == 0, 0,
                                  100*.data$RESULT / .data$RESULT_TOTAL)
          ) |>
          tidyr::pivot_wider(
            id_cols = c("TOURNAMENT_PHASE", "PERSON_ID",
                        "PARTICIPANT_ID", "PARTICIPANT", "WHEN"),
            names_from = "ROLE_CODE",
            values_from = -c("TOURNAMENT_PHASE", "PERSON_ID",
                             "PARTICIPANT_ID", "PARTICIPANT", "WHEN", "ROLE_CODE")
          ) |>
          tidyr::pivot_wider(
            id_cols = c("TOURNAMENT_PHASE", "PERSON_ID",
                        "PARTICIPANT_ID", "PARTICIPANT"),
            names_from = "WHEN",
            values_from = -c("TOURNAMENT_PHASE", "PERSON_ID",
                             "PARTICIPANT_ID", "PARTICIPANT", "WHEN")
          )
      }
      
      get_news_table <- function(tab_name, con, current) {
        if (grepl("\\\\", tab_name)) {
          tab_name <- strsplit(tab_name, "\\|") |> unlist()
          switch(
            tab_name[[1]],
            `\\pivot_results` = pivot_results(tab_name[-1], con, current),
            stop(sprintf("Unknown processing function '%s'", tab_name[1]))
          )
        } else {
          dplyr::tbl(con, tab_name)
        }
      }
      
      get_news <- shiny::reactive({
        tnmt <- matches()$tournament
        .get_filter <- function(filter_fun, con) {
          switch(
            filter_fun,
            person_filter = function(x) {
              dplyr::filter(x, .data$PERSON_ID ==
                              !!pick_person(con, tnmt$selected$TOURNAMENT_ID)$PERSON_ID &
                              .data$TOURNAMENT_PHASE == "Qualification")
            },
            participant_filter = function(x) {
              dplyr::filter(x, .data$PARTICIPANT_ID ==
                              !!pick_participant(con, tnmt$selected$TOURNAMENT_ID)$PARTICIPANT_ID &
                              .data$TOURNAMENT_PHASE == "Qualification")
            },
            tournament_filter = function(x) {
              dplyr::filter(x, .data$TOURNAMENT_ID %in% tnmt$selected$TOURNAMENT_ID)
            },
            I)
        }
        
        .postprocess_params <- function(params, par_names) {
          idx <- grepl("\\\\", par_names)
          command <- gsub("\\\\", "", par_names[idx] |> unique())
          idx <- which(idx)
          .prep_arg <- function(pms, i, fun, ...) {
            arg <- pms[[i - 1]]
            is_date <- grepl("DATE", names(pms)[[i - 1]])
            if (is_date) {
              arg <- lubridate::as_date(arg)
            }
            result <- fun(arg, ...)
            if (is_date) result <- lubridate::format_ISO8601(result)
            result
          }
          params <-
            lapply(seq_along(command), \(i) {
              switch(
                command[i],
                nrow  = nrow(as.data.frame(params)) |> list(),
                min   = .prep_arg(params, idx[i], min),
                max   = .prep_arg(params, idx[i], max),
                mean  = .prep_arg(params, idx[i], mean, na.rm = TRUE),
                isare = {
                  params[idx] <- dplyr::if_else(unlist(params[idx - 1]) == 1L, "is", "are")
                  params
                },
                stop(sprintf("Command '%s' not implemented", command))
              )
            })
          do.call(c, params)
        }
        
        .process_message <- function(msg_row) {
          if (nrow(msg_row) == 0) {
            "Something went wrong while generating news. Please report!"
          } else {
            
            if (!is.na(msg_row$message_table) && msg_row$message_table != "") {
              con <- tnmt$database$connect()
              on.exit({ RSQLite::dbDisconnect(con) }, add = TRUE)
              filt <- .get_filter(msg_row$filter_fun, con)
              param_names <- field_names <-
                strsplit(msg_row$message_field, "\\|") |>
                unlist()
              shiny::req(msg_row$message_table)

              params <- get_news_table(msg_row$message_table, con, tnmt$selected$TOURNAMENT_ID)

              if (!any(colnames(params) %in% field_names))
                field_names <- colnames(params)
              
              params <- params |>
                filt() |>
                dplyr::select(
                  dplyr::any_of(field_names)
                ) |>
                dplyr::collect() |>
                as.list()
              
              pars <- lapply(
                field_names, \(nm) if (nm %in% names(params)) {
                  params[[nm]]
                } else character()
              ) |> stats::setNames(field_names)
              if (any(grepl("\\\\", param_names))) {
                pars <- .postprocess_params(pars, param_names)
              }
              do.call(sprintf, c(list(fmt = msg_row$message), pars))
            } else {
              msg_row$message
            }
          }
        }
        
        input$animation_finished # Trigger when animation has finished
        all_input() # Trigger when app state has changed
        walk_tree <- function(tree, what) {
          branches <- !grepl("^chance", names(tree))
          chances <- lapply(tree[branches], \(x) {
            if ("chance" %in% names(x)) {
              x$chance
              if (is.character(x$chance)) {
                get_chance(x$chance_fun, x$chance_params)
              } else {
                x$chance
              }
            } else {
              NULL
            }
          }) |>
            unlist()
          chances <- chances / sum(chances) ## Scale just to make sure
          chances <- cumsum(chances)
          if (length(chances) == 0) {
            msg_sub <-
              news_messages |>
              dplyr::filter( .data$type == what ) |>
              dplyr::mutate(
                pick_weight = cumsum(.data$pick_weight)/ sum(.data$pick_weight),
                threshold = runif(1)
              ) |>
              dplyr::filter(.data$pick_weight > .data$threshold) |>
              dplyr::slice_head(n = 1) |>
              .process_message()
          } else {
            picked <-
              which(runif(1) < chances) |>
              utils::head(1)
            walk_tree(tree[branches][[picked]], names(picked))
          }
        }
        walk_tree(news_tree, names(news_tree))
      })
      
      shiny::observe({
        
        nws <- tryCatch({
          get_news()
        }, error = \(e) {
          paste(
            "If you are seeing this, something went wrong. Please report an issue together with a reprex. Error:",
            e$message
          )
        }) |>
          shiny::tags$h3() |>
          as.character()
        
        session$sendCustomMessage(
          type = "scroll-next-foosball-news", 
          message = list(
            text_id = ns("foosball-news"),
            mod_id = id,
            duration = 10000,
            text = nws)
        )

      })
      
      return( shiny::reactive({ }))
    }
  )
}