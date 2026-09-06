qualGeneratorUI <- function(id) {
  ns <- shiny::NS(id)
  make_slider <- \(id, lab) shiny::sliderInput(ns(id), lab, 0, 1, 1, step = 0.01)
  shiny::tagList(
    bslib::layout_columns(
      colwidths = c(6,6),
      lookupUI(ns("mod_table"), "Table", "On which the tournament is played"),
      lookupUI(ns("mod_ball"), "Ball", "Ball that will be used in tournament"),
      shiny::numericInput(ns("numRevs"), "Number of revolutions", 2L, 1L, step = 1L),
      shiny::numericInput(ns("numNSim"), "Number of simulations", 1000L, 10L, step = 1L),
      shiny::numericInput(ns("numSeed"), "Random seed", NA)
    ),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Tweak match balance",
        bslib::layout_columns(
          colwidths = c(6,6),
          make_slider("slide_tvar", "Team variation"),
          make_slider("slide_ovar", "Opponent variation"),
          make_slider("slide_mvar", "Match variation"),
          make_slider("slide_mbal", "Match balance"),
          make_slider("slide_mbalvar", "Match balance variation"),
          make_slider("slide_mbalextr", "Match balance extremes"),
          make_slider("slide_pbal", "Participant balance"),
          make_slider("slide_pbalvar", "Participant balance variation"),
          make_slider("slide_pbalextr", "Participant balance extremes"),
        ),
        icon = bsicons::bs_icon("sliders")
      )
    )
  )
}

qualGeneratorServer <- function(id, matches, btnStart) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      progress_file <- tempfile()
      new_matches <- shiny::reactiveVal()

      generator <- shiny::ExtendedTask$new(\(participants, type, phase, options) {
        tryCatch({
          unlink(progress_file, TRUE, TRUE)
        }, error = \(e) NULL)
        
        generator_expression <- quote({
          progress_fun <- \(frac) {
            writeLines(as.character(frac), pgf)
          }
          foosball3::foosball3_generate_matches(pt, tp, ph, opt, progress_fun)
        })
        
        if (requireNamespace("mirai", quietly = TRUE)) {
          m <- mirai::mirai(
            generator_expression,
            pt  = participants,
            tp  = type,
            ph  = phase,
            opt = options,
            pgf = progress_file)
          return(m)
        } else {
          eval_env <- list2env(list(
            pt  = participants,
            tp  = type,
            ph  = phase,
            opt = options,
            pgf = progress_file
          ), parent = parent.frame())
          
          eval(avatar_expression, envir = eval_env)
          return(TRUE)
        }
      })
      
      validator <-
        shinyvalidate::InputValidator$new()
      validator$add_rule(
        "numRevs",
        shinyvalidate::compose_rules(
          shinyvalidate::sv_integer(),
          shinyvalidate::sv_gte(1)
        )
      )
      
      validator$add_rule(
        "numNSim",
        shinyvalidate::compose_rules(
          shinyvalidate::sv_integer(),
          shinyvalidate::sv_gte(10)
        )
      )
      
      validator$add_rule(
        "numSeed",
        shinyvalidate::sv_numeric(allow_na = TRUE)
      )
      
      shiny::observe({
        ## TODO
        # browser() #TODO
        m <- matches()
        mod_table()$set_selected(NA)
      })

      shiny::observe({
        ##TODO
        # browser() #TODO
        m <- matches()
        mod_ball()$set_selected(NA)
      })
      
      mod_table <- lookupServer(
        "mod_table", "Table", shiny::reactive({ matches()$tournament }), "tables", "%s", validator)
      mod_ball <- lookupServer(
        "mod_ball", "Ball", shiny::reactive({ matches()$tournament }), "balls", "%s", validator)

      validator$enable()
      
      shiny::observeEvent(btnStart(), {
        sel <- matches()$tournament$selected
        if (length(sel$TOURNAMENT_ID) == 0 || sel$TOURNAMENT_STATE_CODE != "ACT" ||
            nrow(matches()$matches) != 0) {
          shinyWidgets::show_alert(
            "Can't generate matches",
            "Can only generate matches for an active tournament without any existing matches",
            type = "error"
          )
        } else if (!validator$is_valid()) {
          ## TODO messsage seems incomplete
          msgs <- lapply(validator$validate(), `[[`, "message")
          msgs <- do.call(tagList, msgs)
          shinyWidgets::show_alert(
            "Invalid input",
            msgs,
            type = "error"
          )
        } else if(generator$status() == "running") {
          shinyWidgets::show_alert(
            "Already running",
            paste("Cannot start match generator while it is still running.",
                  "Wait for the generator to finish,",
                  "or cancel the current process to start a new generator."),
            type = "warning"
          )
        } else {
          type <- switch(
            sel$TOURNAMENT_TYPE_CODE,
            I = "individual",
            "Unknown"
          )
          m <- matches()
          con <- m$tournament$database$connect()
          on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)

          participants <-
            dplyr::tbl(con, "participants") |>
            dplyr::filter(.data$TOURNAMENT_ID == !!m$tournament$selected$TOURNAMENT_ID) |>
            dplyr::left_join(
              dplyr::tbl(con, "player_statistics") |>
                dplyr::filter(.data$TOURNAMENT_PHASE == "Qualification") |>
                dplyr::select(dplyr::any_of(c("PERSON_ID", "QUALIFICATION_RATE"))),
              by = "PERSON_ID"
            ) |>
            dplyr::collect()
          
          new_mtch <-
            dplyr::tbl(con, "matches") |> 
            dplyr::summarise(
              MATCH_ID = max(.data$MATCH_ID, na.rm = TRUE)
            ) |>
            dplyr::mutate(
              MATCH_ID = ifelse(is.na(MATCH_ID), 0L, MATCH_ID)
            ) |>
            dplyr::pull("MATCH_ID")
          
          new_mtch <- new_mtch + seq_len(input$numRevs * nrow(participants))
          
          new_mtch <-
            dplyr::tibble(
              MATCH_ID = new_mtch,
              TOURNAMENT_ID = m$tournament$selected$TOURNAMENT_ID,
              TOURNAMENT_PHASE_CODE = "Q",
              TABLE_CODE = mod_table()$id,
              BALL_ID = as.integer(mod_ball()$id)
            )
          
          dplyr::copy_to(con, new_mtch, "matches", append = TRUE, temporary = FALSE)
          
          new_matches(new_mtch)

          opts <- list(
            nsim = input$numNSim,
            revolutions = input$numRevs,
            weights = list(
              teamup         = input$slide_tvar,
              opposing       = input$slide_ovar,
              match_var      = input$slide_mvar,
              match_bal      = input$slide_mbal,
              match_bal_var  = input$slide_mbalvar,
              match_bal_extr = input$slide_mbalextr,
              part_bal       = input$slide_pbal,
              part_bal_var   = input$slide_pbalvar,
              part_bal_extr  = input$slide_pbalextr
            ),
            seed = input$numSeed
          )
          
          generator$invoke(participants, type, tolower(m$selected_phase), opts)
          
          foosball_progress(ns("iqual_gen_progress"), "Generating matches...")
          m$tournament$trigger_refresh() ## matches were added
        }
      })

      shiny::observe({
        if (generator$status() == "running") {
          if (file.exists(progress_file)) {
            prog <- as.numeric(readLines(progress_file))
          } else {
            prog <- 0
          }
          
          shinyjs::js$progressbar(
            id = ns("iqual_gen_progress"),
            percent = as.integer(prog*100)
          )
          shiny::invalidateLater(2000)
        } else if (generator$status() != "initial") {
          if (generator$status() == "success") {
            
            nm <- new_matches()
            if (!is.null(nm)) {
              bslib::hide_toast(ns("iqual_gen_progress"))
              
              con <- matches()$tournament$database$connect()
              on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
              
              dplyr::copy_to(
                con,
                generator$result() |>
                  dplyr::mutate(
                    MATCH_ID = nm$MATCH_ID[.data$MATCH_ID]
                  ),
                "match_players", append = TRUE, temporary = FALSE
              )
              dplyr::copy_to(
                con,
                expand.grid(
                  MATCH_ID = nm$MATCH_ID,
                  SIDE_ID = 1L:2L
                ) |>
                  dplyr::mutate(RESULT = NA_integer_),
                "match_results", append = TRUE, temporary = FALSE
              )
              
              new_matches(NULL)
              matches()$tournament$trigger_refresh()
            }
            
          } else {
            shinyWidgets::show_alert(
              "Generation failed",
              "Failed to generate matches for the tournament"
            )
          }
        }
      })
      
      return( shiny::reactive({ }) )
    }
  )
}
