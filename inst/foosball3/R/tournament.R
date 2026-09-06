tournament_label <- "Tournament Details"

tournamentUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(
    full_screen = TRUE,
    bslib::nav_panel(
      "Tournament Setup",
      bslib::layout_columns(
        col_widths = c(3, 9),
        bslib::card(
          bslib::card_header("Menu"),
          bslib::card_body(
            shinyWidgets::virtualSelectInput(
              ns("selectTournament"), "Select tournament",
              character(), keepAlwaysOpen = TRUE,
              search = TRUE, optionsCount = 4,
              noOptionsText = "No tournaments in database"
            ),
            shiny::actionButton(ns("btnNew"), "New Tournament",
                                icon = bsicons::bs_icon("folder-plus")),
            shiny::actionButton(ns("btnEdit"), "Edit Tournament",
                                icon = bsicons::bs_icon("pencil-square"))
          )
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            tournament_label,
            id = ns("foosball-tournament-header")
          ),
          bslib::card_body(
            bslib::navset_hidden(
              id = ns("nav_tournament"),
              bslib::nav_panel(
                "Details",
                bslib::layout_columns(
                  col_widths = c(4, 8),
                  bslib::card(
                    bslib::card_body(
                      shiny::uiOutput(ns("txtDetails"))
                    )
                  ),
                  bslib::card(
                    full_screen = TRUE,
                    bslib::card_body(
                      pictureUI(ns("mod_picture"))
                    )
                  )
                )
              ),
              bslib::nav_panel(
                "Tournament Editor",
                tournamentEditorUI(ns("mod_editor"))
              )
            )
          )
        )
      )
    ),
    bslib::nav_panel(
      "Tournament statistics",
      tournamentStatsUI(ns("mod_tourn_stats"))
    )
  )
}

tournamentServer <- function(id, db, avatars) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      face_click <- shiny::reactiveVal()
      
      mod_picture <- pictureServer("mod_picture", get_it_all)
      shiny::observe({ face_click(mod_picture()) })
      shiny::observe({ face_click(avatars()$click) })
      
      wait_for_update      <- shiny::reactiveVal()
      edit_mode            <- shiny::reactiveVal()
      ## When new data is written to database
      tournament_update    <- shiny::reactiveVal()
      new_id               <- shiny::reactiveVal()
      current_organisers   <- shiny::reactiveVal()
      current_participants <- shiny::reactiveVal()
      
      get_tournament_people <- shiny::reactive({
        list(
          organisers = current_organisers,
          participants = current_participants
        )
      })
      
      shiny::observeEvent(editor(), {
        new_id(-1L)
        shinyjs::enable("btnNew")
        shinyjs::enable("btnEdit")
        shinyjs::enable("selectTournament")
        bslib::nav_select("nav_tournament", selected = "Details")
        shinyjs::html(
          id = "foosball-tournament-header",
          html = tournament_label
        )

      })
      
      shiny::observeEvent(get_tournaments(), {
        update_tournament_selector()
      })

      update_tournament_selector <- shiny::reactive({
        tab <- get_tournaments()
        id_new <- new_id() %||% -1L
        current_sel <- input$selectTournament
        if (id_new > 0L) current_sel <- as.character(id_new)
        shinyWidgets::updateVirtualSelect(
          inputId = "selectTournament",
          choices = structure(
            as.character(tab$TOURNAMENT_ID), names = tab$label
          ),
          selected = current_sel
        )
        wait_for_update(FALSE)
      })

      trigger_refresh = function() {
        tournament_update((tournament_update() %||% 0L) + 1L)
      }
      
      get_it_all <- shiny::reactive({
        list(
          face_click      = face_click(),
          state           = tournament_update(),
          trigger_refresh = trigger_refresh,
          tournaments     = get_tournaments(),
          selected        = get_selected(),
          database        = db()
        )
      })
      
      editor <- tournamentEditorServer(
        "mod_editor", edit_mode, wait_for_update, get_it_all, get_tournament_people, avatars)
      
      get_tournaments <- shiny::reactive({
        editor()
        tournament_update()
        con <- db()$connect()
        on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
        dplyr::tbl(con, "tournaments") |>
          dplyr::left_join(
            dplyr::tbl(con, "locations"),
            by = "LOCATION_CODE"
          ) |>
          dplyr::arrange(.data$TOURNAMENT_DATE) |>
          dplyr::collect() |>
          dplyr::mutate(
            label = {
              dt <- ifelse(
                is.na(.data$TOURNAMENT_DATE),
                "", format(as.POSIXct(.data$TOURNAMENT_DATE), "%Y"))
              lc <- ifelse(
                is.na(.data$LOCATION_NAME),
                "Undefined", .data$LOCATION_NAME)
              sprintf("%s %s", dt, lc)
            })
      })
      
      get_selected <- shiny::reactive({
        new_id()
        get_tournaments() |>
          dplyr::filter(TOURNAMENT_ID == input$selectTournament) |>
          as.list()
      })
      
      output$txtDetails <- shiny::renderUI({
        tnmt <- get_selected()
        if (length(tnmt$TOURNAMENT_ID) == 0 ||
            is.na(tnmt$TOURNAMENT_ID)) {
          "No tournament selected"
        } else {
          con <- db()$connect()
          on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
          vw <- dplyr::tbl(con, "tournaments_view") |>
            dplyr::filter(.data$TOURNAMENT_ID == !!tnmt$TOURNAMENT_ID) |>
            dplyr::collect()
          df <-
            data.frame(
              Item = c(
                "Location",
                "Date",
                "Number of participants",
                "Duration",
                "Organiser(s)",
                "Number of participants",
                "Number of matches (played / total)",
                "Champion"),
              Property = c(
                vw$LOCATION,
                lubridate::as_datetime(vw$TOURNAMENT_DATE) |>
                  format("%Y-%m-%d"),
                vw$NUMBER_OF_PARTICIPANTS,
                sprintf("%.1f hours", vw$TOURNAMENT_DURATION),
                vw$ORGANISERS,
                vw$NUMBER_OF_PARTICIPANTS,
                sprintf("%i / %i", vw$MATCHES_PLAYED, vw$NUMBER_OF_MATCHES),
                ifelse(is.na(vw$CHAMPION), "Undefined", vw$CHAMPION)),
              check.names = FALSE
            )
          knitr::kable(df, format = "html") |>
            shiny::HTML()
        }
      })
      
      shiny::observeEvent(shiny::req(edit_mode()), {
        shinyjs::disable("btnNew")
        shinyjs::disable("btnEdit")
        shinyjs::disable("selectTournament")
        bslib::nav_select("nav_tournament", selected = "Tournament Editor")
        shinyjs::html(
          id = "foosball-tournament-header",
          html = "Tournament Editor"
        )
      })

      shiny::observeEvent(input$btnNew, {
        tnmt <- get_it_all()
        con <- tnmt$database$connect()
        on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
        id_new <- max(c(0L, tnmt$tournaments$TOURNAMENT_ID)) + 1L
        empty <-
          dplyr::tbl(con, "tournaments") |>
          dplyr::filter(dplyr::row_number() == -1L) |>
          dplyr::collect() |>
          dplyr::add_row(
            TOURNAMENT_ID = id_new,
            TOURNAMENT_TYPE_CODE = 'I',
            TOURNAMENT_STATE_CODE = "ACT",
          )
        dplyr::copy_to(con, empty,
                       name = "tournaments",
                       append = TRUE, temporary = FALSE)
        pp <- character()
        attr(pp, "ts") <- Sys.time()
        current_organisers(pp)
        current_participants(pp)
        new_id(id_new)
        trigger_refresh()
        mode <- "new"
        attr(mode, "ts") <- Sys.time()
        wait_for_update(TRUE)
        update_tournament_selector()
        edit_mode(mode)
      })
      
      shiny::observeEvent(input$btnEdit, {
        sel <- get_selected()
        if (length(sel$TOURNAMENT_ID) == 0) {
          shinyWidgets::show_alert(
            "Can't Edit",
            "Please select a tournament first",
            type = "error")
        } else if (sel$TOURNAMENT_STATE_CODE != "ACT") { # TODO ignore while testing
          shinyWidgets::show_alert(
            "Can't Edit",
            "Can't edit a tournament when it is not active",
            type = "error")
        } else {
          con <- db()$connect()
          on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
          n_matches <-
            dplyr::tbl(con, "matches") |>
            dplyr::filter(.data$TOURNAMENT_ID == !!sel$TOURNAMENT_ID) |>
            dplyr::summarise(n = dplyr::n()) |>
            dplyr::pull("n")
          if (n_matches > 0) {
            shinyWidgets::show_alert(
              "Can't Edit",
              "Can't edit a tournament when it has associated matches. Please remove existing matches first",
              type = "error")
          } else {
            init_edit()
          }
        }
      })

      init_edit <- shiny::reactive({
        tid <- get_selected()$TOURNAMENT_ID
        con <- db()$connect()
        on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
        orgs <-
          dplyr::tbl(con, "tournament_organisers") |>
          dplyr::filter(.data$TOURNAMENT_ID == !!tid) |>
          dplyr::left_join(
            dplyr::tbl(con, "persons"), "PERSON_ID"
          ) |>
          dplyr::select("PERSON_ID", "PERSON_NAME") |>
          dplyr::collect()
        orgs <- structure(as.character(orgs$PERSON_ID), names = orgs$PERSON_NAME)
        current_organisers(orgs)
        parts <-
          dplyr::tbl(con, "participants") |>
          dplyr::filter(.data$TOURNAMENT_ID == !!tid) |>
          dplyr::left_join(
            dplyr::tbl(con, "persons"), "PERSON_ID"
          ) |>
          dplyr::select("PERSON_ID", "PERSON_NAME") |>
          dplyr::collect()
        parts <- structure(as.character(parts$PERSON_ID), names = parts$PERSON_NAME)
        current_participants(parts)
        
        new_id(-1L)
        mode <- "edit"
        attr(mode, "ts") <- Sys.time()
        edit_mode(mode)
      })
      
      mod_tourn_stats <- tournamentStatsServer("mod_tourn_stats", get_it_all)
      
      return(get_it_all)
    }
  )
}
