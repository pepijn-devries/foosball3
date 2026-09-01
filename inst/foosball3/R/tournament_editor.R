NUM_DUR_DEFAULT <- 1.5

tournamentEditorUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_column_wrap(
      shiny::numericInput(ns("numDuration"), "Tourn. duration [h]",
                          NUM_DUR_DEFAULT, 0.01, step = 0.01),
      personPickerUI(ns("mod_orgs"),
                     label = "Organisers",
                     multiple = TRUE,
                     allowNewOption = TRUE),
      personPickerUI(ns("mod_part"),
                     label = "Participants",
                     multiple = TRUE,
                     allowNewOption = TRUE),
      lookupUI(ns("mod_loc"), "Location", "Please select tournament location"),
      lookupUI(ns("mod_ps"), "Point System"),
      shinyWidgets::airDatepickerInput(ns("pickDate"))
    ),
    bslib::toolbar(
      shiny::actionButton(ns("btnCancel"), "Cancel"),
      shiny::actionButton(ns("btnOK"), "OK")
    )
  )
}

tournamentEditorServer <- function(
    id, mode, updating, tournaments, tournament_people, avatars) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      result <- shiny::reactiveVal()
      validator <- shinyvalidate::InputValidator$new()

      mod_orgs <-
        personPickerServer("mod_orgs", tournaments,
                           tournament_people()$organisers,
                           avatars, validator, 1L)
      mod_part <-
        personPickerServer("mod_part", tournaments,
                           tournament_people()$participants,
                           avatars, validator, 4L)
      
      shiny::observe({
        mod_orgs(); mod_part() #TODO do I need this to ensure they are updated?
      })
      
      mod_loc <- lookupServer(
        "mod_loc", "Location",
        shiny::reactive({ tournaments()$database }),
        shiny::reactive({ tournaments()$selected }),
        "locations", "LOCATION_CODE", "LOCATION_NAME", "%s", NA, validator)
      mod_ps <- lookupServer(
          "mod_ps", "Point System",
          shiny::reactive({ tournaments()$database }),
          shiny::reactive({ tournaments()$selected }),
          "point_systems", "POINT_SYSTEM_ID", "POINT_SYSTEM_DESCRIPTION", "%s",
          "1", validator)

      validator$add_rule("numDuration",
                         shinyvalidate::sv_gt(0))
      validator$add_rule("pickDate",
                         shinyvalidate::sv_required("Date is required"))
      validator$enable()
      
      shiny::observeEvent(list(mode(), updating()), {
        shiny::req(updating())
        tnmt <- tournaments()
        selected <- tnmt$selected
        if (length(selected$TOURNAMENT_ID) > 0 && !updating()) {
          dt <- NA
          if (!is.na(selected$TOURNAMENT_DATE))
            dt <- lubridate::as_datetime(selected$TOURNAMENT_DATE)
          
          shinyWidgets::updateAirDateInput(
            inputId = "pickDate",
            value = dt
          )
          num_dur <- NUM_DUR_DEFAULT
          if (!is.na(selected$TOURNAMENT_DURATION)) num_dur <- selected$TOURNAMENT_DURATION
          shiny::updateNumericInput(
            inputId = "numDuration",
            value = num_dur
          )
        }
      })
      
      shiny::observeEvent(input$btnOK, {
        con <- tournaments()$database$connect()
        on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
        dplyr::tbl(con, "tournaments")
        if (validator$is_valid()) {
          sel <- tournaments()$selected$TOURNAMENT_ID
          tnmt <-
            dplyr::tbl(con, "tournaments") |>
            dplyr::collect()
          if (length(sel) == 0) sel <- tnmt$TOURNAMENT_ID[[nrow(tnmt)]]
          tnmt <-
            tnmt |>
            dplyr::rows_update(
              data.frame(
                TOURNAMENT_ID = sel,
                TOURNAMENT_DATE = format(input$pickDate, "%Y-%m-%d"),
                TOURNAMENT_DURATION = input$numDuration,
                POINT_SYSTEM_ID = mod_ps() |>
                  as(Class = typeof(tnmt$POINT_SYSTEM_ID)),
                LOCATION_CODE = mod_loc(),
                TOURNAMENT_STATE_CODE = "ACT"
              ),
              by = "TOURNAMENT_ID"
            )
          dplyr::copy_to(con, tnmt, "tournaments", overwrite = TRUE, temporary = FALSE) ## TODO overwrite in copy_to may destroy keys and constraints!
          max_id <-
            dplyr::tbl(con, "participants") |>
            dplyr::summarise(
              PARTICIPANT_ID = max(.data$PARTICIPANT_ID, na.rm = TRUE)) |>
            dplyr::mutate(
              PARTICIPANT_ID = ifelse(is.na(.data$PARTICIPANT_ID), 0,
                                      .data$PARTICIPANT_ID)
            ) |>
            dplyr::pull("PARTICIPANT_ID")
          parts <- mod_part()$id
          parts <-
            data.frame(
              PARTICIPANT_ID = as.integer(max_id + seq_along(parts)),
              TOURNAMENT_ID = sel,
              PERSON_ID = parts,
              PENALTY_POINTS = NA_integer_
            )
          dplyr::copy_to(con, parts, "participants", append = TRUE)
          orgs <- mod_orgs()$id
          orgs <-
            data.frame(
              TOURNAMENT_ID = sel,
              PERSON_ID = orgs
            )
          dplyr::copy_to(con, orgs, "tournament_organisers", append = TRUE)
          
          result(
            list(
              action       = "ok",
              button       = input$btnOK,
              organisers   = mod_orgs()$id,
              participants = mod_part()$id
            )
          )
          
        } else {
          v <- validator$validate()
          msg <- do.call(
            shiny::tagList,
            lapply(v, `[[`, "message") |>
              lapply(shiny::p)
          )
          shinyWidgets::show_alert(
            "Tournament form not complete!",
            text = msg, type = "error"
          )
        }

      })
      
      shiny::observeEvent(input$btnCancel, {
        if (mode() == "new") {
          con <- tournaments()$database$connect()
          on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
          dbtab <- dplyr::tbl(con, "tournaments")
          tid <- tournaments()$selected$TOURNAMENT_ID
          RSQLite::dbExecute(
            con,
            sprintf("DELETE FROM tournaments WHERE TOURNAMENT_ID = %i", tid)
          )
        }

        result(
          list(
            action       = "cancel",
            button       = input$btnCancel
          )
        )
      })
      
      return( result )
    }
  )
}
