recordUI <- function(id, label = "Edit record") {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::toolbar(
      gap = "5px",
      shiny::actionButton(ns("btnNew"), bsicons::bs_icon("plus-square-fill")),
      shiny::actionButton(ns("btnSave"), bsicons::bs_icon("floppy2-fill")),
      shiny::actionButton(ns("btnDelete"), bsicons::bs_icon("trash3-fill"))
    ),
    bslib::card(
      bslib::card_header(label),
      bslib::card_body(
        shiny::uiOutput(ns("record_details"))
      )
    )
  )
}

recordServer <- function(id, tournaments, record_picker, table_name) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      new_key_cache <- shiny::reactiveVal()
      ns <- session$ns
      lookup_servers <- shiny::reactiveVal(list())

      validator <- shinyvalidate::InputValidator$new()
      validator$enable()
      
      get_record <- shiny::reactive({
        shiny::req(record_picker())
        shiny::req(tournaments())
        rec_id <- record_picker()$id

        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con)}, add = TRUE)
        pk <- get_primary_key()
        if (pk$type == "INTEGER") rec_id <- as.integer(rec_id) |> suppressWarnings()
        dplyr::tbl(con, table_name()) |>
          dplyr::filter(!!rlang::sym(pk$name) == !!rec_id) |>
          dplyr::collect()
      })
      
      get_primary_key <- shiny::reactive({
        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con)}, add = TRUE)
        pk_info <-
          RSQLite::dbGetQuery(
            con,
            paste0("PRAGMA table_info(", table_name(), ");"))
        pk_info |> dplyr::filter(.data$pk == 1)
      })
      
      get_new_pk <- shiny::reactive({
        pk <- get_primary_key()
        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con)}, add = TRUE)
        existing_keys <-
          dplyr::tbl(con, table_name()) |>
          dplyr::pull(pk$name)
        nk <-
          switch(
            typeof(existing_keys),
            integer = {
              max(c(0L, existing_keys)) + 1L
            }, {
              lazy_table <- dplyr::tbl(con, table_name())
              cur_keys <- lazy_table |> dplyr::pull(pk$name)
              descr <- get_description_field(pk$name, colnames(lazy_table))
              new_key <- input[[descr]] |> toupper()
              for (i in 4L:nchar(new_key)) {
                temp <- abbreviate(new_key, i)
                if (!temp %in% cur_keys) {
                  new_key <- temp
                  break
                }
              }
              new_key[[1]]
            }
          )
        new_key_cache(nk)
        nk
      })
      
      get_foreign_keys <- shiny::reactive({
        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con)}, add = TRUE)
        RSQLite::dbGetQuery(
          con,
          paste0("PRAGMA foreign_key_list(", table_name(), ");"))
      })
      
      shiny::observeEvent(input$btnNew, {
        pk <- get_primary_key()
        # new_key <- get_new_pk() #TODO
        switch(
          pk$type,
          INTEGER = {
            shiny::updateNumericInput(inputId = pk$name, value = NA)
          },
          TEXT = {
            shiny::updateTextInput(inputId = pk$name, value = NA)
          }
          
        )
        NULL
      })

      shiny::observeEvent(input$btnSave, {
        pk <- get_primary_key()
        new_key <- input[[pk$name]]
        if (is.na(new_key) || new_key == "") {
          new_key <- get_new_pk()
        }
        validity <- validator$validate()
        valid <- lapply(validity, `[[`, "message") |> unlist() |> paste(collapse = " ")
        if (valid == "") {
          con <- tournaments()$database$connect()
          on.exit({ RSQLite::dbDisconnect(con)}, add = TRUE)
          lazy_tib <- dplyr::tbl(con, table_name())
          edited_row <-
            lazy_tib |>
            dplyr::filter(dplyr::row_number() == -1) |>
            dplyr::collect() |>
            dplyr::add_row()
          for (nm in colnames(edited_row)) {
            src <- names(input)[which(startsWith(names(input), nm))]
            ##TODO handle datetime objects
            if (src == pk$name) {
              val <- new_key
            } else {
              val <- input[[src]]
            }
            val <- methods::as(val, typeof(edited_row[[nm]]))
            if (is.character(val) && (length(val) == 0 || val == ""))
              val <- NA_character_
            edited_row[[nm]] <- val
          }
          
          tryCatch({
            dplyr::rows_upsert(
              lazy_tib,
              edited_row,
              by = get_primary_key()$name,
              in_place = TRUE,
              copy = TRUE
            )
            record_picker()$update()
          }, error = \(e) {
            shinyWidgets::show_alert( "Failed to Save Record", strip_ansi(e$parent$message),
                                      "error" )
          })
        } else {
          shinyWidgets::show_alert( "Failed to Save Record", valid, "error" )
        }
        
      })
      
      shiny::observeEvent(input$btnDelete, {
        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con)}, add = TRUE)
        tryCatch({
          RSQLite::dbExecute(
            con,
            sprintf("DELETE FROM %s WHERE %s = '%s'",
              table_name(), get_primary_key()$name, record_picker()$id
            ))
          record_picker()$update()
        }, error = \(e) {
          shinyWidgets::show_alert( "Failed to Delete Record",
                                    strip_ansi(e$message), "error" )
        })
      })

      output$record_details <- shiny::renderUI({
        rec <- get_record()
        fk <- get_foreign_keys()
        ## TODO make sure that the primary key is disabled. User should be able to change it
        widgets <-
          lapply(names(rec), \(nm) {
            label <-
              nm |>
              stringr::str_replace_all("_", " ") |>
              stringr::str_to_title()
            if (nm %in% fk[["from"]]) {
              lookupUI(ns(nm), label)
            } else {
              switch(
                typeof(rec[[nm]]),
                integer = shiny::numericInput(
                  ns(nm), label,
                  ifelse(length(rec[[nm]]) == 0, NA_integer_, rec[[nm]]), step = 1L),
                character = shiny::textInput(
                  ns(nm), label, ifelse(length(rec[[nm]]) == 0, NA_character_, rec[[nm]])),
                NULL
              )
            }
          }) |>
          stats::setNames(names(rec))
        
        pk <- get_primary_key()$name
        ## User should not be allowed to edit primary key
        widgets[[pk]] <- shinyjs::disabled( widgets[[pk]] )
        widgets <- unname(widgets)
        widgets[["col_widths"]] <- c(3, 3, 3, 3)

        do.call(bslib::layout_columns, widgets) |> suppressWarnings()
      })
      
      get_record_to_foreign <- shiny::reactive({
        fk <- get_foreign_keys()
        rec <- get_record()
        rename_map <- stats::setNames(fk$from, fk$to)
        if (length(rename_map) == 0) rec else
          rec |>
            dplyr::select(dplyr::any_of(rename_map))
      })
      
      shiny::observe({
        svs <- lookup_servers()
        rec <- get_record()
        fk <- get_foreign_keys()
        
        new_fks <- fk$from[!fk$from %in% names(svs)]
        new_servers <- lapply(new_fks, \(nm) {
          fk_cur <- fk |>
            dplyr::filter(.data$from == !!nm)
          lookupServer(
            nm, nm, shiny::reactive({ tournaments()$database }),
            get_record_to_foreign,
            fk_cur$table, fk_cur$to, validator = validator)
        })
        names(new_servers) <- new_fks
        svs <- modifyList(svs, new_servers)
        lookup_servers(svs)
      })
      
      shiny::observe({
        svs <- lookup_servers()
        for (i in seq_along(svs)) {
          lookup <- svs[[i]]()
        }
      })
      
      result <- shiny::reactive({
        list(
          primary_key = get_primary_key(),
          new_created = new_key_cache()
        )
      })
      return( result )
    }
  )
}