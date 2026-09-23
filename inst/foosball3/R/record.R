recordUI <- function(id, table, label = "Edit record") {
  ns <- shiny::NS(id)
  disab_cond <- function(x, cond) {
    if (cond) shinyjs::disabled(x) else (x)
  }
  widgets <-
    db_join_keys() |>
    dplyr::filter(.data$table == .env$table) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      widget = list({
        is_pk <- !is.na(.data$autoincrement)
        type <- get_type_global(table, .data$column)
        widget_name <- paste(table, .data$column, sep = "-")
        label <-
          .data$column |>
          stringr::str_replace_all("_", " ") |>
          stringr::str_to_title()
        
        if (!is.na(.data$parent_table)) {
          lookupUI(ns(widget_name), label, ifelse(is_pk, "Identifier", "Edit record")) |>
            disab_cond(is_pk)
        } else if (grepl("RGB", .data$column)) {
          shinyWidgets::colorPickr(ns(widget_name), label)
        } else {
          switch(
            type,
            INTEGER = {
              shiny::numericInput(ns(widget_name), label, NA_integer_, step = 1L)
            },
            TEXT = {
              shiny::textInput(ns(widget_name), label, "")
            },
            sprintf("Type '%s' not implemented", type)
          ) |>
            disab_cond(is_pk)
          
        }
      })
    )
  widgets <- unname(widgets$widget)
  cw <- c(3, 3, 3, 3)
  n_empty <- length(widgets) %% length(cw)
  if (n_empty > 0) {
    ## Add empty divs to satisfy layout
    widgets <- c(
      widgets,
      replicate(n_empty, shiny::div(), simplify = FALSE))
  }
  widgets <- c(widgets, list(
    fill = FALSE,
    col_widths = cw
  ))
  widgets <- do.call(bslib::layout_columns, widgets)
  
  opts <- list(
    animation = TRUE,
    delay = list(show = 750, hide = 100))
  
  shiny::tagList(
    bslib::toolbar(
      gap = "5px",
      bslib::tooltip(
        shiny::actionButton(ns("btnNew"), bsicons::bs_icon("plus-square-fill")),
        "Draft new record (will only be saved after clicking 'Save')",
        options = opts, placement = "auto"
      ),
      bslib::tooltip(
        shiny::actionButton(ns("btnSave"), bsicons::bs_icon("floppy2-fill")),
        "Save selected record in database",
        options = opts, placement = "auto"
      ),
      bslib::tooltip(
        shiny::actionButton(ns("btnDelete"), bsicons::bs_icon("trash3-fill")),
        "Delete selected record from database",
        options = opts, placement = "auto"
      )
    ),
    bslib::card(
      bslib::card_header(label),
      bslib::card_body(
        widgets
      )
    )
  )
}

recordServer <- function(id, tournaments, record_picker, table_name) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ## initiate lookupServers
      keys <-
        db_join_keys() |>
        dplyr::filter(.data$table == .env$table_name &
                        !is.na(.data$parent_table))
      lookups <-
        lapply(keys$column, \(col) {
          widget_name <- paste(table_name, col, sep = "-")
          lookupServer(
            widget_name, keys$parent_key_cols[keys$column == col],
            tournaments, keys$parent_table[keys$column == col])
        }) |>
        stats::setNames(keys$column)
      
      get_record <- shiny::reactive({
        shiny::req(record_picker())
        shiny::req(tournaments())
        rec_id <- record_picker()$id
        if (rec_id == "") return(NULL)

        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con)}, add = TRUE)
        pk <- get_primary_key_global(table_name)
        tp <- get_type_global(table_name, pk)
        if (length(tp) > 1) {
          rec_id <- strsplit(rec_id, "\\|") |> unlist()
        }
        rec_id <- mapply(\(rec_type, rid) {
          if (rec_type == "INTEGER") rid <- as.integer(rid) |>
              suppressWarnings()
          rid
        }, rec_type = tp, rid = rec_id, SIMPLIFY = FALSE)
        lazy_table <- dplyr::tbl(con, table_name)
        for (i in seq_along(pk)) {
          lazy_table <-
            dplyr::filter(lazy_table, !!rlang::sym(pk[[i]]) == !!rec_id[[i]])
        }
        lazy_table |> dplyr::collect()
      })

      shiny::observeEvent(get_record(), {
        rec <- get_record()
        for (field in colnames(rec)) {
          update_value(field, rec[[field]])
        }
      })
      
      shiny::observeEvent(input$btnNew, {
        rec <- get_record()
        for (field in colnames(rec)) {
          update_value(field, NA)
        }
      })

      get_new_pk <- shiny::reactive({
        pk <- get_primary_key_global(table_name)
        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con)}, add = TRUE)
        existing_keys <-
          dplyr::tbl(con, table_name) |>
          dplyr::pull(pk)
        nk <-
          switch(
            typeof(existing_keys),
            integer = {
              max(c(0L, existing_keys)) + 1L
            }, {
              lazy_table <- dplyr::tbl(con, table_name)
              cur_keys <- lazy_table |> dplyr::pull(pk)
              descr <- get_description_field(pk, colnames(lazy_table))
              new_key <- input[[paste(table_name, descr, sep = "-")]] |> toupper()
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
        nk
      })
      
      shiny::observeEvent(input$btnSave, {
        pk <- get_primary_key_global(table_name)
        fk <- db_static_fk |>
          dplyr::filter(.data$child_table == .env$table_name)
        is_new <- FALSE
        new_key <-
          lapply(pk, \(k) {
            nk <- input[[paste(table_name, k, sep = "-")]]
            is_lookup <- is.null(nk)
            if (is_lookup) nk <- lookups[[k]]()$id
            if (!is_lookup && (is.na(nk) || nk == "")) {
              nk <- get_new_pk()
              is_new <<- TRUE
            }
            nk
          }) |>
          unlist()
        if (length(new_key) == 0 || (length(new_key) > 1L && is_new)) {
          shinyWidgets::show_alert(
            "Cannot create new record for this table",
            type = "error")
          return()
        }
        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con)}, add = TRUE)
        lazy_tib <- dplyr::tbl(con, table_name)
        edited_row <-
          lazy_tib |>
          dplyr::filter(dplyr::row_number() == -1) |>
          dplyr::collect() |>
          dplyr::add_row()
        for (nm in colnames(edited_row)) {
          src <- names(input)[which(startsWith(names(input), paste(table_name, nm, sep = "-")))]
          src_field <- gsub(sprintf("^%s-|-selectLookup$", table_name), "", src)
          if (length(src) > 1) {
            sel <- xor(nm %in% fk$parent_key_cols, src_field == nm)
            src <- src[sel]
            src_field <- src_field[sel]
          }
          if (any(pk %in% src_field)) {
            val <- new_key[pk %in% src_field]
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
            by       = pk,
            in_place = TRUE,
            copy     = TRUE
          )
          create_crucial_foreigns(table_name, edited_row)
          if (length(pk) == 1L) record_picker()$add(edited_row[[pk]])
        }, error = \(e) {
          shinyWidgets::show_alert( "Failed to Save Record", strip_ansi(e$parent$message),
                                    "error" )
        })

      })

      create_crucial_foreigns <- function(table_name, edited_row) {
        
        switch(
          table_name,
          tables = {
            con <- tournaments()$database$connect()
            on.exit({ RSQLite::dbDisconnect(con)}, add = TRUE)
            lazy_tib <-
              dplyr::tbl(con, "side_properties")
            edited_row <-
              expand.grid(
                SIDE_ID = 1L:2L,
                TABLE_CODE = edited_row$TABLE_CODE
              ) |>
              dplyr::mutate(
                COLOR_NAME = c("Side 1", "Side 2"),
                COLOR_RGB = c("#FFFFFF", "#000000")
              )
            dplyr::rows_upsert(
              lazy_tib,
              edited_row,
              by       = c("SIDE_ID", "TABLE_CODE"),
              in_place = TRUE,
              copy     = TRUE
            )
          })
      }
      
      shiny::observeEvent(input$btnDelete, {
        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con)}, add = TRUE)
        tryCatch({
          RSQLite::dbExecute(
            con,
            sprintf("DELETE FROM %s WHERE %s = '%s'",
                    table_name, get_primary_key_global(table_name), record_picker()$id
            ))
          record_picker()$add(NA)
        }, error = \(e) {
          shinyWidgets::show_alert( "Failed to Delete Record",
                                    strip_ansi(e$message), "error" )
        })
      })

      update_value <- function(fields, val) {
        tp <- get_type_global(table_name, fields)
        fk <- db_join_keys() |>
          dplyr::filter(.data$table == !!table_name &
                          !is.na(.data$parent_table))
        widget_name <- paste(table_name, fields, sep = "-")
        mapply(\(key_type, wn, is_foreign, cl) {
          if (is_foreign) {
            lu <- lookups[[cl]]()
            if (!identical(as.character(val), lu$id)) {
              lu$set_selected(val)
            }
          } else if (grepl("RGB", cl)) {
            if (!identical(as.character(val), as.character(input[[wn]]))) {
              shinyWidgets::updateColorPickr(
                inputId = wn, value = val, session = session
              )
            }
          } else {
            if (!identical(as.character(val), as.character(input[[wn]]))) {
              switch(
                key_type,
                INTEGER = {
                  shiny::updateNumericInput(inputId = wn, value = val, session = session)
                },
                TEXT = {
                  shiny::updateTextInput(inputId = wn, value = val, session = session)
                }
              )
            }
          }
          
        }, key_type = tp, wn = widget_name, is_foreign = fields %in% fk$column, cl = fields)
        NULL
      }
      
      return( shiny::reactive({ }) )
    }
  )
}