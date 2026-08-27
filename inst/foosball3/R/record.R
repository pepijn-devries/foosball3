recordUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    shiny::actionButton(ns("btnNew"), "New record"),
    shiny::uiOutput(ns("record_details"))
  )
}

recordServer <- function(id, tournaments, record_picker, table_name) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      lookup_servers <- shiny::reactiveVal(list())

      get_record <- shiny::reactive({
        rec_id <- record_picker()
        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con)}, add = TRUE)
        pk <- get_primary_key()
        dplyr::tbl(con, table_name()) |>
          dplyr::filter(!!pk == !!rec_id) |>
          dplyr::collect()
      })
      
      get_primary_key <- shiny::reactive({
        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con)}, add = TRUE)
        pk_info <-
          RSQLite::dbGetQuery(
            con,
            paste0("PRAGMA table_info(", table_name(), ");"))
        pk_info |> dplyr::filter(.data$pk == 1) |> dplyr::pull("name")
      })
      
      get_new_pk <- shiny::reactive({
        pk <- get_primary_key()
        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con)}, add = TRUE)
        existing_keys <-
          dplyr::tbl(con, table_name()) |>
          dplyr::pull(pk)
        switch(
          typeof(existing_keys),
          integer = {
            max(c(0L, existing_keys)) + 1L
          }, {
            browser() #TODO
          }
        )
      })

      get_foreign_keys <- shiny::reactive({
        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con)}, add = TRUE)
        RSQLite::dbGetQuery(
          con,
          paste0("PRAGMA foreign_key_list(", table_name(), ");"))
      })
      
      shiny::observeEvent(input$btnNew, {
        fk <- get_foreign_keys()
        new_key <- get_new_pk()
        new_record <-
          get_record()[0,] |>
          dplyr::add_row()
      })

      output$record_details <- shiny::renderUI({
        rec <- get_record()
        fk <- get_foreign_keys()
        widgets <-
          lapply(names(rec), \(nm) {
            if (nm %in% fk[["from"]]) {
              lookupUI(ns(nm), nm)
            } else {
              switch(
                typeof(rec[[nm]]),
                integer = shiny::numericInput(ns(nm), nm, NA_integer_, step = 1L),
                character = shiny::textInput(ns(nm), nm, NA_character_),
                NULL
              )
            }
          })
        do.call(tagList, widgets)
      })
      
      get_record_to_foreign <- shiny::reactive({
        fk <- get_foreign_keys()
        rec <- get_record()
        rename_map <- stats::setNames(fk$from, fk$to)
        rec |>
          dplyr::select(dplyr::any_of(rename_map))
      })
      
      shiny::observe({
        svs <- lookup_servers()
        rec <- get_record()
        fk <- get_foreign_keys()
        
        changed <- FALSE
        new_fks <- fk$from[!fk$from %in% names(svs)]
        new_servers <- lapply(new_fks, \(nm) {
          fk_cur <- fk |>
            dplyr::filter(.data$from == !!nm)
          lookupServer(
            nm, nm, shiny::reactive({ tournaments()$database }),
            get_record_to_foreign,
            fk_cur$table, fk_cur$to)
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
      
      return( shiny::reactive({ }) )
    }
  )
}