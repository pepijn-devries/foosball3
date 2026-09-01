editable_tables <-
  c("balls", "locations", "tables")

recordsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectizeInput(
      ns("selectTable"),
      "Table to Edit",
      editable_tables,
      editable_tables[1]
    ),
    shiny::selectizeInput(
      ns("selectRecord"),
      "Pick Record",
      character()
    ),
    recordUI(ns("mod_rec"))
  )
}

recordsServer <- function(id, tournaments) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      options_cache <- shiny::reactiveVal()

      record_picker <- shiny::reactive({
        list(
          id = input$selectRecord,
          update = function() {
            tournaments()$trigger_refresh()
          }
        )
      })
      
      mod_rec <-
        recordServer("mod_rec", tournaments, record_picker,
                     shiny::reactive({
                       input$selectTable
                     }))
      
      shiny::observe({
        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con) }, add = TRUE)
        lazy_table <- dplyr::tbl(con, input$selectTable)
        nms <- colnames(lazy_table)
        pk <- mod_rec()$primary_key
        object <- stringr::str_replace_all(pk$name, "_ID$|_CODE", "")
        ## TODO check if this description field is correctly found for all editable tables
        descript <-
          nms[grepl(sprintf("(?=.*(NAME|DESCR))^%s", object), nms, perl = TRUE) &
                nms != pk$name][1]
        opts <- structure(
          lazy_table |> dplyr::pull(pk$name),
          names = lazy_table |> dplyr::pull(descript),
          class = "list"
        ) |> as.list()
        if (!identical(options_cache(), opts)) {
          options_cache(opts)
        }
      })
      
      shiny::observeEvent(options_cache(), {
        current <- input$selectRecord
        opts <- options_cache()
        if (!current %in% names(opts)) current <- NA_character_
        shiny::updateSelectizeInput(
          inputId = "selectRecord",
          choices = opts,
          selected = current
        )
      })

      return( shiny::reactive({ }) )
    }
  )
}