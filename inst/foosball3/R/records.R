editable_tables <-
  c("balls", "locations", "side_properties", "tables")

recordsUI <- function(id) {
  ns <- shiny::NS(id)
  rec_ui <- lapply(editable_tables, \(tab) {
    bslib::nav_panel(
      title = tab,
      recordUI(ns(sprintf("mod_%s", tab)), tab)
    )
  })
  sel <- editable_tables[1]
  shiny::tagList(
    bslib::card(
      full_screen = TRUE,
      bslib::card_body(
        bslib::layout_columns(
          fill = FALSE,
          col_widths = c(4, 4, 4),
          shiny::uiOutput(ns("uiTableDetails")),
          shiny::selectizeInput( ns("selectRecord"), "Pick Record", character() ),
          shiny::div( )
        ),
        bslib::navset_card_tab( id = ns("nav_recs"), selected = sel, !!!rec_ui )
      )
    )
  )
}

recordsServer <- function(id, tournaments) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      options_cache  <- shiny::reactiveVal()
      new_record_id  <- shiny::reactiveVal()

      record_picker <- shiny::reactive({
        list(
          id = input$selectRecord,
          update = function(val) {
            if (!missing(val) && !is.null(val)) {
              new_record_id(val)
            }
            tournaments()$trigger_refresh() #TODO
          }
        )
      })
      
      ## Initiate recordServers
      rec_servers <- lapply(editable_tables, \(tab) {
        recordServer(
          sprintf("mod_%s", tab), tournaments, record_picker, tab )
      }) |>
        stats::setNames(editable_tables)

      shiny::observe({
        shiny::req(input$nav_recs)
        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con) }, add = TRUE)
        lazy_table <- dplyr::tbl(con, input$nav_recs)
        nms <- colnames(lazy_table)
        pk <- get_primary_key_global(input$nav_recs)
        descript <- get_description_field(pk, nms)
        opts <- structure(
          lazy_table |> dplyr::pull(pk),
          names = lazy_table |> dplyr::pull(descript),
          class = "list"
        ) |> as.list()
        if (!identical(options_cache(), opts)) {
          options_cache(opts)
        }
      })

      shiny::observe({ #TODO
        new_id  <- shiny::isolate(new_record_id())
        current <- shiny::isolate(input$selectRecord)
        opts <- options_cache()
        
        if (!is.null(new_id)) {
          current <- as.character(new_id)
          new_record_id(NULL) 
        } else if (!is.null(current) && !current %in% opts) {
          current <- NA_character_
        }
        
        shiny::updateSelectizeInput(
          inputId = "selectRecord",
          choices = opts,
          selected = current
        )
      })
      
      observe({
        for (nm in names(rec_servers)) {
          rec_servers[[nm]]() #TODO for now just observing all input widgets
        }
      })
      
      output$uiTableDetails <- shiny::renderUI({
        foosball_meta |>
          dplyr::filter(.data$table == input$nav_recs &
                          .data$field_name == "CREATE") |>
          dplyr::pull("description")
      })
      
      return( shiny::reactive({ }) )
    }
  )
}