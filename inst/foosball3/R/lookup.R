lookupUI <- function(id, label, placeholder = "") { #TODO use this placeholder
  ns <- shiny::NS(id)
  settings <- list(
    placeholder = placeholder,
    allowEmptyOption = TRUE,
    plugins = list("remove_button")
  )
  shiny::selectizeInput( ns("selectLookup"), label, NULL, options = settings )
}

lookupServer <- function(id, label, tournaments, table, fmt = "%s", validator = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      choices_cache  <- shiny::reactiveVal()
      selected_cache <- shiny::reactiveVal()
      
      if (!is.null(validator)) {
        validator$add_rule("selectLookup", shinyvalidate::sv_required(
          sprintf("'%s' is required!", label)))
      }
      
      shiny::observeEvent(tournaments(), {
        con <- tournaments()$database$connect()
        on.exit({ RSQLite::dbDisconnect(con) }, add = TRUE)
        pk <- get_primary_key_global(table)
        lazy_table <- dplyr::tbl(con, table)
        descr <- get_description_field(pk, colnames(lazy_table))
        opts <- lazy_table |>
          dplyr::select(dplyr::any_of(c(pk, descr))) |>
          dplyr::collect()
        opts <- structure(
          as.character(opts[[pk]]),
          names = opts[[descr]]
        )
        if (!identical(opts, choices_cache())) {
          choices_cache(opts)
        }
      })
      
      # shiny::observeEvent(choices_cache(), { #TODO
      #   shiny::updateSelectizeInput(
      #     session = session,
      #     inputId = "selectLookup",
      #     choices = choices_cache()
      #   )
      # }, ignoreInit = FALSE, ignoreNULL = FALSE)
      # 
      # shiny::observeEvent(selected_cache(), {
      #   shiny::updateSelectizeInput(
      #     session = session,
      #     inputId = "selectLookup",
      #     selected = selected_cache()
      #   )
      # }, ignoreInit = TRUE)
      
      shiny::observe({
        opts <- choices_cache()
        sel  <- selected_cache()
        # If selection isn't valid for current options, fall back safely
        if (length(sel) > 0 && (length(opts) == 0 || !sel %in% opts)) {
          sel <- NULL
        }
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectLookup",
          choices = opts,
          selected = sel
        )
      })
      
      shiny::observeEvent(input$selectLookup, {
        req(choices_cache())
        current_input <- input$selectLookup
        
        if (identical(current_input, "")) current_input <- NULL 
        
        if (!identical(current_input, shiny::isolate(selected_cache()))) {
          selected_cache(current_input)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      update_fun <- function(val) {
        val_clean <- if (!is.null(val)) as.character(val) else NULL
        if (!identical(val_clean, shiny::isolate(selected_cache()))) {
          selected_cache(val_clean)
        }
      }
      
      result <- shiny::reactive({
        list(
          id = input$selectLookup,
          set_selected = update_fun
        )
      })
      
      return(
        result
      )
    }
  )
}
