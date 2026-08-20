lookupUI <- function(id, label, placeholder = "") {
  ns <- shiny::NS(id)
  settings <- list(
    placeholder = placeholder,
    onInitialize = I("function() { this.setValue(\"\"); }")
  )
  settings[["onInitialize"]] <- NULL
  shiny::selectizeInput(
    ns("selectLookup"), label, character(),
    options = settings
  )
}

lookupServer <- function(id, label, db, initial, table, field_id, name, fmt = "%s", default = NA, validator = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      shiny::observeEvent(initial(), {
        init <- initial()[[field_id]]
        if (length(init) == 0 || is.na(init)) init <- default
        con <- db()$connect()
        on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
        look <- dplyr::tbl(con, table) |>
          dplyr::select(dplyr::any_of(c(field_id, name))) |>
          dplyr::collect()
        opts <-
          structure(look[[field_id]],
                    names = do.call(
                      sprintf, c(list(fmt = fmt), as.list(look[, name]) |> unname())))

        shiny::updateSelectizeInput(
          inputId = "selectLookup", choices = opts, selected = init
        )
      })
      
      if (!is.null(validator)) {
        validator$add_rule("selectLookup", shinyvalidate::sv_required(
          sprintf("'%s' is required!", label)))
      }

      return(
        shiny::reactive( input$selectLookup )
      )
    }
  )
}
