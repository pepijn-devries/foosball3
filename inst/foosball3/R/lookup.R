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

lookupServer <- function(id, label, db, initial, table, field_id, name = NULL, fmt = "%s", default = NA, validator = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      lookup_state <- shiny::reactiveVal(list(opts = NULL, selected = NA))
      
      shiny::observe({
        shiny::req(initial())
        init <- initial()[[field_id]]
        if (length(init) == 0 || is.na(init)) init <- default
        if (is.na(init)) init <- input$selectLookup
        con <- db()$connect()
        on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
        look <- dplyr::tbl(con, table)

        if (is.null(name)) {
          name_pos <- which(colnames(look) == field_id) + 1L
          if (name_pos > ncol(look)) name_pos <- name_pos - 1L
          name <- colnames(look)[[name_pos]]
        }
        
        look <- look |>
          dplyr::select(dplyr::any_of(c(field_id, name))) |>
          dplyr::collect()
        opts <-
          structure(look[[field_id]],
                    names = do.call(
                      sprintf, c(list(fmt = fmt), as.list(look[, name]) |> unname())))
        ls <- lookup_state()
        if (!identical(ls$opts, opts) || !identical(ls$selected, init)) {
          ls$opts <- opts
          ls$select <- init
          lookup_state(ls)
        }
      })
      
      shiny::observe({
        ls <- lookup_state()
        shiny::updateSelectizeInput(
          inputId = "selectLookup", choices = ls$opts, selected = ls$sel
        )
      })
      
      if (!is.null(validator)) {
        validator$add_rule("selectLookup", shinyvalidate::sv_required(
          sprintf("'%s' is required!", label)))
      }

      return(
        shiny::reactive({ print(id); input$selectLookup }) #TODO remove print
      )
    }
  )
}
