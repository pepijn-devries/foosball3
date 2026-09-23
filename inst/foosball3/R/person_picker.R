personPickerUI <- function(id, ..., dropboxWrapper = "body") {
  ns <- shiny::NS(id)
  shinyWidgets::virtualSelectInput(
    ns("selectPeople"), ..., choices = character(0),
    caseInsensitiveMatching = TRUE,
    position = "bottom", noOfDisplayValues = 3,
    dropboxWrapper = dropboxWrapper, zIndex = 2000L,
    search = TRUE, html = TRUE, showValueAsTags = TRUE)
}

personPickerServer <- function(
    id, tournaments, init, avatars, validator, min_required = 0L,
    allow_new = FALSE, filter_id = \() NULL) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      ## Use caching to prevent unneeded updates
      new_peops           <- shiny::reactiveVal()
      select_cache        <- shiny::reactiveVal()

      if (!is.null(validator)) {
        validator$add_rule(
          "selectPeople", \(value) {
            if (length(value) < min_required) {
              return(sprintf("At least %i people is/are required",
                             min_required))
            }
            NULL
          }
        )
        
        validator$enable()
      }

      filter_unknown <- function(val) {
        if (length(val) == 0 || all(is.na(val)) || all(val == "")) return(NA)
        opts <- avatars()$options
        id_match   <- match(val, as.character(unname(opts)))
        name_match <- match(tolower(val),
                            tolower(attr(opts, "PERSON_NAME")))
        unknown_person <- stats::na.omit(c(id_match, name_match))
      }
      
      get_selected_peop <- shiny::reactive({
        input$selectPeople
      })

      shiny::observeEvent(new_peops(), {
        np <- new_peops()

        if (allow_new && !is.null(np) && np != "") {
          con <- tournaments()$database$connect()
          on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
          new_row <-
            dplyr::tbl(con, "persons") |>
            dplyr::collect() |>
            dplyr::add_row(PERSON_ID = 0L) |>
            dplyr::summarise(
              PERSON_ID = max(.data$PERSON_ID) + 1L,
              PERSON_NAME = np,
              GENDER_CODE = "NS",
              QUALIFICATION_CODE = "H",
              HOME_BASE = NA_character_
            )
          dplyr::copy_to(
            con, new_row, "persons", append = TRUE
          )
          new_peops(NULL)
          avatars()$refresh_options()
          tournaments()$trigger_refresh()
        }
      })
      
      add_fun <- function(val) {
        if (length(val) == 0 || all(val == "")) return()
        
        opts <- avatars()$options
        id_match   <- val %in% as.character(unname(opts))
        name_match <- tolower(val) %in% tolower(attr(opts, "PERSON_NAME"))
        
        is_new <- !(id_match | name_match)
        new_vals <- val[is_new]
        
        if (length(new_vals) > 0) {
          new_peops(as.character(new_vals[1])) 
        }
      }

      get_options <- shiny::reactive({
        opts <- avatars()$options
        fid <- filter_id()
        if (!is.null(fid)) {
          filt <- opts %in% fid
          attr(opts, "PERSON_NAME") <- attr(opts, "PERSON_NAME")[filt]
          opts[filt]
        } else opts
      })
      
      shiny::observeEvent(get_options(), {
        opts <- get_options()
        sel <- shiny::isolate(select_cache())
        
        if (!is.null(sel)) {
          unsel <- sel[!sel %in% as.character(unname(opts))]
          sel <- sel[sel %in% as.character(unname(opts))]
          
          if (length(unsel) > 0) {
            new_sel <- unname(opts)[match(tolower(unsel), tolower(attr(opts, "PERSON_NAME")))] |>
              as.character()
            if (length(new_sel) > 0 && !any(is.na(new_sel))) {
              sel <- union(new_sel, sel)
              select_cache(sel)
            }
          }
        }
        
        current_ui_val <- shiny::isolate(input$selectPeople)
        if (!identical(current_ui_val, sel) || !is.null(opts)) {
          shinyWidgets::updateVirtualSelect(
            session = session,
            inputId = "selectPeople", 
            selected = sel, 
            choices = opts
          )
        }
      }, ignoreNULL = FALSE)
      
      shiny::observeEvent(input$selectPeople, {
        add_fun(input$selectPeople)
        select_cache(input$selectPeople)
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      select_fun <- function(val) {
        if (!identical(val, select_cache())) select_cache(val)
      }
      
      result <- shiny::reactive({
        list(
          add    = add_fun,
          select = select_fun,
          id     = get_selected_peop(),
          id_all = input$selectPeople
        )
      })
      
      return( result )
      
    })
}
