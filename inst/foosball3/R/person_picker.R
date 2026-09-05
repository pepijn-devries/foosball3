personPickerUI <- function(id, ...) {
  ns <- shiny::NS(id)
  shinyWidgets::virtualSelectInput(
    ns("selectPeople"), ..., choices = character(0),
    caseInsensitiveMatching = TRUE,
    position = "bottom", noOfDisplayValues = 3,
    dropboxWrapper = "card" , zIndex = 1000L,
    search = TRUE, html = TRUE, showValueAsTags = TRUE)
}

personPickerServer <- function(
    id, tournaments, init, avatars, validator, min_required = 0L) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      ## Use caching to prevent unneeded updates
      options_cache       <- shiny::reactiveVal()
      is_initialising     <- shiny::reactiveVal()

      get_people <-
        shiny::reactive({
          con <- tournaments()$database$connect()
          on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
          dplyr::tbl(con, "persons") |>
            dplyr::collect()
        })
      
      shiny::observeEvent(init(), {
        is_initialising(TRUE)
      })
      
      get_pre_options <- shiny::reactive({
        peops <- get_people()
        opts <- structure(peops$PERSON_ID, names = peops$PERSON_NAME)[
          order(peops$PERSON_NAME) ]
        avt <- avatars()
        img <- lapply(as.numeric(opts), \(x) {
          avt$get_avatar(x, what = "icon") %||% ""
        }) |>
          unlist()
        list(options = opts, img = img)
      })
      
      get_options <- shiny::reactive({
        opts <- get_pre_options()$options
        img <- get_pre_options()$img
        names(opts) <- sprintf("<span>%s %s</span>", img, names(opts))
        opts
      })
      
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
        
        validator$add_rule(
          "selectPeople", \(value) {
            peops <- get_people()$PERSON_ID
            if (length(value) > length(peops)) {
              if (grepl("[0-9]", value[[length(value)]], perl = TRUE)) {
                return("Names should not contain numerics")
              }
            }
            NULL
          }
        )
        
        validator$enable()
      }
      
      get_selected_peop <- reactive({
        peops <- get_people()
        current <- input$selectPeople
        id_match   <- match(input$selectPeople, as.character(peops$PERSON_ID))
        name_match <- match(tolower(input$selectPeople),
                            tolower(as.character(peops$PERSON_NAME)))
        new_peops <- input$selectPeople[is.na(name_match) & is.na(id_match)]
        is_valid <- is.null(validator) ||
          !any(grepl("numerics", validator$validate()[[ns("selectPeople")]]$message))
        if (length(new_peops) == 1 && is_valid && new_peops != "") {
          con <- tournaments()$database$connect()
          on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
          new_row <-
            dplyr::tbl(con, "persons") |>
            dplyr::collect() |>
            dplyr::add_row(PERSON_ID = 0L) |>
            dplyr::summarise(
              PERSON_ID = max(.data$PERSON_ID) + 1L,
              PERSON_NAME = new_peops,
              GENDER_CODE = "NS",
              QUALIFICATION_CODE = "H",
              HOME_BASE = NA_character_
            )
          id_match <- new_row$PERSON_ID
          dplyr::copy_to(
            con, new_row, "persons", append = TRUE
          )
          tournaments()$trigger_refresh()
        }
        na.omit(c(id_match, name_match))[1]
      })

      update <- shiny::reactive({
        peops <- get_people()
        
        if (is_initialising() %||% FALSE) {
          current <- init()
          is_initialising(FALSE)
        } else {
          current <- shiny::isolate(input$selectPeople)
        }
        
        match_names <- match(tolower(current), tolower(peops$PERSON_NAME))
        current[!is.na(match_names)] <- peops$PERSON_ID[na.omit(match_names)]
        current <- current[current %in% as.character(peops$PERSON_ID)]
        dup <- current[duplicated(current)]
        current <- current[!current %in% unique(dup)]
        
        actual_input <- as.character(shiny::isolate(input$selectPeople) %||% character(0))

        if (!identical(get_pre_options(), options_cache()$options) ||
            !identical(actual_input, unname(current))) {
          options_cache(get_pre_options())
          shinyWidgets::updateVirtualSelect(
            "selectPeople", choices = get_options(), selected = unname(current)
          )
        }
      })
      
      update_fun <- function(val) {
        #TODO
      }

      result <- shiny::reactive({
        list(
          update = update_fun,
          id = get_selected_peop(),
          id_all = input$selectPeople
        )
      })
      
      return( result )
      
    })
}
