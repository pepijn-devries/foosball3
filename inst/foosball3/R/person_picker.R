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
      is_initialising <- shiny::reactiveVal()

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
      
      shiny::observeEvent(list(get_people(), avatars(), init()), {
        peops <- get_people()
        avt <- avatars()
        opts <- structure(peops$PERSON_ID, names = peops$PERSON_NAME)
        img <- lapply(as.numeric(opts), \(x) {
          avt$get_avatar(x, what = "icon") %||% ""
        }) |>
          unlist()
        names(opts) <- sprintf("<span>%s %s</span>", img, names(opts))
        
        if (is_initialising() %||% FALSE) {
          current <- init()
          is_initialising(FALSE)
        } else {
          current <- input$selectPeople
        }

        match_names <- match(tolower(current), tolower(peops$PERSON_NAME))
        current[!is.na(match_names)] <- peops$PERSON_ID[na.omit(match_names)]
        current <- current[current %in% as.character(peops$PERSON_ID)]
        dup <- current[duplicated(current)]
        current <- current[!current %in% unique(dup)]
        shinyWidgets::updateVirtualSelect(
          "selectPeople", choices = opts, selected = unname(current)
        )
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
      
      shiny::observeEvent(input$selectPeople, {
        shiny::req(input$selectPeople)

        peops <- get_people()
        current <- input$selectPeople
        id_match   <- match(input$selectPeople, as.character(peops$PERSON_ID))
        name_match <- match(tolower(input$selectPeople),
                            tolower(as.character(peops$PERSON_NAME)))
        new_peops <- input$selectPeople[is.na(name_match) & is.na(id_match)]
        is_valid <- is.null(validator) ||
          !any(grepl("numerics", validator$validate()[[ns("selectPeople")]]$message))
        if (length(new_peops) == 1 && is_valid) {
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
          dplyr::copy_to(
            con, new_row, "persons", append = TRUE
          )
          tournaments$trigger_refresh()
        }
      })
      
      return( shiny::reactive({ input$selectPeople }) )
      
    })
}