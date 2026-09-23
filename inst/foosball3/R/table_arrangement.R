tableArrangementUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    lookupUI(ns("mod_table"), "Table", "On which the matches are played"),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(
          shiny::uiOutput(ns("lab1"))
        ),
        bslib::card_body(
          id = ns("side1"),
          personPickerUI(ns("mod_d1"), "Defense"),
          personPickerUI(ns("mod_s1"), "Strike")
        )
      ),
      bslib::card(
        bslib::card_header(
          shiny::uiOutput(ns("lab2"))
        ),
        bslib::card_body(
          id = ns("side2"),
          personPickerUI(ns("mod_s2"), "Strike"),
          personPickerUI(ns("mod_d2"), "Defense")
        )
      )
    )
  )
}

tableArrangementServer <- function(id, tournaments, avatars, person_filter,
                                   required_players = \() NULL,
                                   teams = \() NULL, restrict_teams = TRUE) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      validator <- NULL #TODO
      mod_table <- lookupServer(
        "mod_table", "Table", tournaments, "tables", "%s", validator)
      
      get_color_tab <- shiny::reactive({
        tab <- mod_table()
        if (!is.null(tab$id) && tab$id != "") {
          con <- tournaments()$database$connect()
          on.exit( { RSQLite::dbDisconnect(con) }, add = TRUE)
          dplyr::tbl(con, "side_properties") |>
            dplyr::filter(.data$TABLE_CODE %in% !!tab$id) |>
            dplyr::arrange(.data$SIDE_ID) |>
            dplyr::collect()
        } else {
          data.frame(
            COLOR_NAME = c("Side 1", "Side2"),
            COLOR_RGB  = c("#FFFFFF", "#000000")
          )
        }
      })
      
      shiny::observe({
        tm <- teams() #TODO
        if (!is.null(tm)) browser()
      })
      
      shiny::observe({
        cols <- get_color_tab()[["COLOR_RGB"]] |>
          grDevices::adjustcolor(alpha = 0.4)
        js_code <- "document.getElementById('%s').style.setProperty('background-color', '%s', 'important');"
        shinyjs::runjs(sprintf(js_code, ns("side1"), cols[[1]]))
        shinyjs::runjs(sprintf(js_code, ns("side2"), cols[[2]]))
      })
      
      output$lab1 <- shiny::renderUI({
        get_color_tab()[["COLOR_NAME"]][[1]]
      })
      
      output$lab2 <- shiny::renderUI({
        get_color_tab()[["COLOR_NAME"]][[2]]
      })
      
      mod_d1 <-
        personPickerServer(
          "mod_d1", tournaments, \() NULL, avatars, NULL, 1L, TRUE, person_filter)
      mod_s1 <-
        personPickerServer(
          "mod_s1", tournaments, \() NULL, avatars, NULL, 1L, TRUE, person_filter)
      mod_d2 <-
        personPickerServer(
          "mod_d2", tournaments, \() NULL, avatars, NULL, 1L, TRUE, person_filter)
      mod_s2 <-
        personPickerServer(
          "mod_s2", tournaments, \() NULL, avatars, NULL, 1L, TRUE, person_filter)
      
      return(shiny::reactive({}))
    }
  )
}