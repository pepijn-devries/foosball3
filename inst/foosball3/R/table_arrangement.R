tableArrangementUI <- function(id) {
  ns <- shiny::NS(id)
  val_host <- shiny::textInput(ns("validation_host"), "", "", width = "100%")
  val_host$children <- val_host$children[-1]
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
    ),
    val_host,
    shinyjs::inlineCSS(sprintf("#%s { display:none !important; }",
                               ns("validation_host"))),
  )
}

tableArrangementServer <- function(id, tournaments, avatars, person_filter,
                                   required_players = \() NULL,
                                   teams = \() NULL, restrict_teams = TRUE,
                                   current_config = \() NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      config_cache <- shiny::reactiveVal()
      
      validator <- shinyvalidate::InputValidator$new()
      
      validator$add_rule("validation_host", ~{
        check <- all(required_players() %in% unlist(config()))
        if (!check) "All required players need to be placed"
      })
      
      if (restrict_teams) {
        validator$add_rule("validation_host", ~{
          pl <- config() |> unlist()
          tms <- teams()
          if (is.null(tms)) return(NULL)
          check <-
            (all(pl[c("d1", "s1")] %in% tms[[1]]) ||
               all(pl[c("d1", "s1")] %in% tms[[2]])) &&
            (all(pl[c("d2", "s2")] %in% tms[[1]]) ||
               all(pl[c("d2", "s2")] %in% tms[[2]]))
          
          if (!check) {
            sprintf("Teams restriction is not honourated. Team '%s' and '%s' should play on opposite sides",
                    names(tms)[1], names(tms)[2])
          }
        })
      }
      
      validator$add_rule("validation_host", ~{
        if (any(unlist(config()) == ""))
          "All positions should be assigned to a player"
      })
      
      validator$add_rule("validation_host", ~{
        pl <- config() |> unlist()
        check <- any(pl[c("d1", "s1")] %in% pl[c("d2", "s2")])
        if (check) {
          "The same person can not play on both sides of the table simultaneously"
        }
      })
      
      validator$enable()

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
          "mod_d1", tournaments, avatars, NULL, 1L, TRUE, person_filter)
      mod_s1 <-
        personPickerServer(
          "mod_s1", tournaments, avatars, NULL, 1L, TRUE, person_filter)
      mod_d2 <-
        personPickerServer(
          "mod_d2", tournaments, avatars, NULL, 1L, TRUE, person_filter)
      mod_s2 <-
        personPickerServer(
          "mod_s2", tournaments, avatars, NULL, 1L, TRUE, person_filter)
      
      shiny::observeEvent(config_cache(), {
        cc <- config_cache()
        get_pos <- \(pos_id) {
          result <-
            cc |>
            dplyr::filter(.data$POSITION_CODE == !!pos_id) |>
            dplyr::pull("PERSON_ID") |>
            as.character()
          if (length(result) == 0) result <- ""
          result
        }
        
        d1 <- mod_d1()
        d2 <- mod_d2()
        s1 <- mod_s1()
        s2 <- mod_s2()
        if (d1$id != get_pos("D1")) d1$select(get_pos("D1"))
        if (d2$id != get_pos("D2")) d2$select(get_pos("D2"))
        if (s1$id != get_pos("S1")) s1$select(get_pos("S1"))
        if (s2$id != get_pos("S2")) s2$select(get_pos("S2"))
      })
      
      shiny::observeEvent(person_filter(), {
        cc <- current_config() |>
          dplyr::mutate(
            POSITION_CODE = {
              pc <- .data$POSITION_CODE
              u1_count <- cumsum(pc == "U1")
              u2_count <- cumsum(pc == "U2")
              pc[pc == "U1" & u1_count == 1] <- "D1"
              pc[pc == "U1" & u1_count == 2] <- "S1"
              pc[pc == "U2" & u2_count == 1] <- "D2"
              pc[pc == "U2" & u2_count == 2] <- "S2"
              pc
            }
          )
        if (nrow(cc) > 0) {
          if (!identical(config_cache(), cc)) config_cache(cc)
        }
      })
      
      config <- shiny::reactive({
        list(
          d1 = mod_d1()$id,
          s1 = mod_s1()$id,
          d2 = mod_d2()$id,
          s2 = mod_s2()$id
        )
      })
      
      result <- shiny::reactive({
        c(
          config(),
          list(
            validate = validator$validate()
          )
        )
      })
      
      return(result)
    }
  )
}