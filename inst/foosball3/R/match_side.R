matchSideUI <- function(id, side) { # Side is either 1L or 2L
  ns <- NS(id)
  opposite_id <- stringr::str_replace_all(
    id, "[12]", \(x) ifelse(x == "1", "2", "1") )
  inputnum <- shiny::numericInput(
    ns("numScore"), "Score", NA, 0L, 10L, 1L, width = "150px"
  )
  inputnum$children[[1]] <- NULL
  inputnum$children[[1]] <-
    inputnum$children[[1]] |>
    tagAppendAttributes(
      class = "foosball-score",
      onfocusout = sprintf("$(`#%s`).focus();", ns(opposite_id))
    )
  
  bslib::card(
    class = paste0("foosball-table-side", side),
    bslib::card_header(
      shiny::textOutput(ns("txtName"))
    ),
    bslib::card_body(
      class = paste0("foosball-table-side", side, "-body"),
      shiny::uiOutput(ns("players")),
      inputnum
    )
  )
}

matchSideServer <- function(id, match, side, avatars) {
  moduleServer(
    id,
    function(input, output, session) {

      validator <- shinyvalidate::InputValidator$new()
      validator$add_rule(
        "numScore",
        shinyvalidate::sv_between(0L, 10L, allow_na = TRUE))
      
      validator$add_rule(
        "numScore",
        shinyvalidate::sv_integer(allow_na = TRUE))
      
      validator$enable()
      
      output$txtName <- shiny::renderText({
        get_color_name()
      })
 
      shiny::observeEvent(get_color(), {
        shinyjs::js$updateSideStyle(
          color_bg = grDevices::adjustcolor(get_color(), alpha = 0.4),
          color_border = get_color(),
          side = side)
      })
      
      get_player_ids <- shiny::reactive({
        if (length(match()$selected_id) > 0) {
          con <- match()$tournament$database()$connect()
          on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
          dplyr::tbl(con, "match_players") |>
            dplyr::filter(.data$MATCH_ID == !!match()$selected_id &
              stringr::str_like(.data$POSITION_CODE, paste0("%", side))
            ) |>
            dplyr::left_join(
              dplyr::tbl(con, "participants") |>
                dplyr::select(dplyr::any_of(c("PARTICIPANT_ID", "PERSON_ID"))),
              by = "PARTICIPANT_ID"
            ) |>
            dplyr::collect()
        } else {
          return(NULL)
        }
      })
      
      get_color <- shiny::reactive({
        shiny::req(match())
        m <- match()
        col <-
          m$matches |>
          dplyr::filter(.data$MATCH_ID == m$selected_id) |>
          dplyr::pull(paste0("COLOR_RGB", side))
        ifelse(length(col) == 0, "#888888", col)
      })

      output$players <- shiny::renderUI({
        m <- get_selected_match()
        shiny::req(get_player_ids())
        pid <- get_player_ids()
        if (nrow(pid) != 2) {
          NULL
        } else {
          avt <- avatars()
          pid <- pid |>
            dplyr::mutate(
              avt = lapply(.data$PERSON_ID, avt$get_avatar,
                           what = "tile", side)
            )
          order_fun <- I
          if (side == 2) order_fun <- rev
          txt <-
            c(
              paste(
                order_fun(
                  c(
                    sprintf("Defense: %s", m[[paste0("PLAYER_DEFENSE_", side)]]),
                    pid$avt[[which(pid$POSITION_CODE == paste0("D", side))]]
                  )
                ),
                collapse = " "
              ),
              paste(
                order_fun(
                  c(
                    sprintf("Strike: %s", m[[paste0("PLAYER_STRIKE_", side)]]),
                    pid$avt[[which(pid$POSITION_CODE == paste0("S", side))]]
                  )
                ),
                collapse = " "
              )
            )
          if (side == 2L) txt <- rev(txt)
          txt <- txt |>
            lapply(shiny::HTML) |>
            lapply(shiny::h2) |>
            lapply(shiny::p)
          do.call(shiny::div, txt)
        }
      })
      
      get_selected_match <- shiny::reactive({
        m <- match()
        m$matches |>
          dplyr::filter(.data$MATCH_ID == m$selected_id)
      })
      
      get_color_name <- shiny::reactive({
        get_selected_match() |>
          dplyr::pull(paste0("COLOR_NAME", side))
      })
      
      get_tournament_state <- shiny::reactive({
        match()$tournament$selected()$TOURNAMENT_STATE
      })
      
      shiny::observeEvent(get_selected_match(), {
        m <- get_selected_match()
        state <- get_tournament_state()

        if (length(state) == 0 || is.na(state) || state == "ARC") {
          shinyjs::disable("numScore")
        } else {
          shinyjs::enable("numScore")
        }

        shiny::updateNumericInput(
          inputId = "numScore",
          value = m[[paste0("SCORE_", side)]]
        )
      })
      
      shiny::observeEvent(input$numScore, {
        m <- get_selected_match()
        state <- get_tournament_state()
        shiny::req(m)
        if (state == "ACT" &&
            m[[paste0("SCORE_", side)]] != input$numScore) {
          browser() #TODO check validity + tournament state and store in database
          #TODO store updated scores in database
        }
      })
      
      return(
        shiny::reactive({ get_color() }) #TODO
      )
    }
  )
}