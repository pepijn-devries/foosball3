matchSideUI <- function(id, side) { # Side is either 1L or 2L
  ns <- NS(id)
  opposite_id <- stringr::str_replace_all(
    id, "[12]", \(x) ifelse(x == "1", "2", "1") )
  inputnum <- numericInput(
    ns("numScore"), "Score", NULL, 0L, 10L, 1L, width = "200px",
    updateOn = "blur"
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
      ## Use cache to ensure that UI is only updated when the data actually changes
      match_cache   <- shiny::reactiveVal()
      players_cache <- shiny::reactiveVal()
      avatar_cache  <- shiny::reactiveVal()

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
      
      shiny::observe({
        pid <-
          if (length(match()$selected_id) > 0) {
            con <- match()$tournament$database$connect()
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
        pc <- players_cache()
        if (!identical(pid, pc[, !(colnames(pc) %in% "avt")])) {
          if (nrow(pid) != 2) {
            NULL
          } else {
            avt <- avatars()
            # if (is.null(pid$avt)) {
            #   browser() #TODO
            # }
            pid <- pid |>
              dplyr::mutate(
                avt = lapply(.data$PERSON_ID, avt$get_avatar,
                             what = "tile", side = side, clickable = TRUE)
              )
          }

          players_cache(pid)
        }
      })
      
      get_color <- shiny::reactive({
        m <- match()
        col <- if (is.null(m$matches) || nrow(m$matches) == 0 || length(m$selected_id) == 0) {
          NULL
        } else {
          m$matches |>
            dplyr::filter(.data$MATCH_ID == m$selected_id) |>
            dplyr::pull(paste0("COLOR_RGB", side))
        }
        ifelse(length(col) == 0, "#888888", col)
      })

      output$players <- shiny::renderUI({
        m <- get_selected_match()
        shiny::req(players_cache())
        pid <- players_cache()
        if (nrow(pid) != 2) {
          NULL
        } else {
          order_fun <- I
          if (side == 2) order_fun <- rev
          txt <-
            c(
              paste(
                order_fun(
                  c(
                    sprintf("<strong>Defense</strong>: %s", m[[paste0("PLAYER_DEFENSE_", side)]]),
                    pid$avt[[which(pid$POSITION_CODE == paste0("D", side))]]
                  )
                ),
                collapse = " "
              ),
              paste(
                order_fun(
                  c(
                    sprintf("<strong>Strike</strong>: %s", m[[paste0("PLAYER_STRIKE_", side)]]),
                    pid$avt[[which(pid$POSITION_CODE == paste0("S", side))]]
                  )
                ),
                collapse = " "
              )
            )
          if (side == 2L) txt <- rev(txt)
          txt <- txt |>
            lapply(shiny::HTML) |>
            lapply(shiny::h2, style = "white-space: nowrap") |>
            lapply(shiny::p)
          do.call(shiny::div, txt)
        }
      })
      
      get_selected_match <- shiny::reactive({
        m <- match()
        if (is.null(m$matches) || nrow(m$matches) == 0) {
          NULL
        } else {
          m$matches |>
            dplyr::filter(.data$MATCH_ID == m$selected_id)
        }
      })
      
      get_color_name <- shiny::reactive({
        m <- get_selected_match()
        if (is.null(m) || nrow(m) == 0) {
          "Select a match"
        } else {
          dplyr::pull(m, paste0("COLOR_NAME", side))
        }
      })
      
      get_tournament_state <- shiny::reactive({
        match()$tournament$selected$TOURNAMENT_STATE
      })
      
      shiny::observeEvent(get_selected_match(), {
        m <- get_selected_match()
        m_cache <- match_cache()
        state <- get_tournament_state()
        if (!identical(m, m_cache)) {
          match_cache(m)
          if (length(state) == 0 || is.na(state) || state == "ARC") {
            shinyjs::disable("numScore")
          } else {
            shinyjs::enable("numScore")
          }
          
          shiny::updateNumericInput(
            inputId = "numScore",
            value = m[[paste0("SCORE_", side)]]
          )
        }
      })
      
      shiny::observeEvent(input$numScore, {
        m <- get_selected_match()
        state <- get_tournament_state()
        shiny::req(m)
        if (state == "ACT" &&
            !identical(m[[paste0("SCORE_", side)]], input$numScore) &&
            validator$is_valid()) {
          
          con <- match()$tournament$database$connect()
          on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
          RSQLite::dbExecute(
            con,
            "UPDATE match_results SET RESULT = ? WHERE MATCH_ID = ? AND SIDE_ID = ?",
            params = list(
              input$numScore,
              m$MATCH_ID,
              side
            )
          )
        }
      })
      
      return(
        shiny::reactive({ match_cache() })
      )
    }
  )
}