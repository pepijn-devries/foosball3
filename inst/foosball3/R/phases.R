phasesUI <- function(id) {
  ns <- shiny::NS(id)
  div(
    class = "d-flex align-items-center gap-5",
    shiny::selectInput(
      ns("selectPhase"), "Tournament Phase",
      character()),
    shiny::actionButton(ns("btnFlow"), bsicons::bs_icon("diagram-3"))
  )
}

phasesServer <- function(id, tournaments) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      phase_options <- shiny::reactiveVal()

      get_phases <- shiny::reactive({
        tnmt <- tournaments()
        tnmt_type <- tnmt$selected$TOURNAMENT_TYPE_CODE
        if (length(tnmt_type) == 0) {
          data.frame()
        } else {
          con <- tnmt$database$connect()
          on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
          dplyr::tbl(con, "tournament_phase_flow") |>
            dplyr::filter(.data$TOURNAMENT_TYPE_CODE == !!tnmt_type) |>
            dplyr::left_join(
              dplyr::tbl(con, "tournament_phases"),
              by = "TOURNAMENT_PHASE_CODE"
            ) |>
            dplyr::collect() |>
            dplyr::arrange(.data$PHASE_ORDER, .data$IS_OPTIONAL)
        }
      })
      
      get_phase_options <- shiny::reactive({
        phs <- get_phases()
        if (nrow(phs) > 0) {
          phs$TOURNAMENT_PHASE
        } else character()
      })
      
      shiny::observe({
        opts <- get_phase_options()
        if (!identical(opts, phase_options())) {
          phase_options(opts)
          current <- input$selectPhase
          if (!current %in% opts) current <- ""
          if (current == "" && nrow(get_phases()) > 0) {
            current <- get_phases() |>
              dplyr::filter( !.data$IS_OPTIONAL ) |>
              dplyr::first() |>
              dplyr::pull("TOURNAMENT_PHASE")
          }
          if (is.na(current)) current <- ""
          shiny::updateSelectInput(
            inputId = "selectPhase",
            choices = opts,
            selected = current
          )
        }
      })

      get_diagram <- shiny::reactive({
        phs <- get_phases()
        if (nrow(phs) == 0) return(NULL)
        edges <- dplyr::tibble(
          from = seq_len(nrow(phs)),
          to = outer(phs$PHASE_ORDER, phs$PHASE_ORDER - 1L, `==`) |>
            apply(1, which)
        ) |>
          tidyr::unnest(to) |>
          dplyr::mutate(
            phase = .env$phs$TOURNAMENT_PHASE[.data$from]
          ) |>
          dplyr::left_join(
            phs |> dplyr::select(phase = "TOURNAMENT_PHASE", "IS_OPTIONAL"),
            by = "phase"
          ) |>
          dplyr::filter(!.data$IS_OPTIONAL)
        
        DiagrammeR::create_graph(
          nodes_df = 
            DiagrammeR::create_node_df(
              n = nrow(phs),
              type  = phs$IS_OPTIONAL,
              label = sprintf(
                "%s%s",
                phs$TOURNAMENT_PHASE,
                ifelse(phs$IS_OPTIONAL, " *", "")),
              rank  = phs$PHASE_ORDER,
              shape     = "box",
              fixedsize = FALSE
            ),
          edges_df = edges |>
            DiagrammeR::create_edge_df(
              from = edges$from,
              to = edges$to
            )
        ) |>
          DiagrammeR::add_global_graph_attrs(
            attr      = "layout",
            value     = "dot",
            attr_type = "graph"
          ) |>
          DiagrammeR::add_global_graph_attrs(
            attr      = "rankdir",
            value     = "LR",
            attr_type = "graph"
          ) |>
          DiagrammeR::render_graph() |>
          DiagrammeRsvg::export_svg() |>
          shiny::HTML()
      })
      
      shiny::observeEvent(input$btnFlow, {
        dg <- get_diagram()
        if (is.null(dg)) {
          shinyWidgets::show_alert(
            "Phase Flow",
            "Select a tournament first!",
            type = "warning"
          )
        } else {
          shiny::modalDialog(
            title = "Phase Flow",
            size = "xl",
            easyClose = TRUE,
            dg,
            "* Optional"
          ) |>
            shiny::showModal()
        }
      })

      match_states <- function(phase) {
        states <- list()
        if (length(phase) == 0 || is.na(phase) || phase == "") return(states)
        con <- tournaments()$database$connect()
        on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
        for (i in seq_len(length(phase))) {
          state <-
            dplyr::tbl(con, "matches_view") |>
            dplyr::filter(
              .data$TOURNAMENT_ID %in% !!tournaments()$selected$TOURNAMENT_ID &
                .data$TOURNAMENT_PHASE %in% !!phase[[i]]
            ) |>
            dplyr::collect() |>
            dplyr::summarise(
              N_PLAYED =
                sum(!is.na(.data$SCORE_1)) & all(!is.na(.data$SCORE_2)),
              MATCH_COMPLETE =
                all(!is.na(.data$SCORE_1)) & all(!is.na(.data$SCORE_2)),
              N = dplyr::n()
            )
          states[[i]] <- state
        }
        states
      }
      
      phase_completed <- function(phase) {
        st <- match_states(phase)
        if (length(st) == 0) return(TRUE)
        lapply(st, \(x) x$MATCH_COMPLETE) |> unlist() |> all()
      }

      current_completed <- shiny::reactive({
        ## Tests if all current phase is completed:
        ## There are matches registered in this phase
        ## And they all have results
        phase_completed(input$selectPhase)
      })
      
      previous_completed <- shiny::reactive({
        ## Tests if all previous required phases were completed:
        ## There are matches registered in this phase
        ## And they all have results
        phs <- get_phases()
        if (nrow(phs) == 0 || input$selectPhase == "") return(TRUE)
        current_order <-
          phs |>
          dplyr::filter(.data$TOURNAMENT_PHASE == !!input$selectPhase) |>
          dplyr::pull("PHASE_ORDER")
        previous_phases <-
          phs |>
          dplyr::filter(.data$PHASE_ORDER == (.env$current_order - 1L) &
                          !.data$IS_OPTIONAL)
        if (nrow(previous_phases) == 0) return(TRUE)
        phase_completed(previous_phases$TOURNAMENT_PHASE)
      })
      
      generator_message <- shiny::reactive({
        sel <- tournaments()$selected
        if (length(sel) == 0 || !("ACT" %in% sel$TOURNAMENT_STATE_CODE))
          return("Can only generate matches for active tournaments.")
        if (!current_completed())
          return("Matches are already generated for this phase.")
        if (!previous_completed())
          return("All required previous tournament phases have to be completed first.")
        NULL
      })
      
      return(shiny::reactive({
        list(
          selected            = input$selectPhase,
          completed           = current_completed(),
          previouse_completed = previous_completed(),
          message             = generator_message()
        )
      }))
    }
  )
}