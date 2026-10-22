tableDetailsUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("table"),
    bslib::nav_panel(
      "Details",
      shiny::uiOutput(ns("details"))
    ),
    bslib::nav_panel(
      "Contents",
      shiny::uiOutput(ns("contents"))
    )
  )
}

tableDetailsServer <- function(id, database, selected) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      get_content <- shiny::reactive({
        tab_name <- selected()$arg$title
        if (!is.null(tab_name)) {
          con <- database()$database$connect()
          on.exit({ RSQLite::dbDisconnect(con) }, add = TRUE)
          dplyr::tbl(con, tab_name) |>
            dplyr::collect()
        } else {
          data.frame()
        }
      })
      
      get_meta <- shiny::reactive({
        tab_name <- selected()$arg$title
        if (!is.null(tab_name)) {
          foosball3_meta_data() |>
            dplyr::filter(.data$table == !!tab_name)
        } else {
          NULL
        }
      })
      
      output$details <- shiny::renderUI({
        met <- get_meta()
        if (!is.null(met)) {
          description <-
            met |>
            dplyr::filter(.data$field_name == "CREATE") |>
            dplyr::pull("description")
          shiny::tagList(
            shiny::p(description),
            met |>
              dplyr::filter(.data$field_name != "CREATE") |>
              dplyr::select(dplyr::any_of(c("field_name", "description"))) |>
              dplyr::mutate(description = ifelse(is.na(.data$description),
                                                 "No description available",
                                                 .data$description)) |>
              knitr::kable(format = "html") |>
              shiny::HTML()
          )
        }
      })
      
      output$contents <-  shiny::renderUI({
        ct <- get_content()
        DT::datatable(ct)
      })
    }
  )
}