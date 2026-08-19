## Server that tracks changes to the persons table in the database
## It can also hand out person pickers to other modules
peopleServer <- function(id, tournaments) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      refresh_trigger <- shiny::reactiveVal(0L)
      
      get_people <-
        shiny::reactive({
          refresh_trigger()
          con <- tournaments()$database()$connect()
          on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
          dplyr::tbl(con, "persons") |>
            dplyr::collect()
        })
      
      trigger_refresh <- function() {
        refresh_trigger(refresh_trigger() + 1)
      }
      
      return( reactive({
        list(
          data = get_people(),
          refresh = trigger_refresh
        )
      }))
    }
  )
}