finalGeneratorUI <- function(id, type) {
  ns <- shiny::NS(id)
  shiny::tagList(
    tableArrangementUI(ns("mod_table")),
    "TODO"
  )
}

finalGeneratorServer <- function(id, type, matches, btnStart, avatars, phases) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      get_match_id <- shiny::reactive({
        matches()$matches$MATCH_ID
      })
      
      shiny::observe({
        get_match_id()#TODO
      })
      
      mod_table <- tableArrangementServer(
        "mod_table", shiny::reactive({ matches()$tournament }), avatars)
      
      shiny::observe({mod_table()}) #TODO
      
      return(shiny::reactive({}))
    }
  )
}