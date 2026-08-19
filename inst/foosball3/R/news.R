newsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "foosball-news-flash",
    shiny::tags$div(
      id = ns("foosball-news"),
      class = "foosball-news-text",
      "News will appear here")
  )
}

newsServer <- function(id, matches, mod_avatar) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      news_tree <-
        readLines("www/newstree.json") |>
          jsonlite::fromJSON()

      get_news <- shiny::reactive({
        input$animation_finished
        news_tree #TODO
        shiny::tags$h3(
          "The news bar is currently not yet operational. Stay tuned..."
        ) |> as.character()
      })
      
      observeEvent(
        list(
          input$animation_finished,
          session$clientData$url_hostname), {
            session$sendCustomMessage(
              type = "scroll-next-foosball-news", 
              message = list(
                text_id = ns("foosball-news"),
                mod_id = id,
                duration = 10000,
                text = get_news())
            )
          }, ignoreInit = FALSE)
      
      shiny::observe({ get_news() })

      return( shiny::reactive({ "TODO" }))
    }
  )
}