news_tree     <- readLines("www/data/newstree.json") |>
  jsonlite::fromJSON(FALSE)
news_messages <-  read.csv("www/data/news.csv")

newsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "foosball-news-flash",
    id = ns("foosball-news-bar"),
    shiny::tags$div(
      id = ns("foosball-news"),
      class = "foosball-news-text",
      "News will appear here")
  )
}

newsServer <- function(id, matches, mod_avatar, show) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      shiny::observe({
        shinyjs::toggle(
          "foosball-news-bar",
          condition = show()
        )
      })
      
      all_input <- shiny::reactive({
        list(
          matches = matches,
          avatars = mod_avatar
        )
      })

      get_chance_fun <- function(fun_name) {
        switch(
          fun_name,
          length_gt = \(x, a, b) {
            ifelse(length(x) > a, b, 1 - b)
          },
          I
        )
      }
      
      get_param <- function(param_name) {
        inp <- all_input()
        m <- inp$matches()
        sel_tour <- m$tournament$selected
        
        switch(
          param_name,
          selected_tournament = sel_tour$TOURNAMENT_ID,
          NULL
        )
      }
      
      get_chance <- function(fun_name, fun_params) {
        fun <- get_chance_fun(fun_name)
        fun_params[[1]] <- get_param(fun_params[[1]])
        do.call(fun, fun_params)
      }
      
      get_news <- shiny::reactive({
        input$animation_finished # Trigger when animation has finished
        all_input() # Trigger when app state has changed
        walk_tree <- function(tree, what) {
          branches <- !grepl("^chance", names(tree))
          chances <- lapply(tree[branches], \(x) {
            if ("chance" %in% names(x)) {
              x$chance
              if (is.character(x$chance)) {
                get_chance(x$chance_fun, x$chance_params)
              } else {
                x$chance
              }
            } else {
              NULL
            }
          }) |>
            unlist()
          chances <- chances / sum(chances) ## Scale just to make sure
          chances <- cumsum(chances)
          if (length(chances) == 0) {
            msg_sub <-
              news_messages |>
              dplyr::filter( .data$type == what ) |>
              dplyr::slice_sample(n = 1) |>
              dplyr::pull("message")
            if (length(msg_sub) == 0) {
              "Something went wrong while generating news. Please report!"
            } else {
              msg_sub
            }
          } else {
            picked <-
              which(runif(1) < chances) |>
              utils::head(1)
            walk_tree(tree[branches][[picked]], names(picked))
          }
        }
        shiny::tags$h3(
          walk_tree(news_tree, names(news_tree))
        ) |> as.character()
      })
      
      shiny::observe({
        
        nws <- get_news()
        session$sendCustomMessage(
          type = "scroll-next-foosball-news", 
          message = list(
            text_id = ns("foosball-news"),
            mod_id = id,
            duration = 10000,
            text = nws)
        )

      })
      
      return( shiny::reactive({ "TODO" }))
    }
  )
}