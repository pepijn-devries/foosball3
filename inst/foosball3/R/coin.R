coinUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    tags$audio(id = ns("audio_player"), src = "audio/coin-toss.mp3", type = "audio/mp3"),
    tags$head(
      tags$script(src = "js/coin.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "coin.css")
    ),
    shiny::absolutePanel(
      id = ns("coinPanel"),
      bottom = 20, left = 425, width = 400,
      draggable = TRUE,
      style = "z-index: 10000; display: none; background: transparent;",
      
      bslib::card(
        class = "foosball-drag-card",
        
        div(class = "coin-game-wrapper",
            div(class = "coin-viewport",
                div(id = ns("coin_object"), class = "coin-cylinder",
                    div(class = "face heads-face"),
                    div(class = "coin-edge",
                        div(class = "segment seg-1"),
                        div(class = "segment seg-2"),
                        div(class = "segment seg-3"),
                        div(class = "segment seg-4"),
                        div(class = "segment seg-5")
                    ),
                    div(class = "face tails-face")
                )
            ),
            
            div(class = "controls", style = "text-align: center;",
                actionButton(ns("btnFlip"), "Spin Coin", class = "btn btn-primary w-100")
            )
        )
      )
    )
  )
}

coinServer <- function(id, show) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observe({
      shinyjs::toggle("coinPanel", condition = show(), anim = TRUE,
                      time = 0.25, animType = "slide")
    })
    
    observeEvent(input$btnFlip, {

      rng_flip <- sample(c("Heads", "Tails"), 1)
      
      session$sendCustomMessage("animate_coin", list(
        coinId = ns("coin_object"),
        btnId = ns("flip_btn"),
        audioId = ns("audio_player"),
        outcome = rng_flip
      ))
    })
    
  })
}
