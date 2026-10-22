coinUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$audio(id = ns("audio_player"), src = "foosball3/audio/coin-toss.mp3", type = "audio/mp3"),
    shiny::tags$head(
      shiny::tags$script(src = "foosball3/js/coin.js"),
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "foosball3/coin.css")
    ),
    shiny::absolutePanel(
      id = ns("coinPanel"),
      bottom = 20, left = 425, width = 400,
      draggable = TRUE,
      style = "z-index: 10000; display: none; background: transparent;",
      
      bslib::card(
        class = "foosball-drag-card",
        
        shiny::div(class = "coin-game-wrapper",
                   shiny::div(class = "coin-viewport",
                              shiny::div(id = ns("coin_object"), class = "coin-cylinder",
                                         shiny::div(class = "face heads-face"),
                                         shiny::div(class = "coin-edge",
                                                    shiny::div(class = "segment seg-1"),
                                                    shiny::div(class = "segment seg-2"),
                                                    shiny::div(class = "segment seg-3"),
                                                    shiny::div(class = "segment seg-4"),
                                                    shiny::div(class = "segment seg-5")
                                         ),
                                         shiny::div(class = "face tails-face")
                              )
                   ),
                   
                   shiny::div(class = "controls", style = "text-align: center;",
                              shiny::actionButton(ns("btnFlip"), "Spin Coin", class = "btn btn-primary w-100")
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
    
    shiny::observeEvent(input$btnFlip, {
      
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
