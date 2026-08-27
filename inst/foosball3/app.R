`!!` <- rlang::`!!`
`:=` <- rlang::`:=`
.data <- rlang::`.data`

ui <- bslib::page_navbar(
  title = "Foosball 3.0",
  
  header = shiny::tagList(
    newsUI("mod_news"),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(script    = "js/shinyjs-extra.js",
                           functions = c("updateSideStyle",
                                         "progressbar",
                                         "speech_supported",
                                         "announce")),
    tags$head(
      shiny::includeCSS("www/custom.css"),
      shiny::tags$script(src = "js/extra.js")
    ),
    tagList(
      shinybusy::add_busy_gif("img/banana.gif", 100, "top-right",
                              width = "33px", height = "35px")
    )
  ),
  selected = "Database Manager",
  bslib::nav_panel("Warming Up",       warmingUpUI(),
                   icon = bsicons::bs_icon("cup-hot-fill")),
  bslib::nav_panel("Database Manager", dbManagerUI("mod_db"),
                   icon = bsicons::bs_icon("database")),
  bslib::nav_panel("Tournaments",      tournamentUI("mod_tournament"),
                   icon = bsicons::bs_icon("trophy-fill")),
  bslib::nav_panel("Matches",          matchesUI("mod_matches"),
                   icon = bsicons::bs_icon("hammer")),
  bslib::nav_panel("People",           peopleUI("mod_peops"),
                   icon = bsicons::bs_icon("people-fill")),
  bslib::nav_panel("About",            "TODO",
                   icon = bsicons::bs_icon("info-circle-fill")),
  bslib::nav_spacer(),
  bslib::nav_item(
    shinyWidgets::materialSwitch(
      "checkTimer",
      shiny::span(bsicons::bs_icon("stopwatch-fill"), "Timer"),
      right = TRUE, width = "120px")
  ),
  bslib::nav_item(
    shinyWidgets::materialSwitch(
      "checkCoin",
      shiny::span(bsicons::bs_icon("coin"), "Coin"),
      right = TRUE, width = "120px")
  ),
  bslib::nav_item(
    shinyWidgets::materialSwitch(
      "checkNews",
      shiny::span(bsicons::bs_icon("newspaper"), "News"),
      TRUE, right = TRUE, width = "120px")
  ),
  bslib::nav_item(
    shinyWidgets::materialSwitch(
      "checkLight",
      shiny::span(bsicons::bs_icon("lightbulb"), "Light"),
      TRUE, right = TRUE, width = "120px")
  ),
  footer = tagList(timerUI("mod_timer"), coinUI("mod_coin"))
)

server <- function(input, output, session) {
  mod_db         <-  dbManagerServer("mod_db")
  mod_avatar     <-     avatarServer("mod_avatar",     mod_db)
  mod_tournament <- tournamentServer("mod_tournament", mod_db, mod_avatar)
  mod_matches    <-    matchesServer("mod_matches",    mod_tournament, mod_avatar)
  mod_news       <-       newsServer("mod_news",       mod_matches, mod_avatar,
                                     shiny::reactive({ input$checkNews }))
  mod_timer      <-      timerServer("mod_timer",      mod_tournament, mod_matches,
                                     shiny::reactive({ input$checkTimer }))
  mod_coin       <-       coinServer("mod_coin",       shiny::reactive({ input$checkCoin }))
  mod_peops      <-     peopleServer("mod_peops",      mod_tournament, mod_avatar)
  
  shiny::observe({
    bslib::toggle_dark_mode(
      ifelse(input$checkLight, "light", "dark")
    )
  })
  
  shiny::observeEvent(mod_tournament()$face_click, {
    mod_tournament()$face_click #TODO
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

}

shiny::shinyApp(ui, server)