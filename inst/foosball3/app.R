`!!` <- rlang::`!!`
`!!!` <- rlang::`!!!`
`:=` <- rlang::`:=`
.data <- rlang::`.data`

nav_switch <- function(id, icon_name, label, value = TRUE, right = TRUE, width = "150px") {
  bslib::nav_item(
    div(
      class = "px-3",
      onclick = "event.stopPropagation();",
      shinyWidgets::materialSwitch(
        id, shiny::span(bsicons::bs_icon(icon_name), label),
        value = value, right = right, width = width)
    )
  )
}

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
    shiny::tagList(
      shinybusy::add_busy_gif("img/banana.gif", 100, "top-right",
                              width = "33px", height = "35px")
    )
  ),
  selected = "Database Manager",
  bslib::nav_panel("Warming Up",       warmingUpUI(),
                   icon = bsicons::bs_icon("cup-hot-fill")),
  bslib::nav_panel("Database Manager", dbManagerUI("mod_db"),
                   icon = bsicons::bs_icon("database")),
  bslib::nav_panel("Records",          recordsUI("mod_recs"),
                   icon = bsicons::bs_icon("table")),
  bslib::nav_panel("Tournaments",      tournamentUI("mod_tournament"),
                   icon = bsicons::bs_icon("trophy-fill")),
  bslib::nav_panel("Matches",          matchesUI("mod_matches"),
                   icon = bsicons::bs_icon("hammer")),
  bslib::nav_panel("People",           peopleUI("mod_peops"),
                   icon = bsicons::bs_icon("people-fill")),
  bslib::nav_panel("About",            "TODO",
                   icon = bsicons::bs_icon("info-circle-fill")),
  bslib::nav_menu(
    bsicons::bs_icon("gear-fill"),
    nav_switch("checkTimer", "stopwatch-fill", "Timer", FALSE),
    nav_switch("checkCoin", "coin", "coin", FALSE),
    nav_switch("checkNews", "newspaper", "News"),
    nav_switch("checkLight", "lightbulb", "Light")
  ),
  footer = shiny::tagList(timerUI("mod_timer"), coinUI("mod_coin"))
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
  mod_recs       <-    recordsServer("mod_recs",       mod_tournament)
  
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