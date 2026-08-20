library(rlang)

ui <- bslib::page_navbar(
  title = "Foosball 3.0",
  
  header = shiny::tagList(
    newsUI("mod_news"),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(script    = "js/shinyjs-extra.js",
                           functions = c("updateSideStyle",
                                         "updateNews")),
    tags$head(
      shiny::includeCSS("www/custom.css"),
      shiny::tags$script(src = "js/extra.js")
    ),
    avatarUI("mod_avatar")
  ),
  selected = "Database Manager",
  bslib::nav_panel("Warming Up",       warmingUpUI(),
                   icon = bsicons::bs_icon("fire")),
  bslib::nav_panel("Database Manager", dbManagerUI("mod_db"),
                   icon = bsicons::bs_icon("database")),
  bslib::nav_panel("Tournaments",      tournamentUI("mod_tournament"),
                   icon = bsicons::bs_icon("trophy-fill")),
  bslib::nav_panel("Matches",          matchesUI("mod_matches"),
                   icon = bsicons::bs_icon("hammer")),
  bslib::nav_panel("Stats",            "TODO",
                   icon = bsicons::bs_icon("graph-up-arrow")),
  bslib::nav_panel("People",           "TODO",
                   icon = bsicons::bs_icon("people-fill")),
  bslib::nav_panel("About",            "TODO",
                   icon = bsicons::bs_icon("info-circle-fill")),
  bslib::nav_spacer(),
  bslib::nav_item(
    bslib::input_dark_mode(id = "dark_mode_toggle")
  ),
  bslib::nav_item(
    shinybusy::add_busy_gif("img/banana.gif", 100, "top-right",
                            width = "33px", height = "35px")
  ),
  footer = timerUI("mod_timer")
)

server <- function(input, output, session) {
  mod_db         <-  dbManagerServer("mod_db")
  mod_avatar     <-     avatarServer("mod_avatar",     mod_db)
  mod_tournament <- tournamentServer("mod_tournament", mod_db, mod_avatar)
  mod_matches    <-    matchesServer("mod_matches",    mod_tournament, mod_avatar)
  mod_news       <-       newsServer("mod_news",       mod_matches, mod_avatar)
  mod_timer      <-      timerServer("mod_timer",      mod_tournament, mod_matches)

  shiny::observe({ mod_news(); mod_db(); mod_tournament(); mod_avatar();
    mod_matches(); mod_timer() })
}

shiny::shinyApp(ui, server)