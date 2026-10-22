#' @include mod_helpers.R
#' @include mod_tournament.R
#' @include mod_picture.R
#' @include mod_tournament_editor.R
#' @include mod_person_picker.R
#' @include mod_tournament_stats.R
#' @include mod_plot.R
#' @include mod_matches.R
#' @include mod_phases.R
#' @include mod_match.R
#' @include mod_match_side.R
#' @include mod_match_generator.R
#' @include mod_generator_qualification.R
#' @include mod_generator_finals.R
#' @include mod_table_arrangement.R
#' @include mod_penalty.R
#' @include mod_people.R
#' @include mod_person_stats.R
#' @noRd
ui <- function(id, appdir) {
  bslib::page_navbar(
    title = "Foosball 3.0",
    id = "nav_main",
    header = shiny::tagList(
      newsUI("mod_news"),
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(script    = "foosball3/js/shinyjs-extra.js",
                             functions = c("updateSideStyle",
                                           "progressbar",
                                           "speech_supported",
                                           "announce")),
      shiny::tags$head(
        shiny::includeCSS(file.path(appdir, "www", "custom.css")),
        shiny::tags$script(src = "foosball3/js/extra.js"),
        shiny::tags$link(rel = "shortcut icon",
                         href = "foosball3/favicon.ico")
      ),
      shiny::tagList(
        shinybusy::add_busy_gif("foosball3/img/banana.gif", 100, "top-right",
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
    bslib::nav_panel("Tournaments",      tournamentUI("mod_tournament", pictureUI("mod_picture")),
                     icon = bsicons::bs_icon("trophy-fill")),
    bslib::nav_panel("Matches",          matchesUI("mod_matches"),
                     icon = bsicons::bs_icon("hammer")),
    bslib::nav_panel("People",           peopleUI("mod_peops"),
                     icon = bsicons::bs_icon("people-fill")),
    bslib::nav_panel("About",
                     shiny::div(
                       shiny::img(src   = "foosball3/img/web-app-manifest-192x192.png",
                                  alt   = "logo",
                                  width = "192px",
                                  style = "float: right; margin-right: 15px;"),
                       shiny::h2("About Foosball3"),
                       shiny::h3("The History of the Foosball App"),
                       shiny::markdown(readLines(file.path(appdir, "www", "about.md")))
                     )),
    bslib::nav_menu(
      bsicons::bs_icon("gear-fill"),
      .nav_switch("checkTimer", "stopwatch-fill", "Timer", FALSE),
      .nav_switch("checkCoin", "coin", "coin", FALSE),
      .nav_switch("checkNews", "newspaper", "News"),
      .nav_switch("checkLight", "lightbulb", "Light")
    ),
    footer = shiny::tagList(timerUI("mod_timer"), coinUI("mod_coin"))
  )
}
