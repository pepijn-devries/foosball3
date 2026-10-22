#' Shiny App Server
#' @param input input controls for app
#' @param output output controls for app
#' @param session app session details
#' @include foosball3.R
#' @include mod_warming_up.R
#' @include mod_db_manager.R
#' @include mod_diagram.R
#' @include mod_table_details.R
#' @include mod_records.R
#' @include mod_record.R
#' @noRd
server <- function(input, output, session) {
  db_file <- file.path(tempdir(), "foosball.sqlite")
  if (!file.exists(db_file)) foosball3::foosball3_create_db(db_file)
  
  mod_db         <-  dbManagerServer("mod_db",         db_file)
  mod_avatar     <-     avatarServer("mod_avatar",     mod_db)
  mod_tournament <- tournamentServer("mod_tournament", mod_db, mod_avatar)
  mod_picture    <-    pictureServer("mod_picture",    mod_tournament, mod_avatar)
  mod_matches    <-    matchesServer("mod_matches",    mod_tournament, mod_avatar)
  mod_news       <-       newsServer("mod_news",       mod_matches, mod_avatar,
                                     shiny::reactive({ input$checkNews }))
  mod_timer      <-      timerServer("mod_timer",      mod_tournament, mod_matches,
                                     shiny::reactive({ input$checkTimer }))
  mod_coin       <-       coinServer("mod_coin",       shiny::reactive({ input$checkCoin }))
  mod_peops      <-     peopleServer("mod_peops",      mod_tournament, mod_avatar, mod_picture)
  mod_recs       <-    recordsServer("mod_recs",       mod_tournament)
  
  shiny::observe({
    bslib::toggle_dark_mode(
      ifelse(input$checkLight, "light", "dark")
    )
  })
  
  shiny::observeEvent(mod_picture(), {
    bslib::nav_select("nav_main", "People")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
}