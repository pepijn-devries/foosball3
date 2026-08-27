db_file <- tempfile(fileext = ".sqlite")
foosball3::foosball3_create_db(db_file)

dbManagerUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(3, 9),
    bslib::card(
      bslib::card_header( "Database menu" ),
      bslib::card_body(
        shiny::fileInput(ns("uploadSQLite"), "Upload SQLite"),
        shiny::downloadButton(ns("downloadSQLite"))
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::uiOutput(ns("header"))),
      bslib::card_body(
        bslib::navset_hidden(
          id = ns("db_set"),
          bslib::nav_panel_hidden(
            "Diagram",
            fillable = FALSE,
            diagramUI(ns("mod_diagram"))
          ),
          bslib::nav_panel_hidden(
            "Table Details",
            tableDetailsUI(ns("mod_details"))
          )
        )
      )
    )
  )
}

dbManagerServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      menu_selector <- shiny::reactiveVal()
      
      db_path <- shiny::reactiveVal( db_file )
      
      shiny::observeEvent(input$uploadSQLite, {
        bslib::show_toast(
          bslib::toast(
            "Please wait while importing data from uploaded file",
            div(
              class = "spinner-border text-primary flex-shrink-0", 
              role = "status",
              style = "width: 1.5rem; height: 1.5rem; border-width: 0.2em;",
              tags$span(class = "visually-hidden", "Loading...")
            ),
            id = ns("import_toast"),
            duration_s = NA,
            position = "bottom-right"
          )
        )
        pt <- db_path()
        new_path <- tempfile(fileext = ".sqlite")
        tryCatch({
          withCallingHandlers({
            foosball3::foosball3_import_db(
              input$uploadSQLite$datapath,
              new_path
            )
            db_path(new_path)
            unlink(pt, TRUE, TRUE)
          }, warning = \(w) {
            shinyWidgets::show_alert(
              "Please be aware of the following",
              paste(w$message, collapse = " "),
              type = "warning")
            invokeRestart("muffleWarning")
          })
        }, error = \(e) {
          shinyWidgets::show_alert(
            "Import failed",
            paste(e$message, collapse = " "),
            type = "error")
        })
        bslib::hide_toast(ns("import_toast"))
      })
      
      output$downloadSQLite <- shiny::downloadHandler(
        \() {
          sprintf("foosball %s.sqlite", Sys.time())
        },
        \(filename) {
          file.copy(database()$path, filename)
        }
      )

      database <- shiny::reactive({
        shiny::req(db_path())
        list(
          path    = db_path(),
          connect = \() RSQLite::dbConnect(
            RSQLite::SQLite(), db_path())
        )
      })
 
      men_sel <- function(item, arg, ts) {
        menu_selector(list(item = item, arg = arg))
      }
      
      sub_mod_react <- shiny::reactive({
        list(database      = database(),
             menu_selector = men_sel)
      })
      
      mod_diagram <- diagramServer("mod_diagram", sub_mod_react)
      mod_details <- tableDetailsServer("mod_details", sub_mod_react, menu_selector)
      
      observeEvent(menu_selector(), {
        bslib::nav_select("db_set", menu_selector()$item)
      })
      
      output$header <- shiny::renderUI({
        input$db_set
      })
      
      return(database)
    }
  )
}