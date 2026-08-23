dbManagerUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(3, 9),
    bslib::card(
      shiny::fileInput(ns("uploadSQLite"), "Upload SQLite"),
      shiny::downloadButton(ns("downloadSQLite"))
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_body(
        fillable = FALSE,
        id = ns("diagram_body"),
        shiny::uiOutput(ns("diagram"))
      ),
      tags$script(HTML(sprintf("
        (function() {
          const bodyId = '%s';
          const inputId = '%s';
          const currentDate = new Date();

          // Wait until the element exists in DOM
          const checkExist = setInterval(function() {
            const container = document.getElementById(bodyId);
            if (container) {
              clearInterval(checkExist);
            
              container.addEventListener('click', function(event) {
                const nodeGroup = event.target.closest('.node');
                if (nodeGroup) {
                  const nodeTitle = nodeGroup.querySelector('title').textContent;
                  // Send value to the namespaced input ID
                  Shiny.setInputValue(inputId, {title: nodeTitle, stamp: currentDate.getTime()}, {priority: 'event'});
                }
              });
            }
          }, 100);
        })();
      ", ns("diagram_body"), ns("diagram_node"))))
    )
  )
}

dbManagerServer <- function(id) {
  db_file <- tempfile(fileext = ".sqlite")
  foosball3::foosball3_create_db(db_file)

  shiny::moduleServer(
    id,
    function(input, output, session) {
      db_path <- shiny::reactiveVal(
        list(
          path = db_file,
          update_counter = 0
        )
      )
      
      shiny::observeEvent(input$uploadSQLite, {
        pt <- db_path()
        temp_path <- tempfile()
        tryCatch({
          foosball3_import_db(
            input$uploadSQLite$datapath,
            temp_path
          )
          file.copy(temp_path, pt$path, overwrite = TRUE)
          pt$update_counter <- pt$update_counter + 1L
          db_path(pt)
          unlink(temp_path, TRUE, TRUE)
        }, error = \(e) {
          shinyWidgets::show_alert(
            "Import failed",
            paste(e$message, collapse = " "),
            type = "error"
          )
        })
      })
      
      output$downloadSQLite <- shiny::downloadHandler(
        \() {
          sprintf("foosball %s.sqlite", Sys.time())
        },
        \(filename) {
          file.copy(database()$path, filename)
        }
      )
      
      output$diagram <- shiny::renderUI({
        schem <- get_schema()
        dm::dm_draw(
          schem
        ) |>
          DiagrammeRsvg::export_svg() |>
          shiny::HTML()
      })
      
      get_schema <- shiny::reactive({
        con <- database()$connect()
        on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
        tbls <- RSQLite::dbListTables(con)
        
        my_dm <- dm::dm_from_con(con, learn_keys = TRUE)
        all_tbls <- RSQLite::dbListTables(con)
        unreferenced <- setdiff(all_tbls, names(my_dm))
        unreferenced <- unreferenced[!grepl("^sqlite_", unreferenced)]

        unreferenced <-
          lapply(unreferenced, dplyr::tbl, src = con) |>
          stats::setNames(unreferenced)
        for (i in seq_along(unreferenced)) {
          my_dm <-
            my_dm |>
            dm::dm(!!names(unreferenced)[[i]] := unreferenced[[i]])
        }
        my_dm
      })
      
      shiny::observeEvent(input$diagram_node, {
        con <- database()$connect()
        on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
        tab_name <- input$diagram_node$title
        tab <- dplyr::tbl(con, tab_name)
        shiny::modalDialog(
          title = sprintf("Contents of '%s'", tab_name),
          easyClose = TRUE,
          size = "l",
          bslib::card(
            full_screen = TRUE,
            bslib::card_body(
              DT::datatable(as.data.frame(tab))
            )
          )
        ) |>
          shiny::showModal()
      })
      
      database <- shiny::reactive({
        shiny::req(db_path())
        list(
          path    = db_path()$path,
          connect = \() RSQLite::dbConnect(
            RSQLite::SQLite(), db_file)
        )
      })
      
      return(database)
    }
  )
}