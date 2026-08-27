diagramUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    id = ns("diagram_body"),
    bslib::toolbar(
      gap = "5px",
      shiny::checkboxInput(ns("checkView"), "Views"),
      shiny::downloadLink(
        ns("downloadDiagram"),
        bsicons::bs_icon("filetype-svg", "2em", title = "Download diagram svg"))
    ),
    shiny::uiOutput(ns("diagram")),
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
}

diagramServer <- function(id, database) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      get_diagram_svg <- shiny::reactive({
        schem <- get_schema()
        dm::dm_draw(
          schem
        ) |>
          DiagrammeRsvg::export_svg()
      })
      
      output$diagram <- shiny::renderUI({
        shiny::HTML(get_diagram_svg())
      })
      
      get_schema <- shiny::reactive({
        con <- database()$database$connect()
        on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
        tbls <- RSQLite::dbListTables(con)
        
        my_dm <- dm::dm_from_con(con, learn_keys = TRUE)
        if (input$checkView) {
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
        }
        my_dm
      })
      
      shiny::observe({
        shiny::req(input$diagram_node)
        database()$menu_selector("Table Details", input$diagram_node)
      })

      output$downloadDiagram <- shiny::downloadHandler(
        \() "foosball-diagram.svg", \(file) {
          writeLines(get_diagram_svg(), file)
        }, "image/svg+xml")
      
    }
  )
}