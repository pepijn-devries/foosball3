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
        db_schemas_static[[ifelse(input$checkView, 1L, 2L)]]
      })
      
      output$diagram <- shiny::renderUI({
        shiny::HTML(get_diagram_svg())
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