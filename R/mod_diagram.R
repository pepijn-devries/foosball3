.schema_svg_cache <- local({
  
  result <- if (requireNamespace(c("dm", "DiagrammeRsvg"))) {
    tf <- tempfile()
    foosball3_create_db(tf)
    con <- RSQLite::dbConnect(RSQLite::SQLite(), tf)
    on.exit({
      RSQLite::dbDisconnect(con)
      unlink(tf, TRUE, TRUE)
    }, add = TRUE)
    
    get_schema_static <- function(con, include_views) {
      .foosball3_get_schema(con, include_views) |>
        dm::dm_draw() |>
        DiagrammeRsvg::export_svg()
    }
    
    lapply(c(TRUE, FALSE), get_schema_static, con = con)
  } else {
    rep("dm or diagrameRsvg not installed", 2) |> as.list()
  }
  stats::setNames(result, c("with_views", "without_views"))
})

#' @include create.R
#' @noRd
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
    shiny::tagList(
      shiny::div(id = ns("diagram_with"),    .schema_svg_cache["with_views"] |>
            as.character() |> shiny::HTML()) |> shinyjs::hidden(),
      shiny::div(id = ns("diagram_without"), .schema_svg_cache["without_views"] |>
            as.character() |> shiny::HTML()),
    ),
    shiny::tags$script(shiny::HTML(sprintf("
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
        .schema_svg_cache[[
          ifelse(input$checkView, "with_views", "without_views")
        ]]
      })
      
      shiny::observe({
        shinyjs::toggle(
          id = "diagram_with",
          condition = isTRUE(input$checkView)
        )
        shinyjs::toggle(
          id = "diagram_without",
          condition = !isTRUE(input$checkView)
        )
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