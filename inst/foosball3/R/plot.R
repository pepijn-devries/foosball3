plotUI <- function(id) {
  ns <- shiny::NS(id)
  ggiraph::girafeOutput(ns("plot"), width = "100%")
}

plotServer <- function(id, layers) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$plot <- ggiraph::renderGirafe({
        lyrs <- layers()
        ggobj <-
          if (nrow(ggplot2::get_layer_data(lyrs)) == 0) {
            ggplot2::ggplot(
              dplyr::tibble(
                x = 0, y = 0, label = "No data to plot"
              ),
              ggplot2::aes(x = x, y = y, label = label)
            ) +
              ggplot2::geom_text() +
              ggplot2::theme_void()
          } else {
            lyrs +
              ggplot2::theme_light()
          }
        ggiraph::girafe( ggobj, width_svg = 10, height_svg = 4)
      })
    }
  )
}