plotUI <- function(id) {
  ns <- shiny::NS(id)
  ggiraph::girafeOutput(ns("plot"), width = "100%")
}

plotServer <- function(id, plot_data, plot_aes) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$plot <- ggiraph::renderGirafe({
        ggobj <-
          if (nrow(plot_data()) == 0) {
            ggplot2::ggplot(
              dplyr::tibble(
                x = 0, y = 0, label = "No data to plot"
              ),
              ggplot2::aes(x = x, y = y, label = label)
            ) +
              ggplot2::geom_text() +
              ggplot2::theme_void()
          } else {
            ggplot2::ggplot(data = plot_data()) +
              plot_aes +
              ggiraph::geom_bar_interactive(stat = "identity") +
              ggplot2::scale_fill_brewer(palette = "Pastel1", name = "Points") +
              ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45)) +
              ggplot2::theme_light()
          }
        ggiraph::girafe( ggobj, width_svg = 10, height_svg = 4)
      })
    }
  )
}