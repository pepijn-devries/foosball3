pictureUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("picture"))
}

pictureServer <- function(id, tournament) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$picture <- renderUI({
        tnmt <- tournament()
        id <- tnmt$selected$TOURNAMENT_ID
        if (length(id) == 0) {
          "Select a tournament first for a picture"
        } else {
          get_picture()
        }
      })
      
      get_picture <- shiny::reactive({
        shiny::req(tournament())
        tnmt <- tournament()
        id <- tnmt$selected$TOURNAMENT_ID
        con <- tnmt$database$connect()
        on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
        pic <-
          dplyr::tbl(con, "pictures") |>
          dplyr::filter(.data$TOURNAMENT_ID == !!id) |>
          dplyr::collect()
        if (nrow(pic) > 1) pic <- dplyr::sample_n(pic, 1L)
        
        pictags <- NULL
        if (nrow(pic) > 0) {
          pictags <-
            dplyr::tbl(con, "picture_tag_names") |>
            dplyr::filter(.data$PICTURE_ID == !!pic$PICTURE_ID) |>
            dplyr::collect()

          tags <-
            pictags |>
            dplyr::rowwise() |>
            dplyr::mutate(
              tags = list(
                shiny::tags$g(
                  class = "foosball-facemarker",
                  shiny::a(
                    onClick = sprintf(
                      "Shiny.onInputChange(`%s`, {id: %i,timestamp: new Date()});",
                      ns("faceclick"),
                      .data$PERSON_ID),
                    shiny::tags$circle(
                      cx = as.integer(.data$TAG_X),
                      cy = as.integer(.data$TAG_Y),
                      r  = as.integer(.data$TAG_SIZE))
                  ),
                  shiny::tags$text(
                    `text-anchor` = "middle",
                    x = as.integer(.data$TAG_X),
                    y = as.integer(.data$TAG_Y + .data$TAG_SIZE + 2*pic$PICTURE_WIDTH/75),
                    .data$PERSON_NAME)
                )
              )
            ) |>
            dplyr::pull("tags")
          tags <- do.call(shiny::tagList, tags)
          result <- shiny::tags$svg(
            xmlns = "http://www.w3.org/2000/svg",
            `xmlns:xlink` = "http://www.w3.org/1999/xlink",
            viewBox = sprintf("0 0 %i %i", pic$PICTURE_WIDTH, pic$PICTURE_HEIGHT),
            style = sprintf("--vb-width: %i; --vb-height: %i",
                            pic$PICTURE_WIDTH, pic$PICTURE_HEIGHT),
            class = "foosball-picture",
            shiny::tags$image(
              width  = pic$PICTURE_WIDTH,
              height = pic$PICTURE_HEIGHT,
              href   = base64enc::dataURI(pic$JPG_DATA[[1]], "image/jpg")
            ),
            !!!tags
          )
        } else {
          result <- "No picture available"
        }
        return(result)
      })
      
      return(shiny::reactive({ input$faceclick }))
    }
  )
}
