warmingUpUI <- function() {
  bslib::layout_column_wrap(
    bslib::card(
      bslib::card_header("Survivor - Eye of the Tiger"),
      bslib::card_body(
        fillable = FALSE,
        shiny::a(
          href = "https://www.youtube.com/watch?v=btPJPFnesV4",
          target = "_blank",
          shiny::img(
            src = "https://i.ytimg.com/vi/btPJPFnesV4/hqdefault.jpg",
            width = "100%"
          )
        )
      )
    ),
    bslib::card(
      bslib::card_header("Bill Conti - Gonna fly now"),
      bslib::card_body(
        fillable = FALSE,
        shiny::a(
          href = "https://www.youtube.com/watch?v=MR6FXpaECY8&t=22s",
          target = "_blank",
          shiny::img(
            src = "https://i.ytimg.com/vi/MR6FXpaECY8/hqdefault.jpg",
            width = "100%"
          )
        )
      )
    )
  )
}
