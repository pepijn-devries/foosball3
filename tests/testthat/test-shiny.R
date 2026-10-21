test_that("For now just test if the app starts with no errors", {
  app_obj <- foosball3() |>
    suppressMessages()
  
  shiny::testServer(app_obj, {
    expect_no_error({})
  })
})