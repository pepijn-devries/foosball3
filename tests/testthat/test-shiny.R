test_that("For now just test if the app starts with no errors", {
  skip_if_not(foosball3_suggests_ok(), "This test needs all suggests")
  app_obj <- foosball3() |>
    suppressMessages()
  
  shiny::testServer(app_obj, {
    expect_no_error({})
  })
})