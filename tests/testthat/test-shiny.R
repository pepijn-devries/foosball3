test_that("For now just test if the app starts with no errors", {
  app_path <- system.file("foosball3", package = "foosball3")

  app_obj <- shiny::shinyAppDir(app_path)
  
  shiny::testServer(app_obj, {
    expect_no_error({})
  })
})