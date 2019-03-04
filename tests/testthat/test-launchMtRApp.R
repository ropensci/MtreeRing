context("imgInput")

test_that("the function run a app", {
  
  a <- launchMtRApp()
  
  expect_is(a, "shiny.appobj")
  expect_type(a, "list")
  
})
