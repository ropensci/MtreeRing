context("launchMtRApp")

test_that("the function run a app", {
  
  a <- launchMtRApp()
  a
  expect_is(a, "shiny.appobj")
  expect_type(a, "list")
  
})
