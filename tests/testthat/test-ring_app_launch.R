context("ring_app_launch")

test_that("the function run a app", {
 
  a <- ring_app_launch()
  expect_is(a, "shiny.appobj")
  expect_type(a, "list")
  
})
