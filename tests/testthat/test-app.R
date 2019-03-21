context('test app')

test_that("app works", {
  library(shinytest)
  skip_on_cran()
  app_dir <- system.file('mtr_app/', package = 'MtreeRing')
  expect_pass(testApp(app_dir, compareImages = FALSE))
})
