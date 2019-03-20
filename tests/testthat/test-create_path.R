context("create_path")

test_that("mock test", {
  
  library(mockery)
  
  path1 <- system.file("001.png", package = "MtreeRing")
  img1 <- imgInput(img = path1, dpi = 1200)
  mock1 <- mock(c(50, 1800), cycle = T)
  mock2 <- mock(c(20, 160), cycle = T)
  mock3 <- mock(list(y = 111), cycle = T)
  stub(create_path, 'draw_border1', mock1)
  stub(create_path, 'draw_border2', mock2)
  stub(create_path, 'locator', mock3)
  
  p1 <- create_path(F, 'canny', T, 1, 40, 2300, 170, 'black') 
  expect_is(p1, 'numeric')
  
  p2 <- create_path(F, 'lineardetect', F, 1, 40, 2300, 170, 'black') 
  expect_is(p2, 'numeric')
  
  p3 <- create_path(F, 'watershed', F, 1, 40, 2300, 170, 'black') 
  expect_is(p2, 'numeric')
  
  dev.off(attributes(img1)$dn)
  
})

