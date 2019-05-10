context("draw_border1")

test_that("mock test", {
  
  library(mockery)
  
  path1 <- system.file("001.png", package = "MtreeRing")
  img1 <- ring_read(img = path1, dpi = 1200, plot = T)
  
  mock3 <- mock(list(x = 50, y = 100), cycle = T)
  stub(draw_border1, 'locator', mock3)
  p1 <- draw_border1(170, 2300, 'black')
  
  mock3 <- mock(list(x = 2500, y = 100), cycle = T)
  stub(draw_border1, 'locator', mock3)
  p1 <- draw_border1(170, 2000, 'black')
  
  mock3 <- mock(list(x = 50, y = 160), cycle = T)
  stub(draw_border2, 'locator', mock3)
  p1 <- draw_border2(170, 2300, 1, 2200, 'black')
  dev.off(attributes(img1)$dn)
  
  expect_is(p1, 'numeric')
  
})

