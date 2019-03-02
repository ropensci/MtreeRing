context("calcRingWidth")

test_that("calcRingWidth returns a data.frame", {
  
  path1 <- system.file("001.png", package = "MtreeRing")
  img1 <- imgInput(img = path1, dpi = 1200)
  
  t1 <- autoDetect(img1, auto.path = T, method = 'watershed', incline = T)
  dev.off()
  t2 <- autoDetect(img1, auto.path = T, method = 'canny', incline = F)
  dev.off()
  
  rw1 <- calcRingWidth(t1, '940220')
  rw2 <- calcRingWidth(t2, '940220')
  
  expect_is(rw1, "data.frame")
  expect_is(rw2, "data.frame")
  expect_equal(nrow(rw1), nrow(rw2))
  expect_true(colnames(rw1) == '940220')
  expect_true(colnames(rw2) == '940220')


})
