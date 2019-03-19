context("calcRingWidth")

test_that("calcRingWidth returns a data.frame", {
  
  path1 <- system.file("001.png", package = "MtreeRing")
  img1 <- imgInput(img = path1, dpi = 1200)
  dev.off(attributes(img1)$dn)
  
  t1 <- autoDetect(ring.data = img1, auto.path = T,
                   method = 'watershed', incline = T)
  dn <- attributes(t1)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  t2 <- autoDetect(ring.data = img1, auto.path = T, 
                   method = 'canny', incline = F)
  dn <- attributes(t2)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)    
  
  t5 <- autoDetect(ring.data = img1, auto.path = T, manual = T, incline = T)
  dn <- attributes(t5)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  
  rw1 <- calcRingWidth(ring.data = t1, '940220')
  rw2 <- calcRingWidth(ring.data = t2, '940220')
  
  expect_is(rw1, "data.frame")
  expect_is(rw2, "data.frame")
  expect_equal(nrow(rw1), nrow(rw2))
  expect_true(colnames(rw1) == '940220')
  expect_true(colnames(rw2) == '940220')
  expect_error(calcRingWidth(t5, '940220'))


})
