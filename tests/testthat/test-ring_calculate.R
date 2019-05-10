context("ring_calculate")

test_that("ring_calculate returns a data.frame", {
  
  path1 <- system.file("001.png", package = "MtreeRing")
  img1 <- ring_read(img = path1, dpi = 1200)

  t1 <- ring_detect(ring.data = img1, auto.path = T, sample.yr = 2015,
                   method = 'watershed', incline = T)
  dn <- attributes(t1)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  t2 <- ring_detect(ring.data = img1, auto.path = T, sample.yr = 2015,
                   method = 'canny', incline = F)
  dn <- attributes(t2)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)    
  
  rw1 <- ring_calculate(ring.data = t1, '940220')
  rw2 <- ring_calculate(ring.data = t2, '940220')
  
  t3 <- ring_detect(ring.data = img1, sample.yr = 2015,incline = T)
  dn <- attributes(t3)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  attributes(t3)$bor.u <- vector()
  attributes(t3)$bor.l <- vector()
  
  t4 <- ring_detect(ring.data = img1, auto.path = T, sample.yr = 2015)
  dn <- attributes(t4)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  attributes(t4)$bor.col <- vector()
  
  t5 <- ring_modify(t1, del.l = c(1,5))
  dn <- attributes(t5)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  
  expect_is(rw1, "data.frame")
  expect_is(rw2, "data.frame")
  expect_equal(nrow(rw1), nrow(rw2))
  expect_true(colnames(rw1) == '940220')
  expect_true(colnames(rw2) == '940220')
  expect_error(ring_calculate(t3, '940220'))
  expect_error(ring_calculate(t4, '940220'))
  expect_error(ring_calculate(t5, '940220'))
  
  attributes(t3)$bor.u <- c(1,2,3)
  expect_error(ring_calculate(t3, '940220'))
  

})
