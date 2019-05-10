context("ring_modify")

test_that("ring_modify", {
  library(mockery)
  path1 <- system.file("001.png", package = "MtreeRing")
  img1 <- ring_read(img = path1, dpi = 1200)

  t1 <- ring_detect(ring.data = img1, auto.path = T, seg = 3, sample.yr = 2015,
                   method = 'watershed', incline = T)
  dn <- attributes(t1)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  
  t2 <- ring_detect(ring.data = img1, auto.path = T, seg = 3, sample.yr = 2015,
                   method = 'watershed', incline = F)
  dn <- attributes(t2)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  
  t3 <- ring_modify(ring.data = t1, del.u = 12:18, del.l = 19:25)
  dn <- attributes(t3)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off) 
  
  t4 <- ring_modify(ring.data = t2, del = 12:25)
  dn <- attributes(t4)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  

  expect_is(t1, "array")
  expect_is(t2, "array")
  expect_is(t3, "array")
  expect_is(t4, "array")
  expect_error(ring_modify(t2, add = T))
  expect_error(ring_modify(t2, del = 31:35), 
    'The ring number you entered was not correct')
  expect_error(ring_modify(t1, del.u = 20:35), 
    'The ring number on the upper path you entered was not correct')
  expect_error(ring_modify(t1, del.l = 20:35), 
    'The ring number on the lower path you entered was not correct')
  expect_error(ring_modify(t1, add = F))

  t5 <- ring_detect(img1, sample.yr = 2015, method = 'watershed', incline = F)
  mock1 <- mock(list(x = c(100, 200, 300, 500), 
                     y = c(20, 160, 160, 20)), cycle = T)
  stub(ring_modify, 'locator', mock1)
  m1 <- ring_modify(ring.data = t5, add = T)
  expect_is(m1, "array")
  
  t6 <- ring_detect(img1, sample.yr = 2015, method = 'watershed', incline = T)
  mock2 <- mock(list(x = c(111, 222, 333, 555, 666), 
                     y = c(20, 160, 160, 20, 20)), cycle = T)
  stub(ring_modify, 'locator', mock2)
  m2 <- ring_modify(ring.data = t6, add = T)
  expect_is(m2, "array")
  dn <- attributes(t5)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  dn <- attributes(m1)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  dn <- attributes(t6)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  dn <- attributes(m2)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
})
