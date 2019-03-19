context("visualSelect")

test_that("visualSelect", {
  
  path1 <- system.file("001.png", package = "MtreeRing")
  img1 <- imgInput(img = path1, dpi = 1200)
  dev.off(attributes(img1)$dn)
  
  t1 <- autoDetect(ring.data = img1, auto.path = T, seg = 3,
                   method = 'watershed', incline = T)
  dn <- attributes(t1)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  t2 <- autoDetect(ring.data = img1, auto.path = T, seg = 3,
                   method = 'watershed', incline = F)
  dn <- attributes(t2)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  
  t3 <- visualSelect(ring.data = t1, del.u = 12:18, del.l = 19:25)
  dn <- attributes(t3)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  t4 <- visualSelect(ring.data = t2, del = 12:25)
  dn <- attributes(t4)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  
  expect_is(t1, "array")
  expect_is(t2, "array")
  expect_error(visualSelect(t2, del = 31:35), 
    'The ring number you entered was not correct')
  expect_error(visualSelect(t1, del.u = 20:35), 
    'The ring number on the upper path you entered was not correct')
  expect_error(visualSelect(t1, del.l = 20:35), 
    'The ring number on the lower path you entered was not correct')

})
