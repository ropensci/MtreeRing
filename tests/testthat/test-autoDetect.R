context("autoDetect")

test_that("autoDetect returns an array", {
  
  path1 <- system.file("001.png", package = "MtreeRing")
  img1 <- imgInput(img = path1, dpi = 1200)
  dev.off(attributes(img1)$dn)
  
  t1 <- autoDetect(ring.data = img1, auto.path = T, method = 'watershed')
  dn <- attributes(t1)$seg.dn
  sapply(dn, dev.off)
  t2 <- autoDetect(ring.data = img1, seg = 3, auto.path = T, method = 'canny')
  dn <- attributes(t2)$seg.dn
  sapply(dn, dev.off)
  t21 <- autoDetect(ring.data = img1, seg = 3, auto.path = T, 
           method = 'canny', canny.smoothing = 1, 
           canny.t1 = 0.3, canny.t2 = 0.2)
  dn <- attributes(t21)$seg.dn
  sapply(dn, dev.off)
  t3 <- autoDetect(ring.data = img1, auto.path = T, method = 'lineardetect')
  dn <- attributes(t3)$seg.dn
  sapply(dn, dev.off)
  t4 <- autoDetect(ring.data = img1, auto.path = T, manual = T)
  dn <- attributes(t4)$seg.dn
  sapply(dn, dev.off)
  t5 <- autoDetect(ring.data = img1, auto.path = T, manual = T, incline = T)
  dn <- attributes(t5)$seg.dn
  sapply(dn, dev.off)
  
  path2 <- system.file("002.png", package = "MtreeRing")
  img2 <- imgInput(img = path2, dpi = 1200)
  dev.off(attributes(img2)$dn)
  
  t6 <- autoDetect(ring.data = img2, marker.correction = T, 
          method = 'watershed', struc.ele1 = c(4,4), struc.ele2 = c(15,15))
  dn <- attributes(t6)$seg.dn
  sapply(dn, dev.off)
  
  
  expect_is(t1, "array")
  expect_is(t2, "array")
  expect_is(t3, "array")
  expect_is(t6, "array")
  expect_true(attributes(t4)$bor.col %>% length == 0)
  expect_null(attributes(t4)$year)
  expect_null(attributes(t4)$sn)
  expect_true(attributes(t5)$bor.u %>% length == 0)
  expect_true(attributes(t5)$bor.l %>% length == 0)
  
  expect_error(autoDetect(img1, method = 'watershed', watershed.threshold = 1), 
               'Ring border was not detected')
  expect_error(autoDetect(img1, method = 'lineardetect', incline = T), 
               'The linear detection can only create one path')
  expect_error(autoDetect(img1, method = 'watershed', watershed.threshold = 1), 
    'Ring border was not detected')

})
