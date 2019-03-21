context("autoDetect")


test_that("autoDetect returns an array", {
  
  library(mockery)
  
  path1 <- system.file("001.png", package = "MtreeRing")
  img1 <- imgInput(img = path1, dpi = 1200)
  dev.off(attributes(img1)$dn)
  
  t1 <- autoDetect(ring.data = img1, auto.path = T, method = 'watershed')
  dn <- attributes(t1)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  t2 <- autoDetect(ring.data = img1, seg = 3, auto.path = T, method = 'canny')
  dn <- attributes(t2)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  t21 <- autoDetect(ring.data = img1, seg = 3, auto.path = T, 
    method = 'canny', canny.smoothing = 1, 
    canny.t1 = 0.3, canny.t2 = 0.2)
  dn <- attributes(t21)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  t3 <- autoDetect(ring.data = img1, auto.path = T, method = 'lineardetect')
  dn <- attributes(t3)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  t4 <- autoDetect(ring.data = img1, auto.path = T, manual = T)
  dn <- attributes(t4)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  t5 <- autoDetect(ring.data = img1, auto.path = T, manual = T, incline = T)
  dn <- attributes(t5)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  
  
  path2 <- system.file("002.png", package = "MtreeRing")
  img2 <- imgInput(img = path2, dpi = 1200)
  dev.off(attributes(img2)$dn)
  
  t6 <- autoDetect(ring.data = img2, marker.correction = T, 
    method = 'watershed', struc.ele1 = c(4,4), struc.ele2 = c(15,15))
  dn <- attributes(t6)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  
  t7 <- autoDetect(ring.data = img2, auto.path = T, incline = T,
    seg = 3, method = 'watershed')
  dn <- attributes(t7)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  
  path3 <- system.file("001gray.png", package = "MtreeRing")
  img3 <- imgInput(img = path3, dpi = 1200)
  dev.off(attributes(img3)$dn)
  t8 <- autoDetect(ring.data = img3, marker.correction = T, 
    method = 'watershed', struc.ele1 = c(4,4), struc.ele2 = c(15,15))
  dn <- attributes(t8)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  
  expect_is(t1, "array")
  expect_is(t2, "array")
  expect_is(t3, "array")
  expect_is(t6, "array")
  expect_is(t7, "array")
  expect_is(t8, "matrix")
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
  expect_error(autoDetect(img1, seg = 'not a numeric'))
  expect_error(autoDetect(img1, method = c('two', 'methods')))
  expect_error(autoDetect(img1, method = 1))
  img1 <- imgInput(img = path1, dpi = 200)
  dev.off(attributes(img1)$dn)
  expect_error(autoDetect(img1, manual = F))
  
})


test_that("mock test", {
  
  library(mockery)
  
  path1 <- system.file("001gray.png", package = "MtreeRing")
  img1 <- imgInput(img = path1, dpi = 1200)
  dn <- attributes(img1)$dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  
  mock1 <- mock(c(20, 2300, 30, 140, 80), cycle = T)
  stub(autoDetect, 'create_path', mock1)
  t8 <- autoDetect(ring.data = img1, auto.path = F, 
    seg = 3, method = 'watershed')
  dn <- attributes(t8)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  
  expect_is(t8, "matrix")
 
})

test_that("mock test", {
  
  library(mockery)
  
  path2 <- system.file("incline.png", package = "MtreeRing")
  img2 <- imgInput(img = path2, dpi = 1200)
  dev.off(attributes(img2)$dn)
  
  mock2 <- mock(c(20, 1400, 0, 160, 50, 80, 20), cycle = T)
  stub(autoDetect, 'create_path', mock2)
  t8 <- autoDetect(ring.data = img2, auto.path = T, seg = 2, incline = T)
  dn <- attributes(t8)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  
  expect_is(t8, "array")
  
})

