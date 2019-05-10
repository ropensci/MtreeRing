context("ring_detect")


test_that("ring_detect returns an array", {
  
  library(mockery)
  
  path0 <- system.file("001.png", package = "MtreeRing")
  img0 <- ring_read(img = path0, dpi = 1200, plot = T)
  t0 <- ring_detect(ring.data = img0, sample.yr = 2015, method = 'watershed')
  dev.off(as.numeric(attributes(img0)$dn))
  dn <- attributes(t0)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  
  path1 <- system.file("001.png", package = "MtreeRing")
  img1 <- ring_read(img = path1, dpi = 1200)
  t1 <- ring_detect(ring.data = img1, sample.yr = 2015, method = 'watershed')
  dn <- attributes(t1)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  
  img1 <- ring_read(img = path1, dpi = 1200)
  t1 <- ring_detect(ring.data = img1, sample.yr = 2015, method = 'watershed')
  dn <- attributes(t1)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  
  t2 <- ring_detect(img1, seg = 3, sample.yr = 2015, method = 'canny')
  dn <- attributes(t2)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off) 
  
  t21 <- ring_detect(ring.data = img1, seg = 3, sample.yr = 2015, 
    method = 'canny', canny.smoothing = 1, 
    canny.t1 = 0.3, canny.t2 = 0.2)
  dn <- attributes(t21)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  
  t3 <- ring_detect(img1, sample.yr = 2015, method = 'lineardetect')
  dn <- attributes(t3)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  
  # t4 <- ring_detect(ring.data = img1, sample.yr = 2015, manual = F)
  # dn <- attributes(t4)$seg.dn
  # apply(matrix(dn, nrow = 1), 2, dev.off)  
  
  # t5 <- ring_detect( img1, sample.yr = 2015, manual = T, incline = T)
  # dn <- attributes(t5)$seg.dn
  # apply(matrix(dn, nrow = 1), 2, dev.off)
  
  
  path2 <- system.file("002.png", package = "MtreeRing")
  img2 <- ring_read(img = path2, dpi = 1200)

  t6 <- ring_detect(ring.data = img2, marker.correction = T, sample.yr = 2015,
    method = 'watershed', struc.ele1 = c(4,4), struc.ele2 = c(15,15))
  dn <- attributes(t6)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  
  t7 <- ring_detect(ring.data = img2, auto.path = T, incline = T,
    sample.yr = 2015, seg = 3, method = 'watershed')
  dn <- attributes(t7)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  
  path3 <- system.file("001gray.png", package = "MtreeRing")
  img3 <- ring_read(img = path3, dpi = 1200)
  
  t8 <- ring_detect(ring.data = img3, marker.correction = T, sample.yr = 2015,
    method = 'watershed', struc.ele1 = c(4,4), struc.ele2 = c(15,15))
  dn <- attributes(t8)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)  
  
  expect_is(t1, "array")
  expect_is(t2, "array")
  expect_is(t3, "array")
  expect_is(t6, "array")
  expect_is(t7, "array")
  expect_is(t8, c("matrix", 'array'))
  # expect_true(attributes(t4)$bor.col %>% length == 0)
  # expect_null(attributes(t4)$year)
  # expect_null(attributes(t4)$sn)
  
  expect_error(
    ring_detect(img1, sample.yr = 2015, auto.path = '001'))
  expect_error(
    ring_detect(img1, sample.yr = 2015, manual = 100))
  expect_error(
    ring_detect(img1, sample.yr = 2015, incline = 'not logical'))
  expect_error(
    ring_detect(img1, sample.yr = 2015, method = 'wrong method'))
  expect_error(
    ring_detect(img1, sample.yr = 2015, method = 100L))
  expect_error(
    ring_detect(img1, sample.yr = 2015, method = c('canny', 'watershed')))
  expect_error(
    ring_detect(img1, sample.yr = 2015, seg = 'not numeric'))
  expect_error(
    ring_detect(img1, method = 'watershed', 
      sample.yr = 2015, watershed.threshold = 1), 
    'Ring border was not detected')
  expect_error(ring_detect(img1, method = 'lineardetect', 
    sample.yr = 2015, incline = T), 
    'The linear detection can only create one path')
  img1 <- ring_read(img = path1, dpi = 200)
  expect_error(ring_detect(img1, sample.yr = 2015, manual = F))
  expect_warning(ring_detect(ring.data = img2, method = 'watershed'))
})

# test manual = TRUE and incline = F
test_that("mock test", {
  
  library(mockery)

  path1 <- system.file("001.png", package = "MtreeRing")
  img1 <- ring_read(img = path1, dpi = 1200)
  t1 <- ring_detect(ring.data = img1, sample.yr = 2015, method = 'watershed')
  dn <- attributes(t1)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  
  mock1 <- mock(t1, cycle = T)
  stub(ring_detect, 'ring_modify', mock1)
  t2 <- ring_detect(img1, seg = 1, sample.yr = 2015, manual = TRUE)
  dn <- attributes(t2)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  expect_is(t2, c("matrix", 'array'))
  
})

# test manual = TRUE and incline = T
test_that("mock test", {
  
  library(mockery)
  
  path1 <- system.file("001.png", package = "MtreeRing")
  img1 <- ring_read(img = path1, dpi = 1200)
  t1 <- ring_detect(ring.data = img1, sample.yr = 2015, method = 'watershed')
  dn <- attributes(t1)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  
  mock1 <- mock(t1, cycle = T)
  stub(ring_detect, 'ring_modify', mock1)
  t2 <- ring_detect(img1, seg = 1, incline = T, sample.yr = 2015, manual = T)
  dn <- attributes(t2)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  expect_is(t2, c("matrix", 'array'))
  
})

test_that("mock test", {
  
  library(mockery)
  
  path1 <- system.file("001gray.png", package = "MtreeRing")
  img1 <- ring_read(img = path1, dpi = 1200)
  
  mock1 <- mock(c(20, 2300, 30, 140, 80), cycle = T)
  stub(ring_detect, 'create_path', mock1)
  t8 <- ring_detect(ring.data = img1, auto.path = F, sample.yr = 2015,
    seg = 3, method = 'watershed')
  dev.off(as.numeric(attributes(t8)$dn))
  dn <- attributes(t8)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  
  expect_is(t8, c("matrix", 'array'))
 
})

test_that("mock test", {
  
  library(mockery)
  
  path2 <- system.file("incline.png", package = "MtreeRing")
  img2 <- ring_read(img = path2, dpi = 1200)

  mock2 <- mock(c(20, 1400, 0, 160, 50, 80, 20), cycle = T)
  stub(ring_detect, 'create_path', mock2)
  t8 <- ring_detect(ring.data = img2, sample.yr = 2015, seg = 2, incline = T)
  dn <- attributes(t8)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  
  expect_is(t8, "array")
  
})

test_that("mock test", {
  
  library(mockery)
  
  path2 <- system.file("incline.png", package = "MtreeRing")
  img2 <- ring_read(img = path2, dpi = 1200)
  expect_error(
    ring_detect(ring.data = img2, sample.yr = 2015, path.dis = 9, incline = T), 
    'The y-position of the path is out of range')
})


test_that("mock test", {
  
  library(mockery)
  
  path2 <- system.file("incline.png", package = "MtreeRing")
  img2 <- ring_read(img = path2, dpi = 1200, plot = FALSE)
  mock2 <- mock(c(20, 1400, 0, 160, 50, 80, 20), cycle = T)
  stub(ring_detect, 'create_path', mock2)
  t8 <- ring_detect(ring.data = img2, auto.path = F, 
    sample.yr = 2015, seg = 2, incline = T)
  dev.off(attributes(t8)$dn)
  dn <- attributes(t8)$seg.dn
  apply(matrix(dn, nrow = 1), 2, dev.off)
  
  expect_is(t8, "array")
  
})

# test_that("mock test", {
#   path5 <- system.file("001gray.png", package = "MtreeRing")
#   library(mockery)
#   mock1 <- mock(matrix(c(1, 5000, 5000), nrow = 1, byrow = T), cycle = T)
#   stub(ring_detect, 'image_info', mock1)
#   img1 <- ring_read(path5, dpi = 1200, plot = FALSE)
#   t1 <- ring_detect(ring.data = img1, manual = TRUE)
#   dn <- attributes(t1)$seg.dn
#   apply(matrix(dn, nrow = 1), 2, dev.off)
#   expect_is(img1, "magick-image")
# })
