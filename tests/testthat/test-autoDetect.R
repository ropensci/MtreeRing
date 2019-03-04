context("autoDetect")

test_that("autoDetect returns an array", {
  
  path1 <- system.file("001.png", package = "MtreeRing")
  img1 <- imgInput(img = path1, dpi = 1200)
  
  t1 <- autoDetect(ring.data = img1, auto.path = T, method = 'watershed')
  dev.off()
  t2 <- autoDetect(ring.data = img1, auto.path = T, method = 'canny')
  dev.off()
  t3 <- autoDetect(ring.data = img1, auto.path = T, method = 'lineardetect')
  dev.off()
  t4 <- autoDetect(ring.data = img1, auto.path = T, manual = T)
  dev.off()
  t5 <- autoDetect(ring.data = img1, auto.path = T, manual = T, incline = T)
  dev.off()
  
  expect_is(t1, "array")
  expect_is(t2, "array")
  expect_is(t3, "array")
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
