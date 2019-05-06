context("ring_read")

test_that("ring_read plots a tree ring image and returns a magick object", {
  
  path1 <- system.file("001.png", package = "MtreeRing")
  path2 <- system.file("001.tif", package = "MtreeRing")
  path3 <- system.file("001.bmp", package = "MtreeRing")
  path4 <- system.file("001.jpg", package = "MtreeRing")
  path5 <- system.file("001gray.png", package = "MtreeRing")
  
  ## Read and plot the image:
  img1 <- ring_read(img = path1, dpi = 1200)
  dev.off(attributes(img1)$dn)
  img2 <- ring_read(img = path2, dpi = 2540)
  dev.off(attributes(img2)$dn)
  img3 <- ring_read(img = path3, dpi = 1200)
  dev.off(attributes(img3)$dn)
  img4 <- ring_read(img = path4, dpi = 1200)
  dev.off(attributes(img4)$dn)
  img5 <- ring_read(img = path5, dpi = 1200)
  dev.off(attributes(img5)$dn)
  img6 <- ring_read(img = path5, dpi = 1200, rotate = 180)
  dev.off(attributes(img6)$dn)
  
  expect_is(img1, "magick-image")
  expect_is(img2, "magick-image")
  expect_is(img3, "magick-image")
  expect_is(img4, "magick-image")
  expect_is(img5, "magick-image")
  expect_is(img6, "magick-image")
  expect_error(ring_read(img = path1), 
    'Please provide the dpi value of the image')
  expect_error(ring_read(img = path1, dpi = 1200, rotate = 88))
  
  
})

test_that("mock test", {
  path2 <- system.file("001.tif", package = "MtreeRing")
  library(mockery)
  mock1 <- mock(20*1024^2, cycle = T)
  stub(ring_read, 'file.size', mock1)
  img1 <- ring_read(path2, dpi = 1200)
  dev.off(attributes(img1)$dn)
  expect_is(img1, "magick-image")
})

test_that("mock test", {
  path5 <- system.file("001gray.png", package = "MtreeRing")
  library(mockery)
  mock1 <- mock(20*1024^2, cycle = T)
  stub(ring_read, 'file.size', mock1)
  img1 <- ring_read(path5, dpi = 1200)
  dev.off(attributes(img1)$dn)
  expect_is(img1, "magick-image")
})
