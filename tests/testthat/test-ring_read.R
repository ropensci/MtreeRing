context("ring_read")

test_that("ring_read plots a tree ring image and returns a magick object", {
  
  path1 <- system.file("001.png", package = "MtreeRing")
  path2 <- system.file("001.tif", package = "MtreeRing")
  path3 <- system.file("001.bmp", package = "MtreeRing")
  path4 <- system.file("001.jpg", package = "MtreeRing")
  path5 <- system.file("001gray.png", package = "MtreeRing")
  
  ## Read and plot the image:
  img1 <- ring_read(img = path1, dpi = 1200, plot = TRUE)
  dev.off(attributes(img1)$dn)
  
  img2 <- ring_read(img = path2, dpi = 2540)
  img3 <- ring_read(img = path3, dpi = 1200)
  img4 <- ring_read(img = path4, dpi = 1200)
  img5 <- ring_read(img = path5, dpi = 1200)
  img6 <- ring_read(img = path5, dpi = 1200, rotate = 180)

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
  expect_is(img1, "magick-image")
})

test_that("mock test", {
  path5 <- system.file("001gray.png", package = "MtreeRing")
  library(mockery)
  mock1 <- mock(20*1024^2, cycle = T)
  stub(ring_read, 'file.size', mock1)
  img1 <- ring_read(path5, dpi = 1200)
  expect_is(img1, "magick-image")
})

test_that("mock test", {
  path5 <- system.file("001gray.png", package = "MtreeRing")
  library(mockery)
  mock1 <- mock(matrix(c(1, 5000, 5000), nrow = 1, byrow = T), cycle = T)
  stub(ring_read, 'image_info', mock1)
  img1 <- ring_read(path5, dpi = 1200)
  expect_is(img1, "magick-image")
})


