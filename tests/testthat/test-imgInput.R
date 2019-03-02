context("imgInput")

test_that("imgInput plots a tree ring image and returns a magick object", {
  
  path1 <- system.file("001.png", package = "MtreeRing")
  path2 <- system.file("001.tif", package = "MtreeRing")
  path3 <- system.file("001.bmp", package = "MtreeRing")
  path4 <- system.file("001.jpg", package = "MtreeRing")
  path5 <- system.file("001gray.png", package = "MtreeRing")
  
  ## Read and plot the image:
  img1 <- imgInput(img = path1, dpi = 1200)
  dev.off()
  img2 <- imgInput(img = path2, dpi = 1200)
  dev.off()
  img3 <- imgInput(img = path3, dpi = 1200)
  dev.off()
  img4 <- imgInput(img = path4, dpi = 1200)
  dev.off()
  img5 <- imgInput(img = path5, dpi = 1200)
  dev.off()
  
  expect_is(img1, "magick-image")
  expect_is(img2, "magick-image")
  expect_is(img3, "magick-image")
  expect_is(img4, "magick-image")
  expect_is(img5, "magick-image")

})
