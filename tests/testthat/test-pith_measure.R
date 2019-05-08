context("pith_measure")
## test1
test_that("pith_measure returns a dataframe", {
  
  library(mockery)
  path1 <- system.file("arc.png", package = "MtreeRing")
  ## Read and plot the image:
  img1 <- ring_read(img = path1, dpi = 1200)

  mock1 <- mock(list(x = c(36, 110, 168.8, 249.3, 354.2), 
                     y = c(264, 264, 264, 264, 264)), cycle = T)
  stub(pith_measure, 'locator', mock1)
  
  a <- list(x = 444.7, y = 264.46)
  b <- list(x = 643, y = 264.46)
  c <- list(x = 543.8, y = 300.48)
  arc <- list(a = a, b = b, c = c)

  mock2 <- mock(list(arc = arc, p = 264, t = 10, s = 2), cycle = T)
  stub(pith_measure, 'add_path', mock2)
  t2 <- pith_measure(img1, T)
  
  dev.off(attributes(img1)$dn)
  expect_is(img1, "magick-image")
  expect_is(t2, "data.frame")
})

## test1-2
test_that("pith_measure returns a dataframe", {
  
  library(mockery)
  path1 <- system.file("arc.png", package = "MtreeRing")
  ## Read and plot the image:
  img1 <- ring_read(img = path1, dpi = 1200, plot = F)
  
  mock1 <- mock(list(x = c(36, 110, 168.8, 249.3, 354.2), 
    y = c(264, 264, 264, 264, 264)), cycle = T)
  stub(pith_measure, 'locator', mock1)
  
  a <- list(x = 444.7, y = 264.46)
  b <- list(x = 643, y = 264.46)
  c <- list(x = 543.8, y = 300.48)
  arc <- list(a = a, b = b, c = c)
  
  mock2 <- mock(list(arc = arc, p = 264, t = 10, s = 2), cycle = T)
  stub(pith_measure, 'add_path', mock2)
  t2 <- pith_measure(img1, T)
  dev.off(as.numeric(dev.cur()))
  expect_is(img1, "magick-image")
  expect_is(t2, "data.frame")
})

## test1-3
test_that("pith_measure returns a dataframe", {
  
  library(mockery)
  path1 <- system.file("arc.png", package = "MtreeRing")
  ## Read and plot the image:
  img1 <- ring_read(img = path1, dpi = 1200, plot = F)
  
  mock1 <- mock(list(x = c(36, 110, 168.8, 249.3, 354.2), 
    y = c(264, 264, 264, 264, 264)), cycle = T)
  stub(pith_measure, 'locator', mock1)
  
  a <- list(x = 444.7, y = 264.46)
  b <- list(x = 643, y = 264.46)
  c <- list(x = 543.8, y = 300.48)
  arc <- list(a = a, b = b, c = c)
  
  mock2 <- mock(list(arc = arc, p = 264, t = 10, s = 2), cycle = T)
  stub(pith_measure, 'add_path', mock2)
  
  mock3 <- mock(matrix(c(1,5000,5000), nrow = 1, byrow = T), cycle = T)
  stub(pith_measure, 'image_info', mock3)
  
  t2 <- pith_measure(img1, T)
  dev.off(as.numeric(dev.cur()))
  expect_is(img1, "magick-image")
  expect_is(t2, "data.frame")
})

## test 2
test_that("pith_measure returns a dataframe", {
  
  library(mockery)
  path1 <- system.file("arc.png", package = "MtreeRing")
  ## Read and plot the image:
  img1 <- ring_read(img = path1, dpi = 1200)
  
  mock1 <- mock(list(x = c(36, 110, 168.8, 249.3, 354.2), 
    y = c(264, 264, 264, 264, 264)), cycle = T)
  stub(pith_measure, 'locator', mock1)
  
  a <- list(x = 444.7, y = 264.46)
  b <- list(x = 643, y = 264.46)
  c <- list(x = 543.8, y = 300.48)
  arc <- list(a = a, b = b, c = c)
  
  mock2 <- mock(list(arc = arc, p = 264, t = 10, s = 2), cycle = T)
  stub(pith_measure, 'add_path', mock2)
  t2 <- pith_measure(img1, T, last.yr = 2015)
  
  dev.off(attributes(img1)$dn)
  expect_is(img1, "magick-image")
  expect_is(t2, "data.frame")
})

## test 3
test_that("pith_measure returns a dataframe", {
  
  library(mockery)
  path1 <- system.file("arc.png", package = "MtreeRing")
  ## Read and plot the image:
  img1 <- ring_read(img = path1, dpi = 1200)
  
  mock1 <- mock(list(t = 6, s = 1), cycle = T)
  stub(pith_measure, 'add_path', mock1)
  
  mock2 <- mock(
   list(x = c(33.39, 101.03, 151.16, 223.58, 305.55, 369.21, 431.28, 471.86), 
        y = c(250.25, 233.19, 223.71, 202.86, 182.00, 167.79, 148.83, 140.30)), 
   cycle = T)
  stub(pith_measure, 'locator', mock2)
  t2 <- pith_measure(img1, F, last.yr = 2015)
  dev.off(attributes(img1)$dn)
  expect_is(img1, "magick-image")
  expect_is(t2, "data.frame")
})

## test 4
test_that("pith_measure returns a dataframe", {
  
  library(mockery)
  path1 <- system.file("arc.png", package = "MtreeRing")
  ## Read and plot the image:
  img1 <- ring_read(img = path1, dpi = 1200)
  
  mock1 <- mock(list(t = 6, s = 1), cycle = T)
  stub(pith_measure, 'add_path', mock1)
  
  mock2 <- mock(
    list(x = c(33.39, 101.03, 151.16, 223.58, 305.55, 369.21, 431.28, 471.86), 
      y = c(250.25, 233.19, 223.71, 202.86, 182.00, 167.79, 148.83, 140.30)), 
    cycle = T)
  stub(pith_measure, 'locator', mock2)
  t2 <- pith_measure(img1, F, last.yr = NULL)
  
  dev.off(attributes(img1)$dn)
  expect_is(img1, "magick-image")
  expect_is(t2, "data.frame")
})

## test 5
test_that("pith_measure returns a dataframe", {
  
  library(mockery)
  path1 <- system.file("arc.png", package = "MtreeRing")
  ## Read and plot the image:
  img1 <- ring_read(img = path1, dpi = 1200)
  
  mock1 <- mock(list(x = c(546.67, 33.39), y = c(122.29, 251.19)), cycle = T)
  stub(add_path, 'inclined_path', mock1)
  a1 <- add_path(inner.arc = F, 16, 'gray', 1.5) 
  expect_is(a1, "list")
  
  mock2 <- mock(list(x = 555, y = 111), cycle = T)
  stub(mark_arc, 'locator', mock2)
  arc.position <- mark_arc(111, 16, 'red', 1.5)  
  expect_is(arc.position, "list")
  
  mock3 <- mock(list(x = 555, y = 265), cycle = T)
  stub(add_path, 'locator', mock3)
  a <- list(x = 444.7, y = 264.46)
  b <- list(x = 643, y = 264.46)
  c <- list(x = 543.8, y = 300.48)
  arc <- list(a = a, b = b, c = c)
  mock4 <- mock(arc, cycle = T)
  stub(add_path, 'mark_arc', mock4)
  a2 <- add_path(inner.arc = T, 16, 'gray', 1.5) 
  expect_is(a2, "list")
  
  mock5 <- mock(list(x = 555, y = 111), cycle = T)
  stub(inclined_path, 'locator', mock5)
  p1 <- inclined_path(16, 'red', 1.5)  
  expect_is(p1, "list")
  
  dev.off(attributes(img1)$dn)
  
})

