imgInput <- function(img, dpi = NULL, RGB = c(0.299, 0.587, 0.114), 
                     rotate = 0, magick = TRUE)
{
  check.degree <- rotate %in% c(0, 90, 180, 270, 360) 
  if (!check.degree)
    stop(paste('The argument \'rotate\' should be one of',
                 'the following integers: 0, 90, 180 or 270'))
  if (is.null(dpi))
    stop('Please provide the dpi value of the image')
  options(warn = -1)
  img.size <- file.size(img) / 1024^2
  if (img.size <=10 | !magick) {
    last.path <- basename(img)
    if (grepl("\\.tif", last.path))
      tree.data <- readTIFF(img, native = FALSE, info = FALSE)
    if (grepl("\\.png", last.path))
      tree.data <- readPNG(img, native = FALSE, info = FALSE)
    if (grepl("\\.jpg", last.path) | grepl("\\.jpeg", last.path))
      tree.data <- readJPEG(img, native = FALSE)
    if (grepl("\\.bmp", last.path))
      tree.data <- read.bmp(img) / 255
    td.dim <- dim(tree.data)
    if (!is.matrix(tree.data)) {
      if (td.dim[3] %in% c(2, 4))
        tree.data <- tree.data[, , -td.dim[3]]
    }
    if (is.matrix(tree.data)) {
      tdata <- as.raster(tree.data) %>%
               image_read %>%
               image_convert(colorspace = 'gray')
    } else {
      tdata <- image_read(tree.data)
    }
    rm(tree.data)
    gc()
  } else {
    tdata <- image_read(img)
  }
  options(warn = 0)
  if (rotate != 0)
    tdata <- image_rotate(tdata, rotate)
  dim.tdata <- image_info(tdata) %>% '['(1, 2:3) %>% as.numeric
  dimcol <- dim.tdata[1]
  dimrow <- dim.tdata[2]
  if ((dimcol*dimrow) >= 1.2e+07) {
    resize.ratio <- 300 / dpi
    resize.str <- paste0(round(dimcol*resize.ratio), 'x', 
                         round(dimrow*resize.ratio))
    tdata.copy <- image_resize(tdata, resize.str)
  } else{
    tdata.copy <- tdata
  }
  dev.new()
  if (names(dev.cur()) == "RStudioGD") dev.new()
  device.number <- as.numeric(dev.cur())
  img.name <- ifelse(is.character(img),
                     basename(img),
                     substitute(img) %>% as.character)
  attributes(tdata) <- c(attributes(tdata), 
                         list(x.dpi = dpi, RGB = RGB, img.name = img.name,
                              dn = device.number, dimt = dim.tdata))
  xleft <- 0
  ybottom <- 0
  xright <- dimcol
  ytop <- dimrow
  layout(matrix(c(rep(1, 3), 2), 4, 1))
  plot(x = 0, y = 0, main = img.name, xlab = '', ylab = '',
       xlim = c(xleft, xright), ylim = c(ybottom, ytop), 
       type = 'n', axes = F, cex.main = 1.2)
  axis(1, col = "grey", cex.axis = 1)
  axis(2, col = "grey", cex.axis = 1)
  rasterImage(as.raster(tdata.copy), xleft, ybottom, 
              xright, ytop, interpolate = FALSE)
  rm(tdata.copy)
  gc()
  return(tdata)
}