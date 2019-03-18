#' @title Read and plot a tree-ring image file
#' @export
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @importFrom tiff readTIFF
#' @importFrom bmp read.bmp
#' @importFrom magrittr "%>%"
#' @importFrom magick image_crop image_info image_read image_convert image_rotate image_resize
#' @importFrom grDevices as.raster dev.cur dev.new dev.set dev.off dev.list
#' @importFrom graphics abline axis layout lines locator mtext par plot points rasterImage segments text title
#' @description This function can read an image file from the hard disk and plot it in a newly-opened graphical device.
#' @author Jingning Shi
#' @param img A character string indicating the path of the image file. Supported formats include png, tiff, jpg and bmp.
#' @param dpi An integer specifying the dpi of the image file. A minimum of 300 dpi is required when running automatic detection.
#' @param RGB A numeric vector of length 3 giving the weight of RGB color channels.
#' @param rotate An integer specifying how many degrees to rotate (clockwise). It requires one of the following values: \code{0}, \code{90}, \code{180} or \code{270}.
#' @param magick A logical value. If \code{TRUE}, the package \code{magick} is used to read images whose file sizes are over 10MB.
#' @return A magick image object containing the image data.
#' @details 
#' Proper image preparation has a great influence on the measurement of ring widths. A tree-ring image should not contain irrelevant or redundant features, such as wooden mounts where cores are glued. The larger the file size of an image, the slower the image processing operation will be.
#' 
#' Pith side of a tree-ring sample should be placed on the right side of the graphical window. Use the argument \code{rotate} to change its position.
#' 
#' It is highly recommended to use the default value \code{magick = TRUE}, because the package \code{magick} can significantly reduce the memory usage.
#' @examples
#' img.path <- system.file("001.png", package = "MtreeRing")
#' 
#' ## Read and plot the image:
#' t1 <- imgInput(img = img.path, dpi = 1200)

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
              xright, ytop, interpolate = TRUE)
  rm(tdata.copy)
  gc()
  return(tdata)
}