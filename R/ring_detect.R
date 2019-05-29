#' @export
#' @importFrom dplyr filter group_by select summarize
#' @importFrom measuRing graySmoothed linearDetect
#' @importFrom spatstat connected im
#' @importFrom magick image_info image_resize 
#' @importFrom imager as.cimg cannyEdges dilate_rect erode_rect imgradient 
#' mclosing_square threshold watershed
#' @title Automatic detection of tree-ring borders
#' @description This function is used to automatically detect tree ring 
#' borders along the user-defined path.
#' @author Jingning Shi
#' @param ring.data A magick image object produced by \code{\link{ring_read}}.
#' @param seg An integer specifying the number of image segments.
#' @param auto.path A logical value. If \code{TRUE}, a path is automatically 
#' created at the center of the image. If \code{FALSE}, the function allows 
#' the user to create a sub-image and a path by interactive clickings. 
#' See details below.
#' @param manual A logical value indicating whether to skip the automatic 
#' detection. If \code{TRUE}, ring borders are visually identified after 
#' creating the path. See \code{\link{ring_modify}} to learn how to mark
#' tree rings by clicking on the image.
#' @param method A character string specifying how ring borders are detected. 
#' It requires one of the following characters: \code{"watershed"}, 
#' \code{"canny"}, or \code{"lineardetect"}. See details below.
#' @param incline A logical value indicating whether to correct ring widths. 
#' If \code{TRUE}, two horizontal paths are added to the image.
#' @param sample.yr \code{NULL} or an integer giving the year of formation 
#' of the left-most ring. If \code{NULL}, use the current year.
#' @param watershed.threshold The threshold used for producing the marker 
#' image, either a numeric from 0 to 1, or the character "auto" (using the 
#' Otsu algorithm), or a character of the form "XX\%" (e.g., "58\%").
#' @param watershed.adjust A numeric used to adjust the Otsu threshold. 
#' The default is 1 which means that the threshold will not be adjusted. 
#' The sizes of early-wood regions in the marker image will reduce along 
#' with the decrease of \code{watershed.adjust}.
#' @param struc.ele1 \code{NULL} or a vector of length two specifying the 
#' width and height of the first structuring element. If \code{NULL}, the 
#' size of the structuring element is determined by the argument \code{dpi}.
#' @param struc.ele2 \code{NULL} or a vector of length two specifying the 
#' width and height of the second structuring element. If \code{NULL}, the 
#' size of the structuring element is determined by the argument \code{dpi}.
#' @param marker.correction A logical value indicating whether to relabel 
#' early-wood regions by comparing the values of their left-side neighbours.
#' @param default.canny A logical value. If \code{TRUE}, upper and lower 
#' Canny thresholds are determined automatically.
#' @param canny.t1 A numeric giving the threshold for weak edges.
#' @param canny.t2 A numeric giving the threshold for strong edges.
#' @param canny.smoothing An integer specifying the degree of smoothing.
#' @param canny.adjust A numeric used as a sensitivity control factor for 
#' the Canny edge detector. The default is 1 which means that the sensitivity 
#' will not be adjusted. The number of detected borders will reduce along 
#' with the increase of this value.
#' @param path.dis A numeric specifying the perpendicular distance between 
#' two paths when the argument \code{incline = TRUE}. The unit is in mm.
#' @param origin A numeric specifying the origin in smoothed gray to find 
#' ring borders. See \code{\link{ringBorders}} from the package 
#' \code{\link{measuRing}}.
#' @param border.color Color for ring borders.
#' @param border.type Symbol for ring borders. See \code{pch} in 
#' \code{\link{points}} for possible values and their shapes.
#' @param label.color Color for years and border numbers.
#' @param label.cex The magnification to be used for years and border numbers.
#' @return A matrix (grayscale image) or array (color image) 
#' representing the tree-ring image.
#' @references Soille, P., Misson, L. (2001)
#' Tree ring area measurements using morphological image analysis.
#' \emph{Canadian Journal of Forest Research}
#' \bold{31}, 1074-1083. {doi: 10.1139/cjfr-31-6-1074}
#' 
#' Lara, W., Bravo, F., Sierra, C.A. (2015)
#' measuRing: An R package to measure tree-ring widths from scanned images.
#' \emph{Dendrochronologia}
#' \bold{34}, 43-50. {doi: 10.1016/j.dendro.2015.04.002}
#' 
#' @details 
#' If \code{auto.path = FALSE}, the user can create a rectangular sub-image 
#' and a horizontal path by interactively clicking on the tree ring image. 
#' The automatic detection will be performed within this rectangular 
#' sub-image. 
#' To create a sub-image and a path, follow these steps.
#' \itemize{
#'   \item
#'   Step 1. Select the left and right edges of the rectangle
#'   
#'   The user can point the mouse at any 
#'   desired locations and click the left mouse button to add each edge. 
#'   
#'   \item
#'   Step 2. Select the top and bottom edges of the rectangle
#'   
#'   The user can point the mouse at any desired locations and click the 
#'   left mouse button to add each edge. The width of the rectangle is 
#'   defined as the distance between the top and bottom edges, and should 
#'   not be unnecessarily large to reduce time consumption and memory usage. 
#'   Creating a long and narrow rectangle if possible.
#'   
#'   \item
#'   Step 3. Create a path
#'   
#'   After creating the rectangular sub-image, the user can add a horizontal 
#'   path by left-clicking on the sub-image (generally at the center of the 
#'   sub-image, try to choose a clean defect-free area). Ring borders and 
#'   other markers are plotted along this path. If \code{incline = TRUE}, 
#'   two paths are added simultaneously.
#' }
#' 
#' After creating the sub-image and the path, this function will open several 
#' graphics windows and plot detected ring borders on image segments. The 
#' number of image segments is controlled by the argument \code{seg}.
#' 
#' Argument \code{method} determines how ring borders are identified. 
#' \itemize{
#'   \item
#'   If \code{method = "watershed"}, this function uses the watershed algorithm 
#'   to obtain ring borders (Soille and Misson, 2001).
#'   \item
#'   If \code{method = "canny"}, this function uses the Canny algorithm 
#'   to detect borders.
#'   \item
#'   If \code{method = "lineardetect"}, a linear detection algorithm from the 
#'   package \code{\link{measuRing}} is used to identify ring borders (Lara 
#'   et al., 2015). Note that \code{incline = TRUE} is not supported in this 
#'   mode, and path will be automatically created at the center of the image. 
#' }
#' If the argument \code{method = "watershed"} or \code{"canny"}, the original 
#' image is processed by morphological openings and closings using rectangular 
#' structuring elements of increasing size before detecting borders. The first 
#' small structuring element is used to remove smaller dark spots in early 
#' wood regions, and the second large structuring element is used to remove 
#' light strips in late wood regions. More details about morphological 
#' processing can be found at Soille and Misson (2001).
#' 

#' @note This function uses \code{\link{locator}} to record mouse 
#' positions so it only works on "X11", "windows" and "quartz" devices.
#' @examples
#' img.path <- system.file("001.png", package = "MtreeRing")
#' 
#' ## Read a tree ring image:
#' t1 <- ring_read(img = img.path, dpi = 1200, plot = FALSE)
#' 
#' ## Split a long core sample into 3 pieces to
#' ## get better display performance and use the
#' ## watershed algorithm to detect ring borders:
#' t2 <- ring_detect(t1, seg = 3, method = 'watershed', border.color = 'green')
#' 


ring_detect <- function(ring.data, seg = 1, auto.path = TRUE, manual = FALSE,
                        method = 'canny', incline = FALSE, sample.yr = NULL, 
                        watershed.threshold = 'auto', watershed.adjust = 0.8, 
                        struc.ele1 = NULL, struc.ele2 = NULL, 
                        marker.correction = FALSE, default.canny = TRUE, 
                        canny.t1, canny.t2, canny.smoothing = 2, 
                        canny.adjust = 1.4, path.dis = 1, origin = 0, 
                        border.color = 'black', border.type = 16, 
                        label.color = 'black', label.cex = 1.2)
{
  if(!is.logical(auto.path))
    stop("The argument 'auto.path' should be a logical vector of length one")
  if(!is.logical(manual))
    stop("The argument 'manual' should be a logical vector of length one")
  if(!is.logical(incline))
    stop("The argument 'incline' should be a logical vector of length one")
  check.method <- method %in% c('canny', 'watershed', 'lineardetect') 
  if (length(method) >= 2) 
    stop("The argument 'method' should be a character vector of length one")
  if (!check.method)
    stop(paste("The argument 'method' should be one of the",
               "following strings: canny, watershed, lineardetect"))
  if (!is.character(method))
    stop("The argument 'method' should be a character vector of length one")
  if (method == "lineardetect" & incline) 
    stop("The linear detection can only create one path")
  if (!is.numeric(seg))
    stop("The argument 'seg' should be a integer vector of length one")
  RGB <- attributes(ring.data)$RGB
  x.dpi <- attributes(ring.data)$x.dpi
  dp <- x.dpi / 25.4 
  if (x.dpi <= 300 & !manual)
    stop('The automatic detection requires a minimum of 300 dpi')
  is.plot <- attributes(ring.data)$plot
  if(is.plot) {
    device.number <- attributes(ring.data)$dn
    dev.set(device.number)
    dimt <- attributes(ring.data)$dimt
    rd.col <- dimt[1]
    rd.row <- dimt[2]
  } else {
    img.name <- attributes(ring.data)$img.name
    dimt <- image_info(ring.data) %>% '['(1, 2:3) %>% as.numeric
    rd.col <- dimt[1]
    rd.row <- dimt[2]
    attributes(ring.data) <- c(attributes(ring.data), list(dimt = dimt))
    device.number <- 'no plot'
    
    if (!auto.path) {
      if (rd.col * rd.row >= 1.2e+07) {
        resize.ratio <- 300 / x.dpi
        resize.str <- paste0(round(rd.col*resize.ratio), 'x', 
          round(rd.row*resize.ratio))
        tdata.copy <- image_resize(ring.data, resize.str)
      } else{
        tdata.copy <- ring.data
      }
      dev.new()
      if (names(dev.cur()) == "RStudioGD") dev.new()
      device.number <- as.numeric(dev.cur())
      xleft <- 0
      ybottom <- 0
      xright <- rd.col
      ytop <- rd.row
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
    }
  }

  xy.position <- create_path(auto.path, method, incline, path.dis,
                             dp, rd.col, rd.row, label.color)
  px2 <- xy.position[1]
  px3 <- xy.position[2]
  py2 <- xy.position[3]
  py3 <- xy.position[4]
  py <- xy.position[5]
  if (incline) {
    py.upper <- xy.position[6]
    py.lower <- xy.position[7]
  }
  img.range <- paste0(as.character(px3 - px2 + 1), 'x',
                      as.character(py3 - py2 + 1), '+',
                      as.character(px2 - 1), '+',
                      as.character(rd.row - py3 - 1))
  img.crop <- image_crop(ring.data, img.range)
  rd.martix <- img.crop[[1]]
  rd.m.array <- magick2array(rd.martix)
  colInd <- column_indices(px2, px3, seg)
  x.left <- colInd$left
  x.right <- colInd$right
  if (is.null(sample.yr)) {
    sample.yr <- Sys.Date() %>% as.character %>% strtrim(4) %>% as.numeric
    warning('The sampling year is set to the current year')
  }
  if (!manual) {
    seg.data <- convert2gray(rd.m.array, RGB)
    if (method == 'watershed') {
      seg.mor <- morphology_process(seg.data, struc.ele1, struc.ele2, x.dpi)
      black.hat <- hat(seg.mor, x.dpi, watershed.threshold, watershed.adjust)
      marker.img <- conn_calc(black.hat, marker.correction)
      seg.data <- watershed_process(marker.img, seg.mor)
    }  
    if (method == 'canny') {
      seg.mor <- morphology_process(seg.data, struc.ele1, struc.ele2, x.dpi)
      if (default.canny) {
        canny.seg <- cannyEdges(as.cimg(seg.mor), alpha = canny.adjust, 
                                sigma = canny.smoothing)
      } else {
        canny.seg <- cannyEdges(as.cimg(seg.mor), t1 = canny.t1, t2 = canny.t2,
                                alpha = canny.adjust, sigma = canny.smoothing)
      }
      seg.data <- canny.seg[,, 1, 1]
    }
    if (incline) {
      bor.l <- border_det(seg.data, py3 - py.lower, dp) + px2 - 1
      bor.u <- border_det(seg.data, py3 - py.upper, dp) + px2 - 1
    } else {
      bor.col <- border_det(seg.data, py3 - py, dp) + px2 - 1
    }
    if (method == 'lineardetect') {
      attributes(seg.data)['image'] <- 'img'
      smoothed <- graySmoothed(seg.data, ppi = x.dpi, rgb = RGB)
      bor.col <- linearDetect(smoothed, origin = origin)
      bor.col <- bor.col + px2 - 1
    }
  }
  img.name <- attributes(ring.data)$img.name
  seg.name <- paste(img.name, '-Section', 1:seg)
  if (!manual) {
    if (incline) {
      img.attr <- two_paths_plot(rd.m.array, bor.u, bor.l, x.left, x.right,
                    seg, py.upper, py.lower, dp, sample.yr,
                    py2, nrow(rd.m.array), py, seg.name, 
                    border.type,border.color, label.color, label.cex)
    } else {
      img.attr <- single_path_plot(rd.m.array, bor.col, x.left, x.right, seg, 
                    dp, sample.yr, py2, nrow(rd.m.array), py, seg.name, 
                    border.type, border.color, label.color, label.cex)
    }
    seg.dn <- img.attr$seg.dn
  } else {
    seg.dn <- vector(length = 0)
    for (i in 1:seg) {
      dev.new()
      if (names(dev.cur()) == "RStudioGD") dev.new()
      img_plot(rd.m.array, x.left[i], x.right[i], 
            py2, nrow(rd.m.array), x.left[1], F)
      seg.dn[i] <- dev.cur() %>% as.numeric
      title(main = seg.name[i], cex.main = 1.5, line = -1)
      abline(h = py, lty = 2, col = label.color)
      if (incline) {
        abline(h = py, lty = 1, col = label.color)
        abline(h = py.lower, lty = 2, col = label.color)
        abline(h = py.upper, lty = 2, col = label.color)
      }
    }
  }
  attributes(rd.m.array) <- c(attributes(rd.m.array), 
    list(x.dpi = x.dpi, RGB = RGB, dn = device.number, seg.dn = seg.dn,
         seg = seg, incline = incline, py = py, py2 = py2, py3 = py3,
         px2 = px2, px3 = px3, sample.yr = sample.yr, path.dis = path.dis, 
         bt = border.type, bc = border.color, 
         lc = label.color, lce = label.cex, 
         x.left = x.left, x.right = x.right, img.name = img.name)
    )
  if (incline) 
    attributes(rd.m.array) <- c(attributes(rd.m.array), 
                                list(py.upper = py.upper, 
                                     py.lower = py.lower))
  if (!manual) {
    if (incline) {
      attributes(rd.m.array) <- c(attributes(rd.m.array), 
                                  list(bor.u = bor.u, 
                                       bor.l = bor.l,
                                        sn.u = img.attr$bn.u, 
                                        sn.l = img.attr$bn.l, 
                                      year.u = img.attr$yn.u, 
                                      year.l = img.attr$yn.l))
    } else {
      attributes(rd.m.array) <- c(attributes(rd.m.array), 
                                  list(bor.col = bor.col, 
                                            sn = img.attr$bn, 
                                          year = img.attr$yn))
    }
  } else {
    if (incline) {
      attributes(rd.m.array) <- c(attributes(rd.m.array), 
                                  list(bor.u = vector(), bor.l = vector()))
    } else {
      attributes(rd.m.array) <- c(attributes(rd.m.array), 
                                  list(bor.col = vector()))
    }
    
    # run ring_modify
    rd.m.array <- ring_modify(rd.m.array, add = T)
  }
  return(rd.m.array)
}

draw_border1 <- function(rd.row, rd.col, label.color) {
  step <- locator(n = 1, type = 'n')
  px <- round(step$x)
  if (px <= 0) px <- 1
  if (px >= rd.col) px <- rd.col
  lines(c(px, px), c(1, rd.row), lty = 2, lwd = 2, col = label.color)
  return(px)
}

draw_border2 <- function(rd.row, rd.col, px2, px3, label.color) {
  step <- locator(n = 1, type = 'n')
  py <- round(step$y)
  if (py <= 0) py <- 0
  if (py >= rd.row) py <- rd.row - 1
  lines(c(px2, px3), c(py, py), lty = 2, lwd = 2, col = label.color)
  return(py)
}

create_path <- function(auto.path, method, incline, path.dis,
                        dp, rd.col, rd.row, label.color) 
{
  coordinate <- vector()
  if (auto.path) {
    py <- rd.row / 2 %>% round
    coordinate <- c(coordinate, 1, rd.col, 0, rd.row - 1, py)
    if (incline) {  
      number.of.pixels <- round((path.dis / 2) * dp)
      py.upper <- py + number.of.pixels
      if (py.upper >= rd.row - 1) 
        stop('The y-position of the path is out of range')
      py.lower <- py - number.of.pixels
      coordinate <- c(coordinate, py.upper, py.lower)
    }
    return(coordinate)
  }
  text.line <- 4
  step.number <- 1
  text.s1 <- paste0('Step ', step.number, ': Select the left and right edges',
    ' of the sub-image. Click the left mouse button to add each edge.')
  mtext(text.s1, side = 1, line = text.line, adj = 0)
  px2 <- draw_border1(rd.row, rd.col, label.color)
  px3 <- draw_border1(rd.row, rd.col, label.color)
  cpx <- c(px2, px3)
  px2 <- min(cpx)
  px3 <- max(cpx)
  coordinate <- c(coordinate, px2, px3)
  text.line <- 2 + text.line
  mtext(paste('Step', step.number, 'has already done.'),
    side = 1, line = text.line, adj = 0, col = 'blue')
  text.line <- 2 + text.line
  step.number <- 1 + step.number
  text.s2 <- paste0('Step ', step.number, ': Select the top and bottom edges',
    ' of the sub-image. Click the left mouse button to add each edge.')
  mtext(text.s2, side = 1, line = text.line, adj = 0)
  py2 <- draw_border2(rd.row, rd.col, px2, px3, label.color)
  py3 <- draw_border2(rd.row, rd.col, px2, px3, label.color)
  cpy <- c(py2, py3)
  py2 <- min(cpy)
  py3 <- max(cpy)
  coordinate <- c(coordinate, py2, py3)
  if (incline) {
    if (path.dis * dp >= (py3 - py2))
      stop('Increase the width of the sub-image or ',
           'decrease the value of \'path.dis\'')
  }
  text.line <- 2 + text.line
  mtext(paste0('Step ', step.number, ' has already done '),
    side = 1, line = text.line, adj = 0, col = 'blue')
  text.line <- 2 + text.line
  step.number <- 1 + step.number
  if (method != 'lineardetect') {
    mtext(paste0('Step ', step.number, ': Add a horizontal ',
      'path by left-clicking on the sub-image.'),
      side = 1, line = text.line, adj = 0)
    step1 <- locator(n = 1, type = 'n')
    py <- round(step1$y)
    coordinate <- c(coordinate, py)
    if (py <= py2 | py >= py3)
      stop('The y-position of the path is out of range')
    if (incline) {  
      number.of.pixels <- round((path.dis / 2) * dp)
      py.upper <- py + number.of.pixels
      if (py.upper >= py3)
        stop('The y-position of the upper path is out of range')
      abline(h = py.upper, lty = 2, lwd = 2, col = label.color)
      py.lower <- py - number.of.pixels
      if (py.lower <= py2)
        stop('The y-position of the lower path is out of range')
      abline(h = py.lower, lty = 2, lwd = 2, col = label.color)
      abline(h = py, lty = 1, lwd = 2, col = label.color)
      coordinate <- c(coordinate, py.upper, py.lower)
    } else { 
      abline(h = py, lty = 1, lwd = 2, col = label.color)
    }
  } else {
    py <- (py3 + py2) / 2 %>% round
    coordinate <- c(coordinate, py)
  }
  return(coordinate)
}

convert2gray <- function(rd.m.array, RGB) {
  rd.channel <- dim(rd.m.array)
  if (length(rd.channel) == 2) {
    seg.data <- rd.m.array[, ]
  } else {
    if (rd.channel[3] == 2)
      seg.data <- rd.m.array[, , 1]
    if (rd.channel[3] >= 3) {
      seg.data <- apply(rd.m.array[, , 1:3], 1, function(x) x %*% RGB) %>% t
    }
  }
  return(seg.data)
}

magick2array <- function(rd.martix) {
  rd.channel <- dim(rd.martix)[1]
  hex2dec <- function(rd.martix) apply(rd.martix, 1, as.numeric)
  if (rd.channel == 1) {
    rd.m.array <- hex2dec(rd.martix[1, , ])
  } else {
    rd.m.array <- array(0, dim = rev(dim(rd.martix)))
    for (i in 1:rd.channel) {
      rd.m.array[, , i] <- hex2dec(rd.martix[i, , ])
    }
  }
  rd.m.array <- rd.m.array/255
}

column_indices <- function(px2, px3, seg) {
  if (seg == 1) {
    x.left <- px2
    x.right <- px3
  } else {
    tot.col <- (px3 - px2) + 1
    x.left <- px2
    for (i in 2:seg) {
      x.left[i] <- x.left[i - 1] + tot.col %/% seg
    }
    x.right <- x.left[-1] - 1
    x.right[seg] <- px3
  } 
  list(left = x.left, right = x.right)
}

