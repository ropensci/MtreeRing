autoDetect <- function(ring.data, seg = 1, auto.path = TRUE, manual = FALSE, 
                       method = 'canny', incline = FALSE, sample.yr = NULL, 
                       watershed.threshold = 'auto', watershed.adjust = 0.8, 
                       struc.ele1 = NULL, struc.ele2 = NULL, 
                       marker.correct = FALSE, default.canny = TRUE, canny.t1, 
                       canny.t2, canny.smoothing = 2, canny.adjust = 1.4, 
                       path.dis = 1, origin = 0, border.color = 'black',
                       border.type = 16, label.color = 'black', label.cex = 1.2)
{
  if (length(method) >= 2) 
    stop("The argument 'method' should be a character vector of length one")
  if (method == "lineardetect" & incline) 
    stop("The linear detection can only create one path")
  device.number <- attributes(ring.data)$dn
  dev.set(device.number)
  x.dpi <- attributes(ring.data)$x.dpi
  dp <- x.dpi / 25.4 
  if (x.dpi <= 300 & !manual)
    stop('The automatic detection requires a minimum of 300 dpi')
  dimt <- attributes(ring.data)$dimt
  rd.col <- dimt[1]
  rd.row <- dimt[2]
  RGB <- attributes(ring.data)$RGB
  if (!auto.path) {
    text.line <- 4
    step.number <- 1
    text.s1 <- paste0('Step ', step.number, ': Select the left and',
                      ' right edges of the rectangular sub-image. ',
                      'Click the left mouse button to add each edge.')
    mtext(text.s1, side = 1, line = text.line, adj = 0)
    step2 <- locator(n = 1, type = 'n')
    px2 <- round(step2$x)
    if (px2 <= 0) px2 <- 1
    if (px2 >= rd.col) px2 <- rd.col
    lines(c(px2, px2), c(1, rd.row), lty = 2, lwd = 2, col = label.color)
    step3 <- locator(n = 1, type = 'n')
    px3 <- round(step3$x)
    if (px3 <= 0) px3 <- 1
    if (px3 >= rd.col) px3 <- rd.col
    lines(c(px3, px3), c(1, rd.row), lty = 2, lwd = 2, col = label.color)
    px.sort <- sort(c(px2, px3))
    px2 <- px.sort[1]
    px3 <- px.sort[2]
    text.line <- 2 + text.line
    mtext(paste('Step', step.number, 'has already done.'), 
          side = 1, line = text.line, adj = 0, col = 'blue')
    text.line <- 2 + text.line
    step.number <- 1 + step.number
    text.s2 <- paste0('Step ', step.number, ': Select the top and ',
                      'bottom edges of the rectangular sub-image. ',
                      'Click the left mouse button to add each edge.')
    mtext(text.s2, side = 1, line = text.line, adj = 0)
    step4 <- locator(n = 1, type = 'n')
    py2 <- round(step4$y)
    if (py2 <= 0) py2 <- 0
    if (py2 >= rd.row) py2 <- rd.row - 1 
    lines(c(px2, px3), c(py2, py2), lty = 2, lwd = 2, col = label.color)
    step5 <- locator(n = 1, type = 'n')
    py3 <- round(step5$y)
    if (py3 <= 0) py3 <- 0
    if (py3 >= rd.row) py3 <- rd.row - 1 
    lines(c(px2, px3), c(py3, py3), lty = 2, lwd = 2, col = label.color)
    py.sort <- sort(c(py2, py3))
    py2 <- py.sort[1]
    py3 <- py.sort[2] 
    if (incline) {
      if (path.dis * dp >= (py3 - py2)) 
        stop('Please increase the width of the rectangular sub-image',
             ' or decrease the value of the argument \'path.dis\'')
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
      } else { 
        abline(h = py, lty = 1, lwd = 2, col = label.color)
      }
    } else {
      py <- (py3 + py2) / 2 %>% round
    }
  } else {
    px2 <- 1
    px3 <- rd.col
    py2 <- 0
    py3 <- rd.row - 1
    py <- (py3 + py2) / 2 %>% round
    if (incline) {  
      number.of.pixels <- round((path.dis / 2) * dp)
      py.upper <- py + number.of.pixels
      if (py.upper >= py3) 
        stop('The y-position of the upper path is out of range')
      py.lower <- py - number.of.pixels
      if (py.lower <= py2) 
        stop('The y-position of the lower path is out of range')
    }
  }
  img.range <- paste0(as.character(px3 - px2 + 1), 'x', 
                      as.character(py3 - py2 + 1), '+',
                      as.character(px2 - 1), '+', 
                      as.character(rd.row - py3 - 1))
  img.crop <- image_crop(ring.data, img.range)
  rd.martix <- img.crop[[1]]
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
  if (is.null(sample.yr))
    sample.yr <- Sys.Date() %>% as.character %>% strtrim(4) %>% as.numeric
  if (!manual) {
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
    if (method == 'watershed') {
      seg.mor <- f.morphological(seg.data, struc.ele1, struc.ele2, x.dpi)
      black.hat <- hat(seg.mor, x.dpi, watershed.threshold, watershed.adjust)
      marker.img <- water.im(black.hat, marker.correct)
      seg.data <- watershed.im(marker.img, seg.mor)
    }  
    if (method == 'canny') {
      seg.mor <- f.morphological(seg.data, struc.ele1, struc.ele2, x.dpi)
      if (default.canny) {
        canny.seg <- cannyEdges(as.cimg(seg.mor), alpha = canny.adjust, 
                                sigma = canny.smoothing)
      } else {
        canny.seg <- cannyEdges(as.cimg(seg.mor), t1=canny.t1, t2=canny.t2,
                                alpha = canny.adjust, sigma = canny.smoothing)
      }
      seg.data <- canny.seg[, , 1, 1]
    } 
    if (method == 'lineardetect') {
      attributes(seg.data)['image'] <- 'img'
      smoothed <- graySmoothed(seg.data, ppi = x.dpi, rgb = RGB)
      bor.col <- linearDetect(smoothed, origin = origin)
    }
    if (incline) {
      bor.l <- f.border(seg.data, py3 - py.lower, dp) + px2 - 1
      bor.u <- f.border(seg.data, py3 - py.upper, dp) + px2 - 1
    } else {
      if (method == 'lineardetect') {
        bor.col <- bor.col + px2 -1
      } else {
        bor.col <- f.border(seg.data, py3 - py, dp) + px2 - 1
      }
    }
  }
  img.name <- attributes(ring.data)$img.name  
  seg.name <- paste(img.name, '-Section', 1:seg)
  if (!manual) {
    if (incline) {
      img.attr <- f.plot.double(rd.m.array, bor.u, bor.l, x.left, x.right, 
                    seg, py.upper, py.lower, dp, sample.yr, 
                    py2, nrow(rd.m.array), py, seg.name, 
                    border.type,border.color, label.color, label.cex)
    } else {
      img.attr <- f.plot.single(rd.m.array, bor.col, x.left, x.right, seg, dp, 
                    sample.yr, py2, nrow(rd.m.array), py, seg.name, 
                    border.type, border.color, label.color, label.cex)
    }
    seg.dn <- img.attr$seg.dn
  } else {
    seg.dn <- vector(length = 0)
    for (i in 1:seg) {
      dev.new()
      f.img.middle(rd.m.array, x.left[i], x.right[i], 
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
         bt = border.type, bc = border.color, lc = label.color, lce = label.cex, 
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
  }
  return(rd.m.array)
}