#' @title Calibrate ring-width series
#' @export
#' @importFrom stats coef lm
#' @importFrom magick image_info image_resize 
#' @description This function can calibrate the ring-width series 
#' using arcs of inner rings.
#' @author Jingning Shi
#' @param ring.data A magick image object produced by \code{\link{ring_read}}.
#' @param inner.arc A logical value indicating whether to calibrate the 
#' ring-width series using the arcs of inner rings. See details below.
#' @param last.yr \code{NULL} or an integer giving the year of formation 
#' of the left-most ring. If \code{NULL}, border numbers (starting from 1) 
#' are used instead of years.
#' @param color Color for labels.
#' @param border.type Symbol for ring borders. See \code{pch} in 
#' \code{\link{points}} for possible values and shapes.
#' @param label.cex The magnification to be used for years or border numbers.
#' @return A data frame of the calibrated ring-width series. The measurements 
#' units are millimeters (mm)
#' @details 
#' This function allows the user to create a path, and manually mark 
#' ring borders by clicking on the graphical window. 
#' 
#' An example demonstrated with pictures can be found in the package vignette. 
#' Type \code{vignette('pith-MtreeRing')} to see this example.
#' 
#' \itemize{
#' \item
#' If \code{inner.arc = TRUE}, the ring-width series is calibrated using arcs 
#' of inner rings (Duncan, 1989).
#'  
#' \bold{Step1}. You can click the left mouse button to add a horizontal path.
#' The path should traverse an appropriate arc (read the reference below  
#' for more details).
#' 
#' \bold{Step2}. You can add three points to the selected arc by
#' left-clicking. The first point should be placed on the left endpoint of 
#' the arc, and the second point is placed on the right endpoint. 
#' 
#' After adding these two points, a vertical dashed line will be plotted 
#' automatically according to the (x,y) positions of endpoints you just added. 
#' The third points should be placed on the intersection of the vertical 
#' dashed line and the selected arc. 
#' 
#' \bold{Step3}. you are prompted to mark tree rings along the path by 
#' left-clicking on the image. Every click draws a point.
#' Note that the left endpoint of the arc will be considered as the last 
#' ring border without the need to mark it. 
#' 
#' After marking tree rings, the identification process does not automatically 
#' stop by itself. On the Windows platform, the identification process 
#' can be terminated by clicking the second button and selecting \bold{Stop} 
#' from the menu. On the MacOS system, you can press the \bold{Escape} key to 
#' terminate this process.
#' 
#' The ring-width series are corrected using formulas proposed by Duncan (1989).
#' 
#' \item
#' If \code{inner.arc = FALSE}, the user can create a path which matches 
#' the direction of wood growth. 
#' 
#' \bold{Step1}. You can add two points by left-clicking on the image. 
#' Every click draws a point.
#' A path passing through these two points will be plotted. The path should 
#' follow the rays from bark to pith.
#' 
#' \bold{Step2}. You can mark tree rings along the path by left-clicking
#' on the image. The termination of identification process is similar.
#' }
#' 
#' @references 
#' Duncan R. (1989) 
#' An evaluation of errors in tree age estimates based on increment cores 
#' in Kahikatea (Dacrycarpus dacrydiodes).
#' \emph{New Zealand Natural Sciences}
#' \bold{16(4)}, 1-37.
#' 
#' @examples 
#' img.path <- system.file("missing_pith.png", package = "MtreeRing")
#' 
#' ## Read the image:
#' t1 <- ring_read(img = img.path, dpi = 1200, plot = FALSE)
#'
#' ## Use the arcs of inner rings to calibrate ring-width series:
#' \donttest{t2 <- pith_measure(t1, inner.arc = TRUE, last.yr = 2016)}
#' 
#' ## Try another method to measure ring widths:
#' \donttest{t3 <- pith_measure(t1, inner.arc = FALSE, last.yr = 2016)}


pith_measure <- function(ring.data, inner.arc = TRUE, last.yr = NULL, 
                         color = 'black', border.type = 16, label.cex = 1.5)
{
  x.dpi <- attributes(ring.data)$x.dpi
  dp <- x.dpi / 25.4 
  is.plot <- attributes(ring.data)$plot
  if(is.plot) {
    device.number <- attributes(ring.data)$dn
    dev.set(device.number)
  } else {
    img.name <- attributes(ring.data)$img.name
    dimt <- image_info(ring.data) %>% '['(1, 2:3) %>% as.numeric
    rd.col <- dimt[1]
    rd.row <- dimt[2]
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
    dn <- as.numeric(dev.cur())
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
  pos <- add_path(inner.arc, border.type, color, label.cex)
  text.line <- pos$t
  step.number <- pos$s
  text.line <- 2 + text.line
  mtext(text = 'Mark ring boundaries along the user-defined path', 
        side = 1, line = text.line, adj = 0)
  bor.xy <- locator(type = 'p', pch = border.type, 
                    col = color, cex = label.cex)
  order.x <- order(bor.xy$x)
  bor.x <- sort(bor.xy$x)
  bor.y <- bor.xy$y[order.x]
  if (inner.arc) {
    arc.a <- pos$arc$a
    arc.b <- pos$arc$b
    arc.c <- pos$arc$c
    py <- pos$p
    bor.x <- c(bor.x, min(arc.a$x, arc.b$x))
    bor.y <- c(bor.y, py)
  }
  lenbx <- length(bor.x)
  if (is.null(last.yr)) {
    yr.list <- 1:lenbx
    text(bor.x, bor.y, yr.list, adj = c(2, 2), 
         col = color, cex = label.cex)
  } else {
      yr.list <- last.yr:(last.yr - lenbx + 1)
      text(bor.x, bor.y, yr.list, adj = c(1.25, 0.25), 
           col = color, cex = label.cex, srt = 90)
  }

  if (inner.arc) {
    l <- abs(arc.a$x - arc.b$x)
    h <- abs(arc.c$y - py)
    miss.r <- l^2/(8 * h) - h/2
    d <- bor_distance(bor.x, rep(0, lenbx)) 
    d.cum <- c(d, l/2) %>% rev %>% cumsum
    r.list <- sqrt(d.cum^2 + miss.r^2) 
    diff.r <- diff(r.list) %>% rev
    d <- c(d/dp) %>% round(digits = 2)
    diff.r <- c(diff.r/dp) %>% round(digits = 2)
    if (is.null(last.yr)) {
        df.pith <- data.frame(border.number = yr.list[-1], 
                             original.width = d, 
                            corrected.width = diff.r)
    } else {
        df.pith <- data.frame(year = yr.list[-1], 
                              original.width = d, 
                              corrected.width = diff.r)
    }
  } else {
    dis.pith <- bor_distance(bor.x, bor.y)
    dis.pith <- c(dis.pith/dp) %>% round(digits = 2)
    if (is.null(last.yr)) {
      df.pith <- data.frame(border.number = yr.list[-1], ring.width = dis.pith)
    } else {
      df.pith <- data.frame(year = yr.list[-1], ring.width = dis.pith)
    }
  }
  return(df.pith)
}


add_path <- function(inner.arc, border.type, color, label.cex) {
  coordinate <- list()
  text.line <- 4
  step.number <- 1
  if (inner.arc) {
    mtext(paste0('Step ', step.number, ': Click the left ',
      'mouse button to add a horizontal path.'), 
      side = 1, line = text.line, adj = 0)
    step1 <- locator(n = 1, type = "n")
    py <- round(step1$y) 
    abline(h = py, lty = 2, lwd = 2, col = color)
    text.line <- 2 + text.line
    mtext(paste0("Step ", step.number, " is already done."), side = 1, 
      line = text.line, adj = 0, col = "blue")
    text.line <- 2 + text.line
    step.number <- 1 + step.number
    mtext(paste0('Step ', step.number, ' : Click the left mouse button',
      ' to add three points on the arc you select.'), 
      side = 1, line = text.line, adj = 0)
    ##arc
    arc.position <- mark_arc(py, border.type, color, label.cex)
    coordinate <- c(coordinate, list(arc = arc.position, p = py))
    text.line <- 2 + text.line
    mtext(paste0("Step ", step.number, " is already done "), side = 1, 
      line = text.line, adj = 0, col = "blue")
  }  
  if (!inner.arc) {
    mtext(paste0('Step ', step.number, ' :Click the left mouse button to add ', 
      'two points. A straight line passing through ',
      'these two points will be added to the image.'), 
      side = 1, line = text.line, adj = 0)
    path.position <- inclined_path(border.type, color, label.cex)
    path.xy <- lm(path.position$y ~ path.position$x + 1)
    a <- coef(path.xy)[1]
    b <- coef(path.xy)[2]
    abline(a = a, b = b, lwd = 2, lty = 2, col = color)
    text.line <- 2 + text.line
    mtext(paste0('Step ', step.number, ' is already done.'), side = 1, 
      line = text.line, adj = 0, col = 'blue')
  }  
  coordinate <- c(coordinate, list(t = text.line, s = step.number))
}

mark_arc <- function(py, border.type, color, label.cex) {
  arc.a <- locator(1, type = "n")
  points(arc.a$x, py, pch = border.type, col = color, cex = label.cex)
  arc.b <- locator(1, type = "n")
  points(arc.b$x, py, pch = border.type, col = color, cex = label.cex)
  ab.x <- (arc.a$x + arc.b$x)/2
  abline(v = ab.x, lwd = 1, lty = 2, col = color)
  arc.c <- locator(1, type = "n")
  points(ab.x, arc.c$y, pch = border.type, col = color, cex = label.cex)
  list(a = arc.a, b = arc.b, c = arc.c)
}

inclined_path <- function(border.type, color, label.cex) {
  p1 <- locator(1, type = 'p', pch = 19)
  points(p1$x, p1$y, pch = border.type, col = color, cex = label.cex)
  p2 <- locator(1, type = 'p', pch = 19)
  points(p2$x, p2$y, pch = border.type, col = color, cex = label.cex)
  list(x = c(p1$x, p2$x), y = c(p1$y, p2$y))
}

bor_distance <- function(bor.x, bor.y) {
  diff.x <- diff(bor.x)
  diff.y <- diff(bor.y)
  dis.pith <- sqrt(diff.x^2 + diff.y^2)
  return(dis.pith)
}
