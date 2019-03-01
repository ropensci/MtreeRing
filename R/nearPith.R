nearPith <- function(ring.data, inner.arc = TRUE, last.yr = NULL, 
                     color = 'black', border.type = 16, label.cex = 1.5)
{
  img.name <- attributes(ring.data)$img.name  
  device.number <- attributes(ring.data)$dn
  x.dpi <- attributes(ring.data)$x.dpi
  dp <- x.dpi / 25.4 
  dev.set(device.number)
  dimt <- attributes(ring.data)$dimt
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
    arc.a <- locator(1, type = "n")
    points(arc.a$x, py, pch = border.type, col = color, cex = label.cex)
    arc.b <- locator(1, type = "n")
    points(arc.b$x, py, pch = border.type, col = color, cex = label.cex)
    ab.x <- (arc.a$x + arc.b$x)/2
    abline(v = ab.x, lwd = 1, lty = 2, col = color)
    arc.c <- locator(1, type = "n")
    points(ab.x, arc.c$y, pch = border.type, col = color, cex = label.cex)
    text.line <- 2 + text.line
    mtext(paste0("Step ", step.number, " is already done "), side = 1, 
          line = text.line, adj = 0, col = "blue")
  }  
  if (!inner.arc) {
    mtext(paste0('Step ', step.number, ' :Click the left mouse button to add ', 
                 'two points. A straight line passing through ',
                 'these two points will be added to the image.'), 
          side = 1, line = text.line, adj = 0)
    p1 <- locator(1, type = 'p', pch = 19)
    points(p1$x, p1$y, pch = border.type, col = color, cex = label.cex)
    p2 <- locator(1, type = 'p', pch = 19)
    points(p2$x, p2$y, pch = border.type, col = color, cex = label.cex)
    path.x <- c(p1$x, p2$x)
    path.y <- c(p1$y, p2$y)
    path.xy <- lm(path.y ~ path.x + 1)
    a = coef(path.xy)[1]
    b = coef(path.xy)[2]
    abline(a = a, b = b, lwd = 2, lty = 2, col = color)
    text.line <- 2 + text.line
    mtext(paste0('Step ', step.number, ' is already done.'), side = 1, 
          line = text.line, adj = 0, col = 'blue')
  }  
  text.line <- 2 + text.line
  mtext(text = 'Please mark ring boundaries along the user-defined path', 
        side = 1, line = text.line, adj = 0)
  bor.xy <- locator(type = 'p', pch = border.type, col = color, cex = label.cex)
  order.x <- order(bor.xy$x)
  bor.x <- sort(bor.xy$x)
  bor.y <- bor.xy$y[order.x]
  if (inner.arc) {
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
  bor.distance <- function(bor.x, bor.y) {
    diff.x <- diff(bor.x)
    diff.y <- diff(bor.y)
    dis.pith <- sqrt(diff.x^2 + diff.y^2)
    return(dis.pith)
  }
  if (inner.arc) {
    l <- abs(arc.a$x - arc.b$x)
    h <- abs(arc.c$y - py)
    miss.r <- l^2/(8 * h) - h/2
    d <- bor.distance(bor.x, rep(0, lenbx)) 
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
    dis.pith <- bor.distance(bor.x, bor.y)
    dis.pith <- c(dis.pith/dp) %>% round(digits = 2)
    if (is.null(last.yr)) {
      df.pith <- data.frame(border.number = yr.list[-1], ring.width = dis.pith)
    } else {
      df.pith <- data.frame(year = yr.list[-1], ring.width = dis.pith)
    }
  }
  return(df.pith)
}