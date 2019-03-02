visualSelect <- function(ring.data, del = NULL, del.u = NULL, 
                         del.l = NULL, add = FALSE)
{
  rd.attr <- attributes(ring.data)
  x.dpi <- rd.attr$x.dpi
  seg.dn <- rd.attr$seg.dn
  seg <- rd.attr$seg
  incline <- rd.attr$incline
  py <- rd.attr$py
  py2 <- rd.attr$py2
  sample.yr <- rd.attr$sample.yr
  border.type <- rd.attr$bt
  border.color <- rd.attr$bc
  label.color <- rd.attr$lc
  label.cex <- rd.attr$lce
  x.left <- rd.attr$x.left
  x.right <- rd.attr$x.right
  img.name <- rd.attr$img.name
  py.upper <- rd.attr$py.upper
  py.lower <- rd.attr$py.lower
  bor.u <- rd.attr$bor.u
  bor.l <- rd.attr$bor.l
  sn.u <- rd.attr$sn.u
  sn.l <- rd.attr$sn.l
  year.u <- rd.attr$year.u
  year.l <- rd.attr$year.l
  bor.col <- rd.attr$bor.col
  sn <- rd.attr$sn
  year <- rd.attr$year
  dp <- x.dpi/25.4
  seg.name <- paste(img.name, '-Section', 1:seg)
  dn.list <- as.numeric(dev.list())
  check.dn <- is.element(seg.dn, dn.list)
  existing.seg <- seg.dn[check.dn]
  del.cond <- c(is.null(del), is.null(del.u), is.null(del.l))
  if (all(del.cond) & !add) 
    stop('You didn\'t perform any operation (addition or deletion)')
  if (!all(del.cond)) {
    if (incline) {
      if (!all(is.element(del.l,sn.l)))
        stop('The ring number on the lower path you entered was not correct')
      if (!all(is.element(del.u,sn.u)))
        stop('The ring number on the upper path you entered was not correct')
      if (!is.null(del.l)) bor.l <- bor.l[-del.l]
      if (!is.null(del.u)) bor.u <- bor.u[-del.u]
    } else {
      if (!all(is.element(del,sn)))
        stop('The ring number you entered was not correct')
      if (!is.null(del)) bor.col <- bor.col[-del]
    }
    for (i in existing.seg) dev.off(i)
    if (incline) {
      img.attr <- f.plot.double(ring.data, bor.u, bor.l, x.left, x.right, 
                    seg, py.upper, py.lower, dp, sample.yr, 
                    py2, nrow(ring.data), py, seg.name, 
                    border.type,border.color, label.color, label.cex)
    } else {
      img.attr <- f.plot.single(ring.data, bor.col, x.left, x.right, seg, dp, 
                    sample.yr, py2, nrow(ring.data), py, seg.name, 
                    border.type, border.color, label.color, label.cex)
    }
    seg.dn <- img.attr$seg.dn
    existing.seg <- seg.dn
  } 
  if (add) {
    if (all(!check.dn)) {
      stop(paste('All graphical windows have been closed.',
                 'You can not mark new ring boundaries.'))
    }  
    add.bor <- vector(length = 0)
    add.l <- vector(length = 0)
    add.u <- vector(length = 0)
    for (i in existing.seg) {
      dev.set(i)
      bor.xy <- locator(type = "p", pch = border.type, col = border.color)
      bor.x <- bor.xy$x
      bor.y <- bor.xy$y
      if (!is.null(bor.x)) {
        if (incline) {
          upper <- bor.y - py > 0
          lower <- bor.y - py < 0
          add.l <- c(add.l, bor.x[lower])
          add.u <- c(add.u, bor.x[upper])
        } else {
          add.bor <- c(add.bor, bor.x)
        }
      }
      dev.off(i)
    } 
    if (incline) {
      bor.l <- c(bor.l, add.l) %>% sort
      bor.u <- c(bor.u, add.u) %>% sort
    } else {
      bor.col <- c(bor.col, add.bor) %>% sort
    }
    if (incline) {
      img.attr <- f.plot.double(ring.data, bor.u, bor.l, x.left, x.right, 
        seg, py.upper, py.lower, dp, sample.yr, 
        py2, nrow(ring.data), py, seg.name, 
        border.type,border.color, label.color, label.cex)
    } else {
      img.attr <- f.plot.single(ring.data, bor.col, x.left, x.right, seg, dp, 
        sample.yr, py2, nrow(ring.data), py, seg.name, 
        border.type, border.color, label.color, label.cex)
    }
  }
  if (incline) {
    attributes(ring.data) <- c(attributes(ring.data),
      list(bor.u = bor.u, bor.l = bor.l, seg.dn = img.attr$seg.dn,
        sn.u = img.attr$bn.u, sn.l = img.attr$bn.l,
        year.u = img.attr$yn.u, year.l = img.attr$yn.l)
    )
  } else {
    attributes(ring.data) <- c(attributes(ring.data),
      list(bor.col = bor.col, seg.dn = img.attr$seg.dn,
        sn = img.attr$bn, year = img.attr$yn)
    )
  }
  return(ring.data)
}