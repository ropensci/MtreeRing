#' @export
#' @title Edit ring borders visually
#' @description This function can remove existing ring borders 
#' or add new borders.
#' @author Jingning Shi
#' @param ring.data A matrix or array produced by \code{\link{ring_detect}}.
#' @param del A numeric vector giving the border numbers to be removed.
#' @param del.u A numeric vector giving the border numbers to be removed 
#' on the upper path.
#' @param del.l A numeric vector giving the border numbers to be removed 
#' on the lower path.
#' @param add A logical value indicating whether to add new ring borders.
#' @return A matrix (grayscale image) or array (color image)
#' representing the tree ring image.
#' @details 
#' This function is used to remove existing ring borders, or to add new 
#' borders by interactively clicking on the image segments.
#' 
#' If the user creates one path (\code{incline = FALSE}), the argument 
#' \code{del} is used to remove ring borders. If the user creates two paths 
#' (\code{incline = TRUE}), arguments \code{del.u} and \code{del.l} are used 
#' to remove ring borders.
#' 
#' If \code{add = TRUE}, graphics windows opened by \code{\link{ring_detect}}
#' will be activated sequentially. When a graphics window is activated, 
#' the user can add new borders by left-clicking the mouse along the path.
#' Every click draws a point representing the ring border.
#' Type \code{vignette('detection-MtreeRing')} to see 
#' an example of adding ring borders.
#' 
#' The identification process does not automatically stop by itself.
#' 
#' \itemize{
#'   \item
#'   On the Windows system, the identification process can be terminated by 
#'   pressing the right mouse button and selecting \bold{Stop} from the menu.
#'   \item 
#'   On the MacOS system, for a X11 device the identification process is 
#'   terminated by pressing any mouse button other than the first, and for a 
#'   quartz device this process is terminated by pressing the \bold{ESC} key.
#' }
#' 
#' Once the user terminates the identification process, the current 
#' graphics window will be closed automatically, and the graphics window of
#' the following segment is activated. When all graphics windows are closed,
#' \code{ring_modify} will re-open graphics windows and plot new borders.
#' 
#' This function can perform both deletion and addition in one call.
#' The removal of ring borders takes precedence over addition.
#' @examples
#' img.path <- system.file("001.png", package = "MtreeRing")
#' 
#' ## Read a tree ring image:
#' t1 <- ring_read(img = img.path, dpi = 1200)
#'
#' ## Split a long core sample into 3 pieces to
#' ## get better display performance and use the
#' ## watershed algorithm to detect ring borders:
#' t2 <- ring_detect(ring.data = t1, seg = 3, method = 'watershed')
#'
#' ## Do not modify t2, but create a new array object t3. 
#' ## Remove some borders without adding new borders:
#' t3 <- ring_modify(ring.data = t2, del = c(1, 3, 5, 19:21), add = FALSE)
#' 

ring_modify <- function(ring.data, del = NULL, del.u = NULL, 
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
        stop('The border number on the lower path you entered was not correct')
      if (!all(is.element(del.u,sn.u)))
        stop('The border number on the upper path you entered was not correct')
      if (!is.null(del.l)) bor.l <- bor.l[-del.l]
      if (!is.null(del.u)) bor.u <- bor.u[-del.u]
    } else {
      if (!all(is.element(del,sn)))
        stop('The border number you entered was not correct')
      if (!is.null(del)) bor.col <- bor.col[-del]
    }
    for (i in existing.seg) dev.off(i)
    if (incline) {
      img.attr <- two_paths_plot(ring.data, bor.u, bor.l, x.left, x.right, 
                    seg, py.upper, py.lower, dp, sample.yr, 
                    py2, nrow(ring.data), py, seg.name, 
                    border.type,border.color, label.color, label.cex)
    } else {
      img.attr <- single_path_plot(ring.data, bor.col, x.left, x.right, seg, 
                    dp, sample.yr, py2, nrow(ring.data), py, seg.name, 
                    border.type, border.color, label.color, label.cex)
    }
    seg.dn <- img.attr$seg.dn
    existing.seg <- seg.dn
  } 
  if (add) {
    if (all(!check.dn)) {
      stop(paste('All graphics windows have been closed.',
                 'You can not mark ring borders.'))
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
      img.attr <- two_paths_plot(ring.data, bor.u, bor.l, x.left, x.right, 
        seg, py.upper, py.lower, dp, sample.yr, 
        py2, nrow(ring.data), py, seg.name, 
        border.type,border.color, label.color, label.cex)
    } else {
      bor.col <- c(bor.col, add.bor) %>% sort
      img.attr <- single_path_plot(ring.data, bor.col, x.left, x.right, seg, 
        dp, sample.yr, py2, nrow(ring.data), py, seg.name, 
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
