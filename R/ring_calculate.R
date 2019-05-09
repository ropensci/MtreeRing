#' @title Generate a ring-width series
#' @export
#' @description This function can calculate ring widths according to 
#' coordinates of detected ring borders.
#' @author Jingning Shi
#' @param ring.data A matrix or array produced by \code{\link{ring_detect}} 
#' or \code{\link{ring_modify}}.
#' @param seriesID A character string specifying the column name of 
#' the ring-width series.
#' @return A data frame. The series ID is the column name 
#' and years are row names. The measurements units are millimeters (mm).
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
#' ## Calculate ring widths from the attribute list of t2:
#' rw.df <- ring_calculate(ring.data = t2, seriesID = "940220")

ring_calculate <- function(ring.data, seriesID)
{
  rd.attr <- attributes(ring.data)
  x.dpi <- rd.attr$x.dpi
  dp <- x.dpi / 25.4
  incline <- rd.attr$incline
  sample.yr <- rd.attr$sample.yr
  path.dis <- rd.attr$path.dis
  bor.u <- rd.attr$bor.u
  bor.l <- rd.attr$bor.l
  bor.col <- rd.attr$bor.col
  if (!incline) {
    bx <- sort(bor.col)
    lenbx <- length(bx)
    if (lenbx <= 1)
      stop('A minimum of two ring borders on each path ',
           'was required to generate a ring-width series')
    diff.col.num <- diff(bx)
    rw <- round(diff.col.num / dp, 2)
    years <- (sample.yr - 1):(sample.yr - lenbx + 1)
    df.rw <- data.frame(rw)
  } else {
    bx.up <- bor.u
    lenup <- length(bx.up)
    if (lenup <= 1)
      stop('A minimum of two ring borders on each path ',
        'was required to generate a ring-width series')
    diff.col.num.up <- diff(bx.up)
    bx.lower <- bor.l
    lenlo <- length(bx.lower)
    if (lenlo <= 1)
      stop('A minimum of two ring borders on each path ',
        'was required to generate a ring-width series')
    diff.col.num.lower <- diff(bx.lower)
    if (lenlo != lenup)
      stop("If incline = TRUE, the upper and lower paths ", 
           "should have the same number of ring borders")
    years <- (sample.yr - 1):(sample.yr - lenup + 1)
    mean.bor <- (diff.col.num.lower + diff.col.num.up) / 2
    x.cor <- abs(bx.lower - bx.up)
    x.cor <- x.cor[-length(x.cor)]
    correct.rw <- mean.bor * cos(atan(x.cor / (dp * path.dis)))
    correct.rw <- round(correct.rw / dp, 2)
    df.rw <- data.frame(correct.rw)
  }
  rownames(df.rw) <- years
  colnames(df.rw) <- seriesID
  return(df.rw)
}
