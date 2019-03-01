hat <- function(seg.mor, x.dpi, watershed.threshold, watershed.adjust)
{
  black.hat <- mclosing_square(seg.mor, size = round(x.dpi / 10))
  black.hat <- black.hat - seg.mor
  black.hat <- threshold(black.hat, thr = watershed.threshold, 
                         approx = FALSE, adjust = watershed.adjust)
  black.hat <- 1 - black.hat
  black.hat.mat <- black.hat[, , 1, 1]
  return(black.hat.mat)
}
