f.img.middle <- function(ring.data, x.left, x.right, 
                         ybottom, ytop, x.left1, incline)
{
  if (incline) {
    plot(0, 0, xlab = "", ylab = "",
         xlim = c(x.left, x.right), 
         ylim = c(ybottom - 0.25 * ytop, ybottom + 1.25 * ytop), 
         axes = F, main = "", type = "n")
  } else {
    plot(0, 0, xlab = "", ylab = "",
         xlim = c(x.left, x.right), 
         ylim = c(ybottom, ybottom + 1.25 * ytop), 
         axes = F, main = "",  type = "n")
    axis(1, cex.axis = 1)
  }
  axis(2, cex.axis = 1, line = -1, at = c(ybottom, ybottom + ytop))
  x.range <- (x.left-x.left1+1):(x.right-x.left1+1)
  x1 <- ifelse(is.matrix(ring.data), 
               assign('plot.data',ring.data[, x.range]), 
               assign('plot.data',ring.data[, x.range, ]))
  rasterImage(plot.data, x.left, ybottom, x.right,
              ybottom + ytop, interpolate = TRUE)
}
