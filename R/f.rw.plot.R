f.rw.plot <- function(bor.list, rw.list, yr.list, 
                      x.left, x.right, seg.name, location)
{
  plot(bor.list, rw.list, xlim = c(x.left, x.right), 
       ylim = c(round(min(rw.list), 1) - 0.3, 
                round(max(rw.list), 1) + 0.3), 
       main = "", xlab = "", ylab = "", type = "o", 
       cex = 1, axes = F, lty = 3, pch = 16)
  if (location == 'upper') {
    axis(1, at = bor.list, labels = yr.list, 
         las = 2, font = 3, cex.axis = 1.2)
    title(main = seg.name, cex.main = 1.5, line = -1)
  } else {
    axis(3, at = bor.list, labels = yr.list, 
         las = 2, font = 3, cex.axis = 1.2)
  }
  axis(2, at = seq(round(min(rw.list), 1) - 0.3, 
                   round(max(rw.list), 1) + 0.3, by = 0.4), 
       cex.axis = 1, las = 2, line = -1)
  title(ylab = "Ring Widths (mm)", cex.lab = 1.2, line = 2.8)
}
