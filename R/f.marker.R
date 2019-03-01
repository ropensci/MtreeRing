f.marker <- function(bor.col, x.left, x.right, ybottom, ytop, py, location,
                     border.type, border.color, label.color, label.cex)
{
  bor.col <- bor.col[-1]
  bor.range <- bor.col >= x.left & bor.col <= x.right
  selected.bor <- bor.col[bor.range]
  y.loc <- ifelse(location == 'upper', 
                  ybottom + 1.25 * ytop, 
                  ybottom - 0.25 * ytop)
  segments(selected.bor, rep(py, length(selected.bor)), 
           selected.bor, rep(y.loc, length(selected.bor)), 
           col = label.color, lty = 2, lwd = 2, lend = 2)
}
