f.bor.plot <-function(bor.col, x.left, x.right, py, location, sn,
                      border.type, border.color, label.color, label.cex)
{
  abline(h = py, lty = 2, col = label.color)
  bor.range <- bor.col >= x.left & bor.col <= x.right
  selected.bor <- bor.col[bor.range]
  selected.sn <- sn[bor.range]
  if (length(selected.bor) >= 1) {
    points(selected.bor, rep(py, time = length(selected.bor)), type = "p", 
           pch = border.type, col = border.color, cex = label.cex)
    text.position <- ifelse(location == "upper", 2, -1)
    text(selected.bor, rep(py, time = length(selected.bor)), selected.sn, 
         adj = c(1.25, text.position), col = label.color, cex = label.cex)
  }
}
