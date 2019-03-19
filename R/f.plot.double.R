f.plot.double <- function(ring.data, bor.u, bor.l, x.left, x.right, 
                          seg, py.upper, py.lower, dp, sample.yr, 
                          ybottom, ytop, py, seg.name, border.type,
                          border.color, label.color, label.cex)
{
  label.list.u <- create.label(bor.u, sample.yr, dp)
  label.list.l <- create.label(bor.l, sample.yr, dp)
  dfrw.u <- label.list.u$dfrw
  dfrw.l <- label.list.l$dfrw
  bn.u <- label.list.u$sn
  bn.l <- label.list.l$sn
  yn.u <- label.list.u$year
  yn.l <- label.list.l$year
  segmented.label.list.u <- split.label(dfrw.u, x.left, x.right, seg)
  segmented.label.list.l <- split.label(dfrw.l, x.left, x.right, seg)
  seg.device.number <- vector(length = 0)  
  for (i in 1:seg) {
    dev.new()
    bor.list.u <- segmented.label.list.u$bor.list[[i]]
    rw.list.u <- segmented.label.list.u$rw.list[[i]]
    yr.list.u <- segmented.label.list.u$yr.list[[i]]
    bor.list.l <- segmented.label.list.l$bor.list[[i]]
    rw.list.l <- segmented.label.list.l$rw.list[[i]]
    yr.list.l <- segmented.label.list.l$yr.list[[i]]
    layout(matrix(c(1, 2, 2, 3), 4, 1))
    par(mar = c(1.75, 5, 2, 0), mfg = c(1, 1))
    if (length(bor.list.u != 0)) {
      f.rw.plot(bor.list.u, rw.list.u, yr.list.u, 
                x.left[i], x.right[i], seg.name[i], 'upper')
    } else {
      plot(0, 0, type = "p", pch = 4, axes = F, cex = 3, ylab = "", col = "red")
      title(main = seg.name[i], cex.main = 1.5, line = -1)
      text(0, 0, labels = "Ring border was not detected", 
           adj = c(0.5, -2), cex = 1.2)
    }
    par(mar = c(2, 5, 1.25, 0), mfg = c(3, 1))
    if (length(bor.list.l != 0)) {
      f.rw.plot(bor.list.l, rw.list.l, yr.list.l, 
                x.left[i], x.right[i], seg.name[i], 'lower')
    } else {
      plot(0, 0, type = "p", pch = 4, axes = F, cex = 3, ylab = "", col = "red")
      text(0, 0, labels = "Ring border was not detected", 
        adj = c(0.5, -2), cex = 1.2)
    }
    par(mar = c(1.25, 5, 1.25, 0), mfg = c(2, 1))
    f.img.middle(ring.data, x.left[i], x.right[i], ybottom, ytop, x.left[1], T)
    f.marker(bor.u, x.left[i], x.right[i], ybottom, ytop, py.upper, 'upper',
      border.type, border.color, label.color, label.cex)
    f.bor.plot(bor.u, x.left[i], x.right[i], py.upper, 'upper', bn.u,
               border.type, border.color, label.color, label.cex)
    f.marker(bor.l, x.left[i], x.right[i], ybottom, ytop, py.lower, 'lower',
      border.type, border.color, label.color, label.cex)
    f.bor.plot(bor.l, x.left[i], x.right[i], py.lower, 'lower', bn.l,
               border.type, border.color, label.color, label.cex)
    seg.device.number[i] <- as.numeric(dev.cur())
  } 
  img.attribute <- list(seg.dn = seg.device.number, yn.u = yn.u, 
                        yn.l = yn.l, bn.u = bn.u, bn.l = bn.l)
}
