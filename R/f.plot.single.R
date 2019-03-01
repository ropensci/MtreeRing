f.plot.single <- function(ring.data, bor.col, x.left, x.right, seg,
                          dp, sample.yr, ybottom, ytop, py, seg.name,
                          border.type, border.color, label.color, label.cex)
{
  label.list <- create.label(bor.col, sample.yr, dp)
  dfrw <- label.list$dfrw
  border.number <- label.list$sn
  year.number <- label.list$year
  segmented.label.list <- split.label(dfrw, x.left, x.right, seg)
  seg.device.number <- vector(length = 0)  
  for (i in 1:seg) {
    dev.new()
    bor.list <- segmented.label.list$bor.list[[i]]
    rw.list <- segmented.label.list$rw.list[[i]]
    yr.list <- segmented.label.list$yr.list[[i]]
    layout(matrix(c(1, 2, 2), 3, 1))
    par(mar = c(1.25, 5, 2, 0))
    if (length(bor.list != 0)) {
      f.rw.plot(bor.list, rw.list, yr.list, 
                x.left[i], x.right[i], seg.name[i], 'upper')
    } else {
      plot(0, 0, type = "p", pch = 4, axes = F, 
        cex = 3, ylab = "", col = "red")
      title(main = seg.name[i], cex.main = 1.5, line = -1)
      text(0, 0, labels = "Ring border was not detected", 
        adj = c(0.5, -2), cex = 1.2)
    }
    par(mar = c(5, 5, 1.25, 0), mfg = c(2, 1))
    f.img.middle(ring.data, x.left[i], x.right[i], ybottom, ytop, x.left[1], F)
    f.marker(bor.col, x.left[i], x.right[i], ybottom, ytop, py, 'upper',
             border.type, border.color, label.color, label.cex)
    f.bor.plot(bor.col, x.left[i], x.right[i], py, 'upper', border.number,
      border.type, border.color, label.color, label.cex)
    seg.device.number[i] <- as.numeric(dev.cur())
  }
  img.attribute <- list(seg.dn = seg.device.number, 
                            yn = year.number, 
                            bn = border.number)
  return(img.attribute)
}