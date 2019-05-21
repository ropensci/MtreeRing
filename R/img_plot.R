# Functions for plotting detected ring borders and labels

label_create <- function(bor.col, sample.yr, dp)
{
  year.number <- sample.yr:(sample.yr - length(bor.col) + 1)
  border.number <- length(bor.col) %>% seq_len
  rw <- diff(bor.col) / dp
  year.rw <- year.number[-1]
  dfrw <- data.frame(col.num = bor.col[-1], rw = rw, year = year.rw)
  label.list <- list(year = year.number, sn = border.number, dfrw = dfrw)
  return(label.list)
}

label_split <- function(dfrw, x.left, x.right, seg)
{
  bor.list <- list()
  rw.list <- list()
  yr.list <- list()
  for (i in 1:seg) {
    bor.range <- which(dfrw[, 1] >= x.left[i] & dfrw[, 1] <= x.right[i])
    df.bor <- dfrw[, 1][bor.range]
    df.rw <- dfrw[, 2][bor.range]
    df.yr <- dfrw[, 3][bor.range]
    bor.list <- c(bor.list, list(df.bor))
    rw.list <- c(rw.list, list(df.rw))
    yr.list <- c(yr.list, list(df.yr))
  }
  split.list <- list(bor.list = bor.list, 
                      rw.list = rw.list, 
                      yr.list = yr.list)
  return(split.list)
}

rw_plot <- function(bor.list, rw.list, yr.list, 
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

img_plot <- function(ring.data, x.left, x.right, 
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

marker_plot <- function(bor.col, x.left, x.right, ybottom, ytop, py, location,
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

border_plot <- function(bor.col, x.left, x.right, py, location, sn,
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

two_paths_plot <- function(ring.data, bor.u, bor.l, x.left, x.right, 
                           seg, py.upper, py.lower, dp, sample.yr, 
                           ybottom, ytop, py, seg.name, border.type,
                           border.color, label.color, label.cex)
{
  label.list.u <- label_create(bor.u, sample.yr, dp)
  label.list.l <- label_create(bor.l, sample.yr, dp)
  dfrw.u <- label.list.u$dfrw
  dfrw.l <- label.list.l$dfrw
  bn.u <- label.list.u$sn
  bn.l <- label.list.l$sn
  yn.u <- label.list.u$year
  yn.l <- label.list.l$year
  # "lbl" is short for "label"
  seg.lbl.u <- label_split(dfrw.u, x.left, x.right, seg)
  seg.lbl.l <- label_split(dfrw.l, x.left, x.right, seg)
  seg.device.number <- vector(length = 0)  
  for (i in 1:seg) {
    dev.new()
    if (names(dev.cur()) == "RStudioGD") dev.new()
    bor.list.u <- seg.lbl.u$bor.list[[i]]
    rw.list.u <- seg.lbl.u$rw.list[[i]]
    yr.list.u <- seg.lbl.u$yr.list[[i]]
    bor.list.l <- seg.lbl.l$bor.list[[i]]
    rw.list.l <- seg.lbl.l$rw.list[[i]]
    yr.list.l <- seg.lbl.l$yr.list[[i]]
    layout(matrix(c(1, 2, 2, 3), 4, 1))
    par(mar = c(1.75, 5, 2, 0), mfg = c(1, 1))
    if (length(bor.list.u != 0)) {
      rw_plot(bor.list.u, rw.list.u, yr.list.u, 
        x.left[i], x.right[i], seg.name[i], 'upper')
    } else {
      plot(0, 0, type = "p", pch = 4, axes = F, cex = 3, ylab = "", col = "red")
      title(main = seg.name[i], cex.main = 1.5, line = -1)
      text(0, 0, labels = "Ring border was not detected", 
        adj = c(0.5, -2), cex = 1.2)
    }
    par(mar = c(2, 5, 1.25, 0), mfg = c(3, 1))
    if (length(bor.list.l != 0)) {
      rw_plot(bor.list.l, rw.list.l, yr.list.l, 
        x.left[i], x.right[i], seg.name[i], 'lower')
    } else {
      plot(0, 0, type = "p", pch = 4, axes = F, cex = 3, ylab = "", col = "red")
      text(0, 0, labels = "Ring border was not detected", 
        adj = c(0.5, -2), cex = 1.2)
    }
    par(mar = c(1.25, 5, 1.25, 0), mfg = c(2, 1))
    img_plot(ring.data, x.left[i], x.right[i], ybottom, ytop, x.left[1], T)
    marker_plot(bor.u, x.left[i], x.right[i], ybottom, ytop, py.upper, 'upper',
      border.type, border.color, label.color, label.cex)
    border_plot(bor.u, x.left[i], x.right[i], py.upper, 'upper', bn.u,
      border.type, border.color, label.color, label.cex)
    marker_plot(bor.l, x.left[i], x.right[i], ybottom, ytop, py.lower, 'lower',
      border.type, border.color, label.color, label.cex)
    border_plot(bor.l, x.left[i], x.right[i], py.lower, 'lower', bn.l,
      border.type, border.color, label.color, label.cex)
    seg.device.number[i] <- as.numeric(dev.cur())
  } 
  img.attribute <- list(seg.dn = seg.device.number, yn.u = yn.u, 
                        yn.l = yn.l, bn.u = bn.u, bn.l = bn.l)
  return(img.attribute)
}

single_path_plot <- function(ring.data, bor.col, x.left, x.right, seg,
                             dp, sample.yr, ybottom, ytop, py, seg.name,
                             border.type, border.color, label.color, label.cex)
{
  label.list <- label_create(bor.col, sample.yr, dp)
  dfrw <- label.list$dfrw
  border.number <- label.list$sn
  year.number <- label.list$year
  segmented.label.list <- label_split(dfrw, x.left, x.right, seg)
  seg.device.number <- vector(length = 0)  
  for (i in 1:seg) {
    dev.new()
    if (names(dev.cur()) == "RStudioGD") dev.new()
    bor.list <- segmented.label.list$bor.list[[i]]
    rw.list <- segmented.label.list$rw.list[[i]]
    yr.list <- segmented.label.list$yr.list[[i]]
    layout(matrix(c(1, 2, 2), 3, 1))
    par(mar = c(1.25, 5, 2, 0))
    if (length(bor.list != 0)) {
      rw_plot(bor.list, rw.list, yr.list, 
        x.left[i], x.right[i], seg.name[i], 'upper')
    } else {
      plot(0, 0, type = "p", pch = 4, axes = F, 
        cex = 3, ylab = "", col = "red")
      title(main = seg.name[i], cex.main = 1.5, line = -1)
      text(0, 0, labels = "Ring border was not detected", 
        adj = c(0.5, -2), cex = 1.2)
    }
    par(mar = c(5, 5, 1.25, 0), mfg = c(2, 1))
    img_plot(ring.data, x.left[i], x.right[i], ybottom, ytop, x.left[1], F)
    marker_plot(bor.col, x.left[i], x.right[i], ybottom, ytop, py, 'upper',
      border.type, border.color, label.color, label.cex)
    border_plot(bor.col, x.left[i], x.right[i], py, 'upper', border.number,
      border.type, border.color, label.color, label.cex)
    seg.device.number[i] <- as.numeric(dev.cur())
  }
  img.attribute <- list(seg.dn = seg.device.number, 
    yn = year.number, 
    bn = border.number)
  return(img.attribute)
}

