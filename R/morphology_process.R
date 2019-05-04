# functions for image processing 
color_correct <- function(water.c2)
{
  color.adj <- function(i, water.c2, diff.m) {
    color.position <- which(water.c2 == i, arr.ind = T)
    row.range <- range(color.position[, 1])
    row.range <- row.range[1]:row.range[2]
    color.adjacent <- integer()
    for (j in row.range) {
      row.p <- which(color.position[, 1] == j)
      min.column <- color.position[row.p, 2] %>% min
      color.diff <- which(diff.m[, j] != 0)
      color.pre.p <- color.diff[which(color.diff == min.column) - 1] - 1
      color.pre <- water.c2[j, color.pre.p]
      color.adjacent <- c(color.adjacent, color.pre)
    }
    max(color.adjacent)
  }  
  water.c3 <- cbind(matrix(-1, nrow(water.c2), 1), 
    matrix(0, nrow(water.c2), 1), 
    water.c2)
  diff.m <- apply(water.c3, 1, function(x) c(0, diff(x)))
  color.max <- max(water.c2)
  df.color <- data.frame(color = c(1:color.max), 
    adj = rep(NA, times = color.max))
  for (i in 1:color.max) {
    test.c <- color.adj(i, water.c3, diff.m)
    df.color[i, 2] <- test.c
  }
  for (i in -1:color.max) {
    adj.c <- which(df.color[, 2] == i) 
    if (length(adj.c) >= 2) {   
      max.c <- max(df.color[adj.c, 1])  
      covered.c <- sort(df.color[adj.c, 1])
      covered.c <- covered.c[-length(covered.c)]
      for (j in covered.c) {
        cl <- which(water.c3 == j, arr.ind = T)
        water.c3[cl] <- max.c
        df.color[which(df.color == j, arr.ind = T)] <- max.c
      } 
    } 
  }
  return(water.c3[, -c(1, 2)])
}

morphology_process <- function(seg.data, struc.ele1, struc.ele2, x.dpi) 
{
  if (is.null(struc.ele1)) {
    stru.1 <- x.dpi/400
    struc.ele1 <- c(stru.1, stru.1) %>% round
  }
  if (is.null(struc.ele2)) {
    stru.2 <- x.dpi/80
    struc.ele2 <- c(stru.2, stru.2) %>% round
  }
  cim <- as.cimg(seg.data)
  cim2 <- erode_rect(cim, sx = struc.ele1[1], sy = struc.ele1[2], sz = 1L)
  cim2 <- dilate_rect(cim2, sx = struc.ele1[1], sy = struc.ele1[2], sz = 1L)
  cim2 <- dilate_rect(cim2, sx = struc.ele1[1], sy = struc.ele1[2], sz = 1L)
  cim2 <- erode_rect(cim2, sx = struc.ele1[1], sy = struc.ele1[2], sz = 1L)
  cim2 <- erode_rect(cim2, sx = struc.ele2[1], sy = struc.ele2[2], sz = 1L)
  cim2 <- dilate_rect(cim2, sx = struc.ele2[1], sy = struc.ele2[2], sz = 1L)
  return(cim2)
}

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

conn_calc <- function(black.hat, is.correct)
{
  water.c <- connected(im(black.hat), background = 0, method = "C")
  f1 <- function(x){
    x[is.na(x)] <- 0
    return(x)
  }
  water.c2 <- apply(water.c$v, 2, f1)
  if(is.correct)
    water.c2 <- color_correct(water.c2)
  return(water.c2)
}

watershed_process <- function(water.seg, seg.data)
{
  normalize <- function(x) return((x - min(x))/(max(x) - min(x)))
  imgra <- imgradient(as.cimg(seg.data), axes = "y", scheme = 2)
  watershed.seg <- watershed(as.cimg(water.seg), imgra, fill_lines = T)
  watershed.seg <- normalize(watershed.seg[, , 1, 1])
  return(watershed.seg)
}

border_det <- function(seg.data, py, dp)
{
  border.col <- ring_det(seg.data, py)
  border.col <- ring_sort(border.col, dp)
  return(border.col)
}

ring_det <- function(seg.data, py) {
  gray.values <- seg.data[py, ]
  diff.gray <- c(0, diff(gray.values, lag = 1))
  col.num <- which(diff.gray != 0)
  if (length(col.num) == 0)
    stop("Ring border was not detected")
  return(col.num)
}

ring_sort <- function(border.col, dp)
{
  filter.col <- diff(border.col) >= dp/10
  selected.border <- c(border.col[1], border.col[-1][filter.col])
  return(selected.border)
}

