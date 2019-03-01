correct.color <- function(water.c2)
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