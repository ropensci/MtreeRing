split.label <- function(dfrw, x.left, x.right, seg)
{
  bor.list<-list()
  rw.list<-list()
  yr.list<-list()
  for (i in 1:seg) {
    bor.range <- which(dfrw[, 1] >= x.left[i] & 
                       dfrw[, 1] <= x.right[i])
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
