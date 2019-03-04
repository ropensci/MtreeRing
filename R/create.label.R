create.label <- function(bor.col, sample.yr, dp)
{
  year.number <- sample.yr:(sample.yr - length(bor.col) + 1)
  border.number <- seq_len(bor.col)
  rw <- diff(bor.col) / dp
  year.rw <- year.number[-1]
  dfrw <- data.frame(col.num = bor.col[-1], rw = rw, year = year.rw)
  label.list <- list(year = year.number, sn = border.number, dfrw = dfrw)
  return(label.list)
}
