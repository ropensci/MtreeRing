r.det <- function(seg.data, py)
{
  gray.values <- seg.data[py, ]
  diff.gray <- c(0, diff(gray.values, lag = 1))
  col.num <- which(diff.gray != 0)
  if (length(col.num) == 0)
    stop("Ring border was not detected")
  return(col.num)
}
