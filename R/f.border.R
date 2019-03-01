f.border <- function(seg.data, py, dp)
{
  border.col <- r.det(seg.data, py)
  border.col <- f.sort(border.col, dp)
  return(border.col)
}
