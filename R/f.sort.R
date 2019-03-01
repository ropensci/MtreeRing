f.sort <- function(border.col, dp)
{
  filter.col <- diff(border.col) >= dp/10
  selected.border <- c(border.col[1], border.col[-1][filter.col])
  return(selected.border)
}
