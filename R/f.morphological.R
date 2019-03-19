f.morphological <- function(seg.data, struc.ele1, struc.ele2, x.dpi) 
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
