water.im <- function(black.hat, is.correct)
{
  water.c <- connected(im(black.hat), background = 0, method = "C")
  water.c2 <- apply(water.c$v, 2, function(x){
    x[is.na(x)] <- 0
    return(x)
  })
  if(is.correct)
    water.c2 <- correct.color(water.c2)
  return(water.c2)
}