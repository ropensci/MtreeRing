## ----eval=FALSE----------------------------------------------------------
#  install.packages("MtreeRing")

## ----eval=FALSE----------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("JingningShi/MtreeRing")

## ------------------------------------------------------------------------
library(MtreeRing)
## Read and plot a tree ring image
img.name <- system.file("001.png", package = "MtreeRing")
t1 <- imgInput(img = img.name, dpi = 1200)

## ------------------------------------------------------------------------
## Split a long core sample into 2 pieces to
## get better display performance and use the
## watershed algorithm to detect ring borders:
t2 <- autoDetect(ring.data = t1, auto.path = TRUE, seg = 2, method = 'watershed')

## ------------------------------------------------------------------------
img.name <- system.file("incline.png", package = "MtreeRing")
t1 <- imgInput(img = img.name, dpi = 1200)
t2 <- autoDetect(ring.data = t1, incline = TRUE, method = 'watershed')

## ------------------------------------------------------------------------
rw.df <- calcRingWidth(ring.data = t2, seriesID = "940220")

## ----eval=FALSE----------------------------------------------------------
#  launchMtRApp()

