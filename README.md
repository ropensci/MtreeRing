
MtreeRing
=======



**Authors:** [Jingning Shi](https://www.researchgate.net/profile/Jingning_Shi), [Wei Xiang](https://www.researchgate.net/profile/Wei_Xiang15)<br/>
**License:** [GPL3](https://cran.r-project.org/web/licenses/GPL-3)

<!--我的徽章-->
[![cran checks](https://cranchecks.info/badges/worst/MtreeRing)](https://cranchecks.info/pkgs/MtreeRing)
[![cran version](https://www.r-pkg.org/badges/version/MtreeRing)](https://cran.r-project.org/package=MtreeRing)
[![Downloads](https://cranlogs.r-pkg.org/badges/MtreeRing)](https://CRAN.R-project.org/package=MtreeRing)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/MtreeRing?color=orange)](https://CRAN.R-project.org/package=MtreeRing)


`MtreeRing` is a tool for automatically measuring tree-ring width using image processing techniques.

## Installation

Install the stable version from CRAN


```r
install.packages("MtreeRing")
```

or the development version from GitHub


```r
# install.packages("devtools")
devtools::install_github("ropensci/MtreeRing")
```

## Utilities


```r
library(MtreeRing)
```

### Read an image


```r
## Read and plot a tree ring image
img.name <- system.file("001.png", package = "MtreeRing")
t1 <- imgInput(img = img.name, dpi = 1200)
```

### Detect ring borders 

After plotting the image, the automatic detection of ring borders can be performed using three approaches: (1) watershed algorithm; (2) Canny edge detector; (3) a linear detection algorithm from R package [measuRing](https://cran.r-project.org/web/packages/measuRing/index.html).


```r
## Split a long core sample into 2 pieces to
## get better display performance and use the
## watershed algorithm to detect ring borders:
t2 <- autoDetect(ring.data = t1, seg = 2, method = 'watershed')
```

<img src="https://github.com/jingningshi/test001/blob/master/man/figures/README-img001.png" width = 75% height = 65% div align=left />  

## Shiny application

Run a Shiny-based application within the system's default web browser. The application provides a beginner-friendly graphical interface and supports more flexible mouse-based interactions.


```r
launchMtRApp()
#> Error in loadNamespace(name): there is no package called 'webshot'
```
