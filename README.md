
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

## Quick start

### Install

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

![Image text](https://github.com/jingningshi/test001/blob/master/man/figures/README-img001.png)
