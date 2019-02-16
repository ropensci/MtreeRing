
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

<center><img src="https://github.com/jingningshi/test001/blob/master/man/figures/README-img001.png" width = 65% height = 65% /></center>
<center>Figure 1. The automatic detection of ring borders</center>

## Shiny application

Run a Shiny-based application within the system's default web browser. The application provides a beginner-friendly graphical interface and supports more flexible mouse-based interactions.


```r
MtreeRing::launchMtRApp()
```

The dashboard has three components: a header, sidebar and body, like this
<br></br>
<img src="https://github.com/jingningshi/test001/blob/master/man/figures/README-img002.png" width = 85% height = 85% />  


## A simple workflow for Shiny app

### 1. Image upload

Once your launch the app, you can upload tree ring images from local hard disk. [Here](https://github.com/jingningshi/test001/blob/master/man/figures/001.png) is a sample image with the resolution of 1200 dpi. In the following sections, this image is used for the demonstration of ring-width measurement. 

### 2. Path creation

After image loading, you can click on the "**Measurement**" link in the sidebar, and it switches content in the main body. 

The new page has two graphical windows, named **Measurement Window** and **Zoomed Image Window**.

A path creation consists of the following steps:

1. Enter valid path information, including Series ID, DPI, Sampling year and Y-coordinate of the path.
2. Click "**Create Path**" (blue button on top left corner of the **Measurement Window**).

In current version, the path is a horizontal dashed line (see Figure 1 above). The path position is usually placed at the center of the core sample and is adjustable both in width and color.

Detected ring borders are placed along the path, and are tagged with years and border numbers.

### 3. Ring detection

If ring borders are clearly visible, click on the "**Automation**" button, and `MtreeRing` will show a new box at top right corner of the app. This box provides a series of input controls for image processing, such as morphological operators and edge detection. 

The Shiny-based app provides a mouse event, called "**brush**". You can select certain portions of the image by left-clicking the mouse button and dragging the mouse over the graphical window. This operation will create a blue rectangle. Here is a example.

<!--回头用动态gif代替-->
<img src="https://github.com/jingningshi/test001/blob/master/man/figures/README-img003.png" width = 85% height = 85% /> 

If you have created a rectangle in the the **Measurement Window**, click on the green "**Run Detection**" button.

