
MtreeRing
=======

<!--require(knitr);require(markdown);knit("README.Rmd")-->


**Authors:** [Jingning Shi](https://www.researchgate.net/profile/Jingning_Shi), [Wei Xiang](https://www.researchgate.net/profile/Wei_Xiang15)<br/>
**License:** [GPL3](https://cran.r-project.org/web/licenses/GPL-3)

<!--我的徽章-->
[![Build Status](https://travis-ci.org/JingningShi/MtreeRing.svg?branch=master)](https://travis-ci.org/JingningShi/MtreeRing.svg?branch=master)
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
devtools::install_github("JingningShi/MtreeRing")
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

After plotting the image, the automatic detection of ring borders can be performed using three alternative methods: (1) watershed algorithm; (2) Canny edge detector; (3) a linear detection algorithm from R package [measuRing](https://cran.r-project.org/web/packages/measuRing/index.html).


```r
## Split a long core sample into 2 pieces to
## get better display performance and use the
## watershed algorithm to detect ring borders:
t2 <- autoDetect(ring.data = t1, seg = 2, method = 'watershed')
```

<center><img src="https://github.com/JingningShi/GifRepo/blob/master/figures/README-img001.png" width = 65% height = 65% /></center>
<center>Figure 1. The automatic detection of ring borders</center>

## Shiny application


```r
MtreeRing::launchMtRApp()
```

This command allows to run a Shiny-based application within the system's default web browser. The app provides a beginner-friendly graphical interface and supports more flexible mouse-based interactions.

The dashboard has three components: a header, sidebar and body, like this

<img src="https://github.com/JingningShi/GifRepo/blob/master/figures/README-img002.png" width = 85% height = 85% />  

## A simple workflow for Shiny app

### 1. Image upload

Once you launch the app, you can upload tree ring images from local hard disk. [Here](https://github.com/JingningShi/GifRepo/blob/master/figures/001.png) is a sample image with the resolution of 1200 dpi. In the following sections, this image is used to demonstrate the ring-width measurement. 

### 2. Path creation

After image loading, you can click on the "**Measurement**" button in the sidebar, and it switches content in the main body. The new page has two graphical windows, named **Tree Ring Detection** and **Tree Ring Editing**. These two graphical windows constitute the core of MtreeRing and enable the display of detected tree rings and different types of user-defined markers.

A path creation consists of the following steps:

1. Enter valid path information, including Series ID, DPI, Sampling year and Y-coordinate of the path.

2. Click on the blue "**Create Path**" button at the top left corner of the **Tree Ring Detection**.

See this example:

<img src="https://github.com/JingningShi/GifRepo/blob/master/figures/PathCreation3.gif" width = 70% height = 75% /> 

In the current version, the path is a horizontal dashed line (see Figure 1 above). The path is usually placed at the center of the core sample and is adjustable both in width and color.

### 3. Ring detection

Let's start by introducing a new action. The Shiny-based app provides a mouse event, called "**brush**". You can select certain portions of the image by left-clicking the mouse button and dragging the mouse over the graphical window. This action will create a blue rectangle. Here is an example of **brush**.


<img src="https://github.com/JingningShi/GifRepo/blob/master/figures/Brush2.gif" width = 60% height = 60% /> 

If ring borders are clearly visible, follow the steps below to detect tree rings:

1. Click on the "**Automation**" button (if this button is not selected). 

    After clicking on this button, `MtreeRing` will show a new box at the top right corner of the app. This box provides a series of input controls for image processing, such as morphological operators and different approaches to edge detection.

2. Create a blue rectangle mentioned above in the first graphical window (**Tree Ring Detection**) by brushing.

3. Click on the green "**Run Detection**" button. 
    
    The app will detect ring borders within the rectangular region. Detected ring borders are placed along the path, and are tagged with years and border numbers. We suggest creating a narrow rectangle to accelerate the detecting process. 

See this example:


<img src="https://github.com/JingningShi/GifRepo/blob/master/figures/RingDetection2.gif" width = 60% height = 60% /> 

### 4. Edit tree rings

If non-edge pixels are incorrectly detected as ring borders, or the wood sample is not suitable for automatic detection, you may need to mark tree rings manually. In this case, the second graphical window (**Tree Ring Editing**) is used to add (remove) tree ring borders to (from) the image.

You may have noticed that the graphical windows **Tree Ring Editing** has no image. To generate a zoomed-in image in this windows, follow these steps:

1. Create a blue rectangle in the **Tree Ring Detection** by brushing.

2. **Double click** on this rectangle, or Click on the blue "**Create Sub-image**" button below.

#### 4.1 Add tree rings

After creating the zoomed-in image, you can add a ring by **double clicking** on the path. 

See this example:

<img src="https://github.com/JingningShi/GifRepo/blob/master/figures/AddRing2.gif" width = 60% height = 60% /> 

#### 4.2 Remove tree rings

Follow these steps to remove tree rings: 

1. Create a rectangle in the **Tree Ring Editing** by brushing.

2. Click on the red "**Delete Border**" button at the top left corner of the **Tree Ring Editing**.

    This operation will delete all ring borders covered by the rectangular region. 

See this example:

<img src="https://github.com/JingningShi/GifRepo/blob/master/figures/RemoveRing.gif" width = 60% height = 60% /> 

You can also perform a mass deletion of borders using the input control below the **Tree Ring Editing**.

### 5. File download

When the analysis of a sample is complete, you can generate a preview ring-width series by clicking on the blue "**Generate Series**" button at the bottom right corner of the app. 

To download a file, click on the **RWL** tab or **CSV** tab. You can provide additional **headers** for the RWL file to record more useful information, such as species, elevation, and site. 

## Ring width correction

If an increment borer is used to extract samples, it is well known that the auger sometimes fails to traverse the pith of the sampled tree but passes through one side of the pith at a certain distance. Tangent lines of rings close to the pith are therefore not perpendicular to the horizontal path, which may lead to considerable errors in ring widths.

Under such conditions, you can create two paths by setting the argument `incline = TRUE`, or by ticking the checkbox "**Inclined tree rings**". See this example.

<img src="https://github.com/JingningShi/GifRepo/blob/master/figures/RingCorrection.png" /> 

The line segment connecting two dots on the same ring should match the tangent of a tree ring border. The corrected ring width is estimated from the distance between adjacent rings and orientation of ring borders.



