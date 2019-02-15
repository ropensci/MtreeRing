\encoding{UTF-8}
\name{visualSelect}
\alias{visualSelect}

\title{Edit ring borders visually}
\description{This function can delete existing ring borders and add new borders visually.}
\usage{
visualSelect(ring.data, del = NULL, del.u = NULL, del.l = NULL, add = FALSE)
}
\arguments{
  \item{ring.data}{A matrix or array produced by \code{autoDetect}.}
  \item{del}{A numeric vector giving the border numbers to be deleted.}
  \item{del.u}{A numeric vector giving the border numbers to be deleted on the upper path.}
  \item{del.l}{A numeric vector giving the border numbers to be deleted on the lower path.}
  \item{add}{A logical value indicating whether to re-add ring borders visually.}
}

\value{A matrix or array representing the user-selected subset of the image.}
\details{
This function is used to delete existing ring borders or add new borders by interactively clicking on the image segments.

If the user creates only one path (\code{incline = FALSE}), the argument \code{del} is used to delete ring borders. If the user creates two paths (\code{incline = TRUE}), arguments \code{del.u} and \code{del.l} are used to delete ring borders.

If \code{add = TRUE}, graphical windows where image segments are plotted will be activated sequentially. When one graphical window is activated, the user can add new borders by left-clicking the mouse along the path. This visual selection process can be terminated by clicking the right button and selecting ‘Stop’ from the menu, or from the ‘Stop’ button on the top-left corner of the graphical window (depending on the R version). 

Once the user terminates the visual selection process on one segment, the current graphical window will be closed automatically, and the graphical window of the following segment is activated. When all graphical windows are closed, this function will re-open graphical windows and plot new borders.

This function can perform both operations (deletion and addition) in one call. A deletion of borders takes precedence over addition.
}
\author{
Jingning Shi
}

\examples{
## Find the image file name in package MtreeRing:
img.name <- system.file("001.png", package = "MtreeRing")

## Read and plot the image:
t1 <- imgInput(img = img.name, dpi = 1200)

## Split a long core sample into 3 pieces to
## get better display performance and use the
## watershed algorithm to detect ring borders:
t2 <- autoDetect(ring.data = t1, seg = 3, method = 'watershed')

## Not modify t2, create a new array object t3. Delete 
## some borders without adding new borders:
t3 <- visualSelect(ring.data = t2, del = c(1, 3, 5, 19:21), add = FALSE)

}

