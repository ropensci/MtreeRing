\encoding{UTF-8}
\name{calcRingWidth}
\alias{calcRingWidth}

\title{Generate a ring-width series}
\description{This function can calculate the ring-width series according to detected ring borders.}
\usage{
calcRingWidth(ring.data, seriesID)
}
\arguments{
  \item{ring.data}{A matrix or array produced by \code{autoDetect} or \code{visualSelect}.}
  \item{seriesID}{A character string specifying the column name of the ring-width series.}
}

\value{A data frame. The series ID is the column name and years are row names.}

\author{
Jingning Shi
}

\examples{
img.path <- system.file("001.png", package = "MtreeRing")

## Read and plot the image:
t1 <- imgInput(img = img.path, dpi = 1200)

## Split a long core sample into 3 pieces to
## get better display performance and use the
## watershed algorithm to detect ring borders:
t2 <- autoDetect(ring.data = t1, seg = 3, method = 'watershed')

## Calculate ring widths from the attribute list of t2:
rw.df <- calcRingWidth(ring.data = t2, seriesID = "940220")


}

