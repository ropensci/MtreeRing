\encoding{UTF-8}
\name{nearPith}
\alias{nearPith}

\title{Calibrate ring-width series}
\description{This function can calibrate the ring-width series using arcs of inner rings.}
\usage{
nearPith(ring.data, inner.arc = TRUE, last.yr = NULL, 
         color = 'black', border.type = 16, label.cex = 1.5)
}
\arguments{
  \item{ring.data}{A magick image object produced by \code{imgInput}.}
  \item{inner.arc}{A logical value indicating whether to calibrate the ring-width series using the arcs of inner rings. See details below.}
  \item{last.yr}{\code{NULL} or an integer giving the year of formation of the left-most ring. If \code{NULL}, borders numbers (starting from 1) ares used instead of the year.}
  \item{color}{Color for labels.}
  \item{border.type}{Symbol for ring borders. See \code{pch} in \code{\link{points}} for possible values and their shapes.}
  \item{label.cex}{The magnification to be used for years and ring numbers.}
}

\details{
This function allows the user to create a path, and visually identify ring borders by clicking on the graphical window.  

\itemize{
    \item
    If \code{inner.arc = TRUE}, the ring-width series is calibrated using arcs of inner rings (Duncan, 1989). 
    
    First, the user can click the left mouse button to add a horizontal path. The path should traverse an appropriate arc (read the reference below for more details). Then, the user can add three points to the selected arc by left-clicking. The first point should be placed on the left endpoint of the arc, and the second point is placed on the right endpoint. 
    
    After two endpoints are added, a vertical dashed line will be plotted automatically according to the (x,y) positions of endpoints you just added. The third points should be placed on the intersection of the vertical dashed line and the selected arc. 
    
    Finally, the user is prompted to visually mark ring borders along the path. The termination of visual selection is similar to \code{visualSelect}. Note that the left endpoint of the arc will be considered as the last ring border without the need to mark it.
    
    The ring-width series are corrected using formulas proposed by Duncan (1989).

    \item
    If \code{inner.arc = FALSE}, the user can create a path which matches the direction of wood growth. 
    
    First, the user can add two points by left-clicking on the image. A path passing through these two points will be plotted. The path should follow the rays from bark to pith.
    
    Then, the user can visually mark ring borders along the path. The termination of visual selection is similar to \code{visualSelect}.
    
}

}

\value{A data frame of the calibrated ring-width series.}

\references{
  Duncan R. (1989)
  An evaluation of errors in tree age estimates based on increment cores in Kahikatea (Dacrycarpus dacrydiodes).
  \emph{New Zealand Natural Sciences}
  \bold{16(4)}, 1-37.

}

\author{
Jingning Shi
}

\examples{

img.path <- system.file("arc.png", package = "MtreeRing")

## Read and plot the image:
t1 <- imgInput(img = img.path, dpi = 1200)

## Use the arcs of inner rings to calibrate ring-width series:
\donttest{t2 <- nearPith(ring.data = t1, inner.arc = TRUE, last.yr = 2016) }


## Try another method to measure ring widths:
\donttest{t3 <- nearPith(ring.data = t1, inner.arc = FALSE, last.yr = 2016) }

}

