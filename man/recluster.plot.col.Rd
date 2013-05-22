\name{recluster.plot.col}
\alias{recluster.plot.col}
\title{
Plotting data in RGB space
}
\description{
This function plots the RGB space belonging to a matrix obtained by recluster.col. 
}
\usage{
recluster.plot.col(mat, cext = 0.3, cex=1,cex.axis=0.7, cex.lab=0.8, text=TRUE,...)
}
\arguments{
  \item{mat}{
A matrix inherited by recluster.col.
}
 \item{text}{
A logical indicating if row names should be plotted.
}
 \item{cext}{
Dimension for row names.
}
 \item{cex}{
Dimension of dots.
}
 \item{cex.axis}{
Dimension of axis labels.
}
 \item{cex.lab}{
Dimension of labels.
}
 \item{...}{
See par() for other graphical parameters
  }
}
\value{
A colour plot. 
}	
\references{
Kreft H., Jetz, W. 2010. 
"A framework for delineating biogeographic regions based on species distributions" 
J Biogeogr (2010),37: 2029-2053.
\url{
www.unifi.it/scibio/bioinfo/recluster.html
}
}
\author{
Leonardo Dapporto and Matteo Ramazzotti
}
\examples{
data(datamod)
sordiss<- recluster.dist(datamod,dist="sorensen")
points<-metaMDS(sordiss, center=TRUE)$points
col<-recluster.col(points)
recluster.plot.col(col)
}
\keyword{ plot }
