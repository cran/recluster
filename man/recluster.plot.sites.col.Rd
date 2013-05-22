\name{recluster.plot.sites.col}
\alias{recluster.plot.sites.col}
\title{
Plotting RGB dots on a custom coordinate space
}
\description{
This function plots the RGB dots belonging to a matrix obtained by recluster.col on a user defined set of coordinates (usually longitude-latitude) for original sites. 
}
\usage{
recluster.plot.sites.col(long, lat, mat, cext = 0.3, cex = 1, cex.axis = 0.7, 
cex.lab = 0.8, text=FALSE,...)
}
\arguments{
	\item{long}{
A vector indicating longitude for cases.
}
	\item{lat}{
A vector indicating latitude for cases.
}
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
sordiss<- recluster.dist(datamod, dist="sorensen")
lat<-c(2,2,2,1,3,1,1,3,3)
long<-c(1,5,3,3,3,1,5,1,5)
points<-metaMDS(sordiss, center=TRUE)$points
col<-recluster.col(points)
recluster.plot.sites.col(long, lat, col,text=TRUE)
}
\keyword{ plot }
