\name{recluster.col}
\alias{recluster.col}
\title{
Projecting a two dimensional plot in RGB space
}
\description{
This function projects a two dimensional matrix into a RGB space with red, green, yellow and blue at its four corners. RGB combination for each case corresponding to its position in this space is provided together with new coordinates.
}
\usage{
recluster.col(mat)
}
\arguments{
  \item{mat}{
A matrix containing two dimensional coordinates for cases.
}
  }
\value{
A matrix with the first two columns representing the new coordinates and the third, fourth and fifth representing the red, green and blue components, respectively. 	
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
col
}
\keyword{ plot }
