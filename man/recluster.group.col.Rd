\name{recluster.group.col}
\alias{recluster.group.col}
\title{
Compute mean coordinate values and RGB colours.
}
\description{
This function computes mean coordinate values and RGB colours for cases belonging to the same group from an original RGB colour space.
}
\usage{
recluster.group.col(mat,member)
}
\arguments{
  \item{mat}{
An inherited matrix from recluster.col containing the original RGB colour space.
}
 \item{member}{
A vector indicating group membership for each case.
}
  }
\value{
 \item{aggr}{
A matrix in the recluster.col format with mean values for coordinates and RGB colours for groups.
 }
 \item{all}{
A matrix in the recluster.col format with mean RGB colours computed for the group of each original case.
 }
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
group<-c(1,2,3,3,3,1,2,1,2)
ncol<-recluster.group.col(col,group)
recluster.plot.col(ncol$aggr)
}
\keyword{ cluster }
