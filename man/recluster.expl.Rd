\name{recluster.expl}
\alias{recluster.expl}
\title{
Compute the dissimilarity contained in a distance matrix which is explained by a clustering solution of the elements.
}
\description{
This function compute the fraction of the distances contained in a dissimilarity matrix which is explained by a clustering solution of the elements. The value is obtained by computing the sum of all the dissimilarity values among elements belonging to different clusters and diving this sum for the sum of all the cells of the original dissimilarity matrix.
}
\usage{
recluster.expl(mat, clust)
}
\arguments{
  \item{mat}{
A dissimilarity matrix
}
 \item{clust}{
A clustering solution for the cases contained in the dissimilarity matrix.
}
}
\value{
A number ranging between 0 and 1 indicating the fraction of explained dissimilarity.
}
\references{
Holt, B.G. et al 
"An Update of Wallace's Zoogeographic Regions of the World."
Science, 339:74-78.
\url{
www.unifi.it/scibio/bioinfo/recluster.html
}
}
\author{
Leonardo Dapporto
}
\examples{
data(datamod)
sor_tree<- recluster.cons(datamod, dist="sorensen")
sor_diss <- recluster.dist (datamod, dist="sorensen")
expl_diss <- recluster.expl.diss (sor_tree$cons,sor_diss)
expl_diss 
}
\keyword{ cluster }
