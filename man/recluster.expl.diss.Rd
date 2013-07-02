\name{recluster.expl.diss}
\alias{recluster.expl.diss}
\title{
Compute explained dissimilarity of all possible cuts of a given tree
}
\description{
This function compute the fraction of the diversity contained in a dissimilarity matrix which is explained by any possible cut of a given tree constructed on that matrix.
}
\usage{
recluster.expl.diss(tree, dist, maxcl=NULL)
}
\arguments{
  \item{tree}{
A phylo tree
}
 \item{dist}{
A dissimilarity matrix.
}
 \item{maxcl}{
A custom maximum number of clusters.
}
}
\details{
When polytomic nodes are involved in a cut the increase in the  number of clusters at a given cut will be higher than one. Holt at al. (2013) discussed the level of explained dissimilarity to be used as a reliable threshold to identify a tree cut. When cases are highly numerous maxcl can be set in order to avoid a very long computation
}
\value{
 \item{matrix}{
A matrix indicating cluster membership of each site in each cut of the tree.
 }
 \item{expl.div}{
A vector indicating the explained dissimilarity for each cut.
 }
  \item{nclust}{
A vector indicating the number of clusters resulting from each cut.
 }
}	
\references{
Holt B. et al. 
"An Update of Wallace's Zoogeographic Regions of the World." 
Science (2013), 339, 74-78. 
\url{
www.unifi.it/scibio/bioinfo/recluster.html
}
}
\author{
Leonardo Dapporto and Matteo Ramazzotti
}
\examples{
data(datamod)
sor_tree<- recluster.cons(datamod, dist="sorensen")
sor_diss <- recluster.dist (datamod, dist="sorensen")
expl_diss <- recluster.expl.diss (sor_tree$cons,sor_diss)
expl_diss 
}
\keyword{ cluster }
