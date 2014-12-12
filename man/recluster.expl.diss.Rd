\name{recluster.expl.diss}
\alias{recluster.expl.diss}
\title{
Cut a phylogenetic tree to a given series of nodes and computes explained dissimilarity.
}
\description{
This function cuts a phylogenetic tree at any given series of nodes, provides membership for each element in the series of resulting clusters and computes the fraction of dissimilarity explained by each cut.
}
\usage{
recluster.expl.diss(tree, dist, maxcl=NULL, mincl=NULL, expld=TRUE) 
}
\arguments{
  \item{tree}{
A phylo tree
}
 \item{dist}{
A dissimilarity matrix.
}
 \item{maxcl}{
A custom number indicated the most external node to be cutted. If NULL all the nodes are cutted.
}
 \item{mincl}{
A custom number indicated the most internal node to be cutted. If NULL the root is uded
}
\item{expld}{
A logical. If TRUE then the matrix for explained dissimilarity is computed.
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
Dapporto L. et al 
"A new procedure for extrapolating turnover regionalization at mid-small spatial scales, tested on  British butterflies."
submitted
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
