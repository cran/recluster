\name{recluster.boot}
\alias{recluster.boot}
\title{
Bootstrap nodes of consensus trees
}
\description{
Given an initial tree and a data matrix, this function computes bootstrap for nodes. Each tree used for bootstrap can be constructed by re-sampling the row order several times and by applying a consensus rule as done by recluster.cons. The number of sampled columns (species) can be varied.
}
\usage{
recluster.boot(tree, mat, tr = 100, p = 0.5, dist = "simpson", method = "average", boot = 1000, level = 1)
}
\arguments{
  \item{tree}{
A phylo tree presumably constructed with recluster.cons function.
}
  \item{mat}{
The matrix used to construct the tree.
}
  \item{tr}{
The number of trees to be included in the consensus.
}
  \item{p}{
A numeric value between 0.5 and 1 giving the proportion for a clade to be represented in the consensus tree.
}
  \item{dist}{
One among the six beta-diversity indexes \code{"simpson" "sorensen" "nestedness" "beta3" "richness" "jaccard"}. Any custom binary dissimilarity can also be specified according to the syntax of designdist function of the vegan package.
}
  \item{method}{
Any clustering method allowed by hclust.
}
  \item{boot}{
The number of trees used for bootstrap computation.
}
  \item{level}{
The ratio between the number of species to be included in the analysis and the original number of species in the mat matrix.
}
}
\details{
Computation can be time consuming due to the high number of trees required for analysis. It is suggested to assess the degree of row bias by recluster.hist and recluster.node.strength to optimize the number of required consensus trees before starting the analysis.
}
\value{
A vector indicating the percentage of bootstrap trees replicating each original node.
}
\references{
Dapporto L., Ramazzotti M., Fattorini S., Vila R., Dennis R. 
"recluster: an unbiased clustering procedure to detect biogeographic patterns by beta-diversity turnover"
XXXX (2013), XXX:XXX.
\url{
www.unifi.it/scibio/bioinfo/recluster.html
}
}
\author{
Leonardo Dapporto
}
\examples{
data(datamod)
tree<-recluster.cons(datamod,tr=10)
boot<-recluster.boot(tree,tr=5,boot=50,datamod)
recluster.plot(tree,boot,direction="downwards")
}
\keyword{ cluster }
