\name{recluster.cons}
\alias{recluster.cons}
\title{
Consensus tree among re-sampled trees 
}
\description{
This function creates a series of tree by resampling the order row in the original matrix. Then, it computes a consensus among them. The resulting tree is unaffected by original row order.
}
\usage{
recluster.cons(mat, tr = 100, p = 0.5, dist = "simpson", method = "average")
}
\arguments{
  \item{mat}{
A matrix containing sites (rows) and species (columns).
}
  \item{tr}{
The number of trees to be used for the consensus.
}
  \item{p}{
a numeric value between 0.5 and 1 giving the proportion for a clade to be represented in the consensus tree.
}
  \item{dist}{
One among the six beta-diversity indexes \code{"simpson" "sorensen" "nestedness" "beta3" "richness" "jaccard"}. Any custom binary dissimilarity can also be specified according to the syntax of designdist function of the vegan package.

}
    \item{method}{
Any clustering method allowed by hclust.
}
}
\details{
According to the primitive "consensus" function from the "ape" package, p must range between 0.5 and 1.
}
\value{
An object of class phylo.
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
plot(tree,direction="downwards")
}
\keyword{ cluster }
