\name{recluster.node.strength}
\alias{recluster.node.strength}
\title{
Evaluating order row bias in a cluster
}
\description{
This function helps to understand the order row bias by computing a first hclust tree with original order of areas. Then it creates six trees by recluster.cons with increasing consensus rule from 50% to 100% by a 10% step. Then, it scores the percentage of times each node is replicated among the six runs of consensus thresholds
}
\usage{
recluster.node.strength(mat, phylo = NULL, dist = "simpson", tr = 100, method = "average")
}
\arguments{
  \item{mat}{
A matrix containing sites (rows) and species (columns).
}
  \item{phylo}{
An ultrametric and rooted phylo tree for species having the same labels as in mat columns. Only required for phylogenitic beta-diversity indexes.
}
  \item{tr}{
The number of trees to be used for the consensus.
}
   \item{dist}{
One among the twelve beta-diversity indexes \code{"simpson" "sorensen" "nestedness" "beta3" "richness" "jaccard" "phylosor" "phylosort" "phylosorpd" "unifrac" "unifract" "unifractpd"}. Any custom binary dissimilarity can also be specified according to the syntax of designdist function of the vegan package.
}
    \item{method}{
Any clustering method allowed by hclust.
}
}
\details{
It has to be noted that values obtained by this function are not bootstrap support for node but a crude indication of the effect of the row bias. Nodes with low support in this analysis can have strong bootstrap support and vice versa. This preliminary analysis can avoid that a strict consensus could collapse nodes potentially highly supported by bootstrap when the bias is high and allow to reduce the number of consensus trees thus fastening bootstrap analysis when the bias is low. 
}
\value{
A cluster with percentages of occurrence over different consensus runs for each node.
}
\references{
Dapporto L., Ramazzotti M., Fattorini S., Vila R., Talavera G., Dennis R. 
"recluster: an unbiased clustering procedure for beta-diversity turnover"
XXXX (2013), XXX:XXX.
\url{
www.unifi.it/scibio/bioinfo/recluster.html
}
}
\author{
Leonardo Dapporto and Matteo Ramazzotti
}
\examples{
data(datamod)
recluster.node.strength(datamod, tr=10)
}
\keyword{ cluster }
