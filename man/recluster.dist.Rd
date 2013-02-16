\name{recluster.dist}
\alias{recluster.dist}
\title{
Compute a dissimilarity matrix using a battery of beta-diversity indices
}
\description{
This function computes dissimilarity matrices based on the two most popular partitions of faunistic and phylogenitic beta-diversity. In particular Jaccard = beta3 + richness and Sorensen = Simpson + nestedness for faunistic indexes and Unifrac = Unifrac_turn + Unifrac_PD and PhyloSor = PhyloSor_turn + Phylosor_PD. Any other binary index can be included in brackets by using the syntax of designdist function of the vegan package.
}
\usage{
recluster.dist(mat, phylo=NULL, dist=c("simpson","sorensen","nestedness","beta3","richness","jaccard","phylosor","phylosort","phylosorpd","unifrac","unifract","unifracpd"))
}
\arguments{
  \item{mat}{
A matrix containing sites (rows) and species (columns).
}
  \item{phylo}{
An ultrametric and rooted phylo tree for species having the same labels as in mat columns. Only required for phylogenitic beta-diversity indexes.
}
  \item{dist}{
One among the twelve beta-diversity indexes \code{"simpson" "sorensen" "nestedness" "beta3" "richness" "jaccard" "phylosor" "phylosort" "phylosorpd" "unifrac" "unifract" "unifracpd"}. Any custom binary dissimilarity can also be specified according to the syntax of designdist function of the vegan package.
}
}
\details{
Syntax for binary indices in vegdist: J, number of common species; A and B, number of species exclusive of the first and of the second site. 
}
\value{
An object of class dist (see vegan:designdist for further details)
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
\keyword{ array }
