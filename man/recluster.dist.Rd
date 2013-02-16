\name{recluster.dist}
\alias{recluster.dist}
\title{
Compute a dissimilarity matrix using a battery of beta-diversity indices
}
\description{
This function computes dissimilarity matrices based on the two most popular partitions of beta-diversity. In particular Jaccard = beta3 + richness and Sorensen = Simpson + nestedness. Any other binary index can be included in brackets by using the syntax of designdist function of the vegan package.
}
\usage{
recluster.dist(mat, dist="simpson")
}
\arguments{
  \item{mat}{
A matrix containing sites (rows) and species (columns).
}
  \item{dist}{
One among the six beta-diversity indexes \code{"simpson" "sorensen" "nestedness" "beta3" "richness" "jaccard"}. Any custom binary dissimilarity can also be specified according to the syntax of designdist function of the vegan package.
}
}
\details{
Syntax for binary indices in vegdist: J, number of common species; A and B, number of species exclusive of the first and of the second site. 
}
\value{
An object of class dist (see vegan:designdist for further details)
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
\keyword{ array }
