\name{recluster.hist}
\alias{recluster.hist}
\title{
Histogram with tied and zero values
}
\description{
This function creates a histogram where the number of cells with zero value are explicitely showed in the first bar. Moreover, it provides the percentage of cells having equal values in the matrix.
}
\usage{
recluster.hist(x, dist="simpson")
}
\arguments{
  \item{x}{
A dissimilarity matrix.
}
  \item{dist}{
One among the six beta-diversity indexes \code{"simpson" "sorensen" "nestedness" "beta3" "richness" "jaccard"}. Any custom binary dissimilarity can also be specified according to the syntax of designdist function of the vegan package.
}
}
\value{
An histogram with supplementary information. The first bar only show the zero values.
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
simpdiss<- recluster.dist(datamod)
recluster.hist(simpdiss)
}
\keyword{ cluster }
