\name{recluster.plot.matrix}
\alias{recluster.plot.matrix}
\title{
Plot cell values of a matrix in grey scale
}
\description{
This function plots values of single cells of a matrix in grey scale.
}
\usage{
recluster.plot.matrix(mat)
}
\arguments{
  \item{mat}{
A dissimilarity matrix.
}
}
\value{
A plot of cell values.
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
simpdiss<- recluster.dist(datamod)
recluster.plot.matrix(simpdiss)
}
\keyword{ dissimilarity }
