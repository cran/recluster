\name{recluster.plot}
\alias{recluster.plot}
\title{
A plotter for recluster bootstrapped objects
}
\description{
This function produce plots for recluster trees and assign single or pairs of support values for single or multilevel analyses.
}
\usage{
recluster.plot(tree, data, low = 1, high = 0, id=NULL, nodelab.cex=0.8, direction="downwards",...)
}

\arguments{
  \item{tree}{
A phylo tree presumably constructed with recluster.cons function.
}
  \item{data}{
A recluster.mult result.
}
  \item{id}{
A vector to colour node support with different colours as presumably identified by recluster.identify.nodes.
}
  \item{low}{
The lower level which values should be included in the tree.
}
  \item{high}{
The higher level which values should be included in the tree.
}
  \item{nodelab.cex}{
the cex() parameter for controlling the size of the labels on the nodes (see \code{?nodelabels}).
}
  \item{direction}{
the \code{direction} parameter for controlling the orientation of the plot, see the ape package manual and \code{?plot.phylo}.
This parameters also controls the display of the labels on nodes.
}
  \item{...}{
Arguments to be passed to plot.phylo methods, see the ape package manual and \code{?plot.phylo}.
}
}
\value{
A plot representing the tree with pairs of bootstrap values, low (usually x1 BP above) and high below.
}
\details{
This function allow to select up to two labels for nodes based on bootstrap values nad optimize their layout.
Thisis done with the \code{nodelabels} ape function, by specifying the \code{adj} parameters in the appropriate way.
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
tree<-recluster.cons(datamod, tr=10)
boot<-recluster.boot(tree,datamod, tr=10, boot=50)
recluster.plot(tree,boot,direction="downwards")
}
\keyword{ hplot }

