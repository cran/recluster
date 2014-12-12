\name{recluster.plot}
\alias{recluster.plot}
\title{
A plotter for recluster bootstrapped objects
}
\description{
This function produces plots for recluster trees and assign single or pairs of support values belonging to single or multiscale analyses.
}
\usage{
recluster.plot(tree, data, low = 1, high = 0, id=NULL, 
nodelab.cex=0.8, direction="downwards",...)
}
\arguments{
  \item{tree}{
A phylo tree presumably constructed with recluster.cons function.
}
  \item{data}{
A matrix belonging to recluster.multi.
}
  \item{id}{
A vector used to mark node supports (low and high) with different colours. Such classificarion is presumably made by recluster.identify.nodes.
}
  \item{low}{
The lower scale level which bootstrap values should be indicated in the tree.
}
  \item{high}{
The higher scale level which bootstrap values should be indicated in the tree.
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
A plot representing the tree with pairs of bootstrap values, below (usually x1 BP above) and high, above.
}
\details{
This function allow to select up to two labels for nodes based on bootstrap values and optimize their layout.
This is done with the \code{nodelabels} ape function, by specifying the \code{adj} parameters in the appropriate way.
}
\references{
Dapporto L., Ramazzotti M., Fattorini S., Talavera G., Vila R., Dennis R. 
"recluster: an unbiased clustering procedure for beta-diversity turnover"
Ecography (2013), 36:1070-1075.
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
boot<-recluster.boot(tree$cons,datamod, tr=10, boot=50)
recluster.plot(tree$cons,boot,direction="downwards")
}
\keyword{ hplot }

