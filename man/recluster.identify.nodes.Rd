\name{recluster.identify.nodes}
\alias{recluster.identify.nodes}
\title{
Evaluating solutions in multiscale bootstrap
}
\description{
This function helps to understand different behaviours of node supports by i)plotting trends of support values in different bootstrap scales, ii) identifying the boostrap scale with highest diversification between two groups of nodes and iii) identifying nodes into two classes according to the best bootstrap level identified in (ii) and ploting their mean support values. 
}
\usage{
recluster.identify.nodes(mat, low=TRUE)
}
\arguments{
  \item{mat}{
A matrix containing nodes (rows) and bootstrap levels (columns) as obtained by recluster.multi.
}
  \item{low}{
A logical value indicating if lower scales should be favoured in the selection.
}
  }
\details{
This function mainly serves to recognize nodes showing different rates of support increasing at some stage of a multilevel bootstrap. In the analysis of turnover in biogeography some nodes may show a rapidly increasing support in a multiscale bootstrap. This means that the areas connected by these nodes host only a few species responsible for turnover, but the biogeographic pattern with respect to other areas is clear. Another kind of node may show a slow (or no) increase in support. The links among areas are actually uncertain. Partitioning Around Medioids is used to identify two classes of nodes at each level, then the bootstrap level showing the best diversification in two classes is identified by silhouette scores weighted by differences in mean values between classes. If low is set to  and favouring low scales. 
}
\value{
A plot with bootstrap supports and their means (diamonds) for the best combination of two groups of nodes (black and red).
	\item{scale}{
The best bootstrap scale to identify two groups of nodes.
}
	\item{nodes}{
A vector containing classification for nodes in the best bootstrap scale.
}
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
data(multiboot)
recluster.identify.nodes(multiboot)
}
\keyword{ bootstrap }
