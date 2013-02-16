\name{recluster}
\alias{recluster}
\alias{reCluster}
\docType{package}
\title{
Solving biasing produced by tied values in cluster analysis.
}
\description{
A high frequency of ties and zero values produces hierarchical cluster dendrograms whose topology and bootstrap supports are affected by the order of rows in the original matrix. The recluster package evaluates the magnitude of this bias and computes consensus trees after re-sampling the original row order also applying this algorithm to a multiscale bootstrap procedure. 
}
\details{
\tabular{ll}{
Package: \tab recluster\cr
Type: \tab Package\cr
Version: \tab 1.5\cr
Date: \tab 2013-02-12\cr
License: \tab GPL (>= 2.0)\cr
}
}
\author{
Leonardo Dapporto, Matteo Ramazzotti, Simone Fattorini, Roger Vila, Gerard Talavera, Roger H.L. Dennis

Maintainer: Leonardo Dapporto <leondap@gmail.com>

}
\references{
Dapporto L., Ramazzotti M., Fattorini S., Vila R., Talavera G., Dennis R. 
"recluster: an unbiased clustering procedure for beta-diversity turnover"
XXXX (2013), XXX:XXX.
\url{
www.unifi.it/scibio/bioinfo/recluster.html
}
}
\keyword{ package }
\examples{
#load butterfly data provided with the package 
data(datamod)

#explore zeroes and tien in the data set
simpdiss<- recluster.dist(datamod)
recluster.hist(simpdiss)

#create and view unbiased consensus tree
constree_full<-recluster.cons(datamod, tr=10, p=1)
plot(constree_full,direction="downwards")

#compute and view node strength
recluster.node.strength(datamod, tr=10)

#create and view unbiased consensus tree (50% rule)
constree_half<-recluster.cons(datamod, tr=10, p=0.5)
plot(constree_half, direction="downwards")

#the latter is the correct tree
tree<-constree_half

#perform and view bootstrap on nodes
boot<-recluster.boot(tree, datamod, tr=10, p=0.5, boot=50)
recluster.plot(tree,boot)

#perform and view multiscale bootstrap on nodes
multiboot<- recluster.multi(tree, datamod, tr=10, boot=50, levels=2, step=1)
recluster.plot(tree,multiboot,low=1,high=2, direction="downwards")
}
