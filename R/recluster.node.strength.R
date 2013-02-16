recluster.node.strength <-function (mat, dist="simpson", tr=100, method="average"){
	out<-NULL
	treesstr<-(as.phylo(hclust(recluster.dist(mat,dist),method=method)))
	tree1<-treesstr
	for (cons in 1 : 6){
		p<-0.5+((cons-1)*0.1)
		treesstr[[cons]]<-recluster.cons(mat,dist=dist,tr=tr,method=method,p=p)
	}
	btr2 <- .compressTipLabel(treesstr)
	tr2 <- recluster.check(tree1, attr(btr2, "TipLabel"))
	btr2 <- .uncompressTipLabel(btr2)
	result <- prop.clades(tr2, btr2, rooted=T)*(100/6)
	recluster.plot(tree1,as.matrix(result))
	out$result<-as.matrix(result)
	out$tree<-tree1
	return(out)
}
