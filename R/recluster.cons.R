recluster.cons <- function(mat,phylo=NULL,tr=100,p=0.5,dist="simpson", method="average") {
	trees<-(as.phylo(hclust(recluster.dist(mat,phylo,dist),method=method)))
	for (i in 1 : tr){
		datas<-(mat[sample(1:nrow(mat)),])
		trees[[i]]<- as.phylo(hclust(recluster.dist(datas,phylo,dist),method=method))
	}
	cons<-multi2di(compute.brlen(consensus(trees[1:i],p=p, check.labels=T), method="Grafen"), random=T)
	cons
}
