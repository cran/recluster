recluster.expl.diss <- function(tree,dist){
	dist<-as.matrix(dist)
	nclust<-NULL
	result<-NULL
	res<-NULL
	tree <- reorder(tree)
	mat <- nodeHeights(tree)
	mat2<-mat[order(mat[,1],mat[,2]),]
	mat2<-mat2[mat2[,1]+mat2[,2]!=0,]
	mat2<-mat2[!duplicated(mat2[,1]),]
	mat2<-mat2[mat2[,1]!=mat2[,2],]
	matrix<-matrix(data=NA,ncol=nrow(mat2),nrow=length(tree$tip.label))
	comp<-rownames(dist)
	for (cl in 1:nrow(mat2)) {
		res<-treeSlice(tree,mat2[cl,1]+0.000001,trivial=TRUE)
		sub<-length(res)
		nclust[cl]<-sub
		for (subtrees in 1:sub) {
			tip<-length(res[[subtrees]]$tip.label)
			for (tp in 1:tip) {
				pos<-match(res[[subtrees]]$tip.label[tp],comp)
				matrix[pos,cl]<-subtrees
				}
			}
		}
	beta<-sum(dist)
	cluster<-NULL
for (loops in 1:(nrow(mat2)-1)){
		ref<-as.numeric(matrix[,loops])
		betapartial<-0
		for (row in 1:nrow(dist)){
			for (col in 1:ncol(dist)){
			if(ref[row]!=ref[col]){betapartial<-betapartial+dist[row,col]}
			}
		}
		cluster[loops]<-betapartial/beta
	}
	rownames(matrix)<-comp
	result$matrix<-matrix
	result$expl.div<-cluster
	result$nclust<-nclust
	return(result)
}

