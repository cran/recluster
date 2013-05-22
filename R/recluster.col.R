recluster.col<-function (mat) 
{
	mat[,1]<-(mat[,1]-min(mat[,1]))/(max(mat[,1])-min(mat[,1]))
	mat[,2]<-(mat[,2]-min(mat[,2]))/(max(mat[,2])-min(mat[,2]))
	colour<-array(data=0,dim=c(dim(mat)[1],(dim(mat)[2])+3))
	for (t in 1 : dim(mat)[1]){
		colour[t,1]<-mat[t,1]
		colour[t,2]<-mat[t,2]
		colour[t,3]<-round(mat[t,2]*255)
		colour[t,4]<-round(mat[t,1]*255)
		colour[t,5]<-round((1-(mat[t,2]))*255)
		rownames(colour)<-rownames(mat)
	}
	return(colour)
}