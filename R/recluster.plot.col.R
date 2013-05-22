recluster.plot.col<-function (mat,cext=0.3,cex=1,cex.axis=0.7,cex.lab=0.8,text=TRUE,...) 
{
	plot(mat[,1],mat[,2],pch=19,col=rgb(mat[,3],mat[,4],mat[,5], maxColorValue = 255),cex=cex,xlim=c(0,1), ylim=c(0,1),ylab="Axis 2",xlab="Axis 1",cex.axis=cex.axis,cex.lab=cex.lab,...)
	if (text){text(mat[,1],mat[,2]-0.03,rownames(mat),cex=cext)}
}

