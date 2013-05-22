recluster.plot.sites.col<-function (long,lat,mat,cext=0.3,cex=1,cex.axis=0.7,cex.lab=0.8,text=FALSE,...) 
{
	plot(long,lat,pch=19,col=rgb(mat[,3],mat[,4],mat[,5], maxColorValue = 255),cex=cex, cex.axis=cex.axis,cex.lab=cex.lab,...)
	if (text){text(long,lat-0.1,rownames(mat),cex=cext)}
}
