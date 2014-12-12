recluster.region<-function(mat,tr=10,dist="simpson",method="ward",phylo=NULL, mincl=2,maxcl=3,rettree=FALSE,retmat=FALSE,retmemb=FALSE){
        clusters<-maxcl-mincl+1
        res<-NULL
        expl2<-matrix(data=NA,clusters,ncol=tr)
        nclust<-expl2   
        tab2<-array(NA,dim=c(nrow(mat),tr,clusters))
        rownames(tab2)<-rownames(mat)
        mat2<-mat
        rownames(mat2)<-c(1:nrow(mat))
        for(i in 1:tr){
                table_r<-mat2[sample(1:nrow(mat2)),]
                dista<-recluster.dist(table_r, phylo=phylo, dist=dist)
                tree<-as.phylo(hclust(dista,method=method))
                exp<-recluster.expl.diss(tree,dista,mincl=mincl-1,maxcl=maxcl-1,expld=F)
                tree<-NULL
                expmat<-as.matrix(exp$matrix)
                expmat<-as.matrix(expmat[order(as.numeric(rownames(expmat))),])
                nclust[1:length(exp$nclust),i]<-exp$nclust
                        for(t in 1 : ncol(expmat)){
                        tab2[1:nrow(expmat),i,t]<-expmat[,t]
                        }
        }
        exp<-NULL
        expmat<-NULL
        nclust<-as.matrix(rbind(rep(1,tr),nclust))
        nclust<-nclust[which(rowSums(nclust,na.rm=T)>0),]
        matrices<-array(NA,dim=c(nrow(mat),nrow(mat),nrow(nclust)-1))
        for(sel in 1:(nrow(nclust)-1)){
                tabsel<-tab2[,,sel]
                for(cl in 1:nrow(tabsel)){
                        for(rw in cl:nrow(tabsel)){
                                     vect<-round((tabsel[rw,]-tabsel[cl,])/(tabsel[rw,]-tabsel[cl,]+0.0001),0)
                                     matrices[rw,cl,sel]<-sum(vect,na.rm=T)/tr
                                     }
                        }
                }
        tabsel<-NULL
        if(retmemb){res$memb<-tab2}
        tab2<-NULL
        if(retmat){res$matrices<-matrices}
        n.clust<-rowMeans(nclust,na.rm=T)
        res$nclust<-n.clust[2:nrow(nclust)]
        solution<-c(mincl:maxcl)
        if(sum(solution[which(solution>=nrow(mat))])>0){solution<-solution[-which(solution>=nrow(mat))]}
        sol<-matrix(data=NA, nrow=length(solution),ncol=length(res$nclust))
        for (solu in 1 : nrow(sol)){
                 sol[solu,]<-abs(solution[solu]-res$nclust)
                 }
        distance<-recluster.dist(mat,dist=dist)
        pamsol<-matrix(data=NA, nrow=nrow(mat),ncol=length(solution))
        colnames(pamsol)<-c(min(solution):max(solution))
        rownames(pamsol)<-rownames(mat)
        res$solutions<-matrix(data=NA, nrow=length(solution),ncol=4)
        colnames(res$solutions)<-c("k","clust","silh","ex.diss")
        res$solutions[,1]<-solution
        for (pamr in 1:length(solution)){
                 min<-which(sol[pamr,]==min(sol[pamr,]))
                 pami<-as.phylo(hclust(as.dist(matrices[,,min]),method=method))
                 exppami<-recluster.expl.diss(pami,as.dist(matrices[,,min]),mincl=solution[pamr]-1,maxcl=solution[pamr]-1)
                 pamsol[,pamr]<-exppami$matrix
                 if(rettree){res$tree[[pamr]]<-pami}
                 pami<-NULL
                 res$solutions[pamr,2]<-res$nclust[min]
                 res$solutions[pamr,4]<-recluster.expl(distance,pamsol[,pamr])
                 res$solutions[pamr,3]<-mean(silhouette(pamsol[,pamr],distance)[,3])
                 }
        res$grouping<-pamsol
        return(res)
}
