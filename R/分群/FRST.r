FRST=function(decision.table,index1){

condAttr<-c(index1)
decAttr<-c(ncol(decision.table))
control.ind<-list(type.aggregation=c("t.tnorm","lukasiewicz"),type.relation=c("tolerance","eq.1"))
control.dec<-list(type.aggregation=c("crisp"),type.relation="crisp")
IND.condAttr<-BC.IND.relation.FRST(decision.table,attributes=condAttr,control=control.ind)
IND.decAttr<-BC.IND.relation.FRST(decision.table,attributes=decAttr,control=control.dec)
control<-list(t.implicator="lukasiewicz",t.tnorm="lukasiewicz")
FRST.LU<-BC.LU.approximation.FRST(decision.table,IND.condAttr,IND.decAttr,type.LU="implicator.tnorm",control=control)
res1<-BC.positive.reg.FRST(decision.table,FRST.LU)

#upper_g1=as.data.frame(matrix(,length(table(decision.table[ncol(decision.table)])),nrow(decision.table)+1))
#upper_g1_mat=t(sapply(1:length(table(decision.table[ncol(decision.table)])),function(i,FRST.LU){sprintf("%.3f",unlist(FRST.LU$fuzzy.upper[i]))},FRST.LU))
#upper_g1[,1]=names(FRST.LU$fuzzy.upper)
#upper_g1[,2:ncol(upper_g1)]=upper_g1_mat
#names(upper_g1)=c("class",c(1:nrow(decision.table)))
#----------------------------------
#lower_g1=as.data.frame(matrix(,length(table(decision.table[ncol(decision.table)])),nrow(decision.table)+1))
#lower_g1_mat=t(sapply(1:length(table(decision.table[ncol(decision.table)])),function(i,FRST.LU){sprintf("%.3f",unlist(FRST.LU$fuzzy.upper[i]))},FRST.LU))
#lower_g1[,1]=names(FRST.LU$fuzzy.lower)
#lower_g1[,2:ncol(lower_g1)]=lower_g1_mat
#names(lower_g1)=c("class",c(1:nrow(decision.table)))
#----------------------------------
#res_g1=as.data.frame(matrix(,length(table(decision.table[ncol(decision.table)])),2))
#res_g1[,1]=names(FRST.LU$fuzzy.lower)
#res_g1[,2]=(sapply(1:length(table(decision.table[ncol(decision.table)])),function(i,FRST.LU){sum(unlist(FRST.LU$fuzzy.lower[i]))/ sum(unlist(FRST.LU$fuzzy.upper[i]))},FRST.LU))
#names(res_g1)=c("class","degree.dependency_by_class")
return(res1$degree.dependency)
}