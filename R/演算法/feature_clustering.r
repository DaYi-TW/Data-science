feature_clustering=function(data_A,method,folder,m,seed){

#h=(1-dataset_imformative_quality[3])
#if(h==0){
#	center=500
#}else{
#	center=ceiling(1/h)
#}
data_B=as.data.frame(t(data_A[-ncol(data_A)]))
#data_B=as.data.frame(t(data_A))
fcm.phase1=switch(x,
fcm=fcm(data_B,centers=as.numeric(center),m=4),
EM=init.EM(data_B, nclass = 4, method = "em.EM"),
KM=kmeans(data_B,  centers=4)
)
  

#fcm.phase1=fcm(data_B,centers=as.numeric(center),m=4)
#fcm.phase1=init.EM(data_B, nclass = 9, method = "em.EM")
fcm.phase1.cluster=fcm.phase1$cluster
fcm.phase1.center=fcm.phase1$v
fcm.phase2=fcm.phase1.center[as.numeric(names(table(fcm.phase1.cluster))),]
fcm.phase3=as.data.frame(t(fcm.phase2))
fcm.phase4=parSapply(cl,1:ncol(fcm.phase3),function(i,fcm.phase3,seed){FCM_C_AutoSelect(fcm.phase3[i],4,seed)$cluster},fcm.phase3,seed)
fcm.phase5=data.frame(matrix(unlist(fcm.phase4),ncol=length(fcm.phase4)))


fcm.phase.evaluation.store=as.data.frame(matrix(,nrow(fcm.phase2),4))
rownames(fcm.phase.evaluation.store)=rownames(fcm.phase2)
colnames(fcm.phase.evaluation.store)=c("number of c","IG","GR","Chi-Square")		
fcm.phase.evaluation.store[1]=c(table(fcm.phase1.cluster))
fcm.phase.evaluation.store[2]=IG(data_A,fcm.phase5)
fcm.phase.evaluation.store[3]=GR(data_A,fcm.phase5)
fcm.phase.evaluation.store[4]=CHI(data_A,fcm.phase5)

write.csv(fcm.phase.evaluation.store,file=paste0("C:/R/Result/",folder,"/",filename,"fcm_clusters_evaluation.csv"))
write.csv(fcm.phase1.cluster,file=paste0("C:/R/Result/",folder,"/",filename,"fcm_phase_cluster.csv"))
write.csv(fcm.phase3,file=paste0("C:/R/Result/",folder,"/",filename,"fcm_phase_center.csv"))
write.csv(fcm.phase5,file=paste0("C:/R/Result/",folder,"/",filename,"fcm_phase_center_discretization.csv"))

return(list(fcm.phase1.cluster=fcm.phase1.cluster,fcm.phase.evaluation.store=fcm.phase.evaluation.store))
}
