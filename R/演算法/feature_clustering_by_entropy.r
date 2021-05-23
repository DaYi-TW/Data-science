feature_clustering_by_entropy=function(data_A,filename,folder,m=0,seed=1){

data_B=as.data.frame(t(data_A[-ncol(data_A)]))
DD=read.csv(file=paste0("C:/R/Result/",folder,"/",filename,"_Discrete_data",".csv"))[-1]
ED=parSapply(cl,1:ncol(DD),function(i,DD){entropy(DD[i])},DD)

#--------------------------------------------------
entropy_clustering=floor(ED)
#for(i in 0:floor(ED[which.max(ED)])){
#	if(length(ED[which(ED<=i)])!=0){
#		entropy_clustering[which(ED>i&ED<=(i+1)),]=i
#		cluster_center[,]=rowMeans(DD[which(ED>i&ED<=(i+1))]))
#	}
#}


cluster_center=as.data.frame(parSapply(cl,1:length(table(floor(ED))),function(i,ED,DD,entropy_clustering){
	rowMeans(DD[which(entropy_clustering==as.numeric(names(table(floor(ED)))[i]))])
},ED,DD,entropy_clustering))
colnames(cluster_center)=names(table(floor(ED)))
cluster_analysis=((parSapply(cl,1:ncol(cluster_center),function(i,cluster_center,data_A){
		cbind(
			IG(data_A,cluster_center[i]),
			GR(data_A,cluster_center[i]),
			SU(data_A,cluster_center[i]),
			CHI(data_A,cluster_center[i])
		)
},cluster_center,data_A)))
cluster_analysis=as.data.frame(t(matrix(unlist(cluster_analysis), nrow = nrow(cluster_analysis))))
cluster_analysis=cbind(cluster_analysis,as.data.frame(table(floor(ED)))[-1])
colnames(cluster_analysis)=c("IG","GR","SU","Chi-Square","quantity")
rownames(cluster_analysis)=names(table(floor(ED)))




write.csv(cluster_analysis,file=paste0("C:/R/Result/",folder,"/",filename,"_cluster_analysis_by_entropy.csv"))
write.csv(cluster_center,file=paste0("C:/R/Result/",folder,"/",filename,"_cluster_center.csv"))
write.csv(entropy_clustering,file=paste0("C:/R/Result/",folder,"/",filename,"_entropy_clustering.csv"))

return(print("feature clustering by entropy complete"))
}