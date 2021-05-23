Dataset_Discretization=function(data_A,filename,folder,method,methodname,Discrete,seed){
	
	M_data=matrix(,nrow(data_A),ncol(data_A)-1)
	M_data=as.data.frame(M_data)
	ptm <- proc.time()	

		if(methodname=="SELF"){
		
			a=parSapply(cl,1:(ncol(data_A)-1),function(i,data_A){
				if(length(table(as.matrix(data_A[i])))>Discrete){
					as.data.frame(method(cbind(as.numeric(as.matrix(data_A[i])),as.numeric(as.matrix(data_A[i]))),Discrete))
				}else{
					data_A[i]
				}
			},data_A)
			M_data=data.frame(a)
			colnames(M_data)=colnames(data_A)[1:(ncol(data_A)-1)]
			
		}else if(methodname=="FCM"){

			a=parSapply(cl,1:(ncol(data_A)-1),function(i,data_A,seed){
				if(length(table(as.matrix(data_A[i])))>7){
					FCM_C_AutoSelect(data_A[i],4,seed)$cluster
				}else{
					data_A[i]
				}
			},data_A,seed)
			M_data=data.frame(a)
			colnames(M_data)=colnames(data_A)[1:(ncol(data_A)-1)]
		}else{
			a=parSapply(cl,1:(ncol(data_A)-1),function(i,data_A){
				if(length(table(as.matrix(data_A[i])))>Discrete){
					as.data.frame(method(as.numeric(as.matrix(data_A[i])),Discrete))
				}else{
					data_A[i]
				}
			},data_A)
			M_data=data.frame(a)
			colnames(M_data)=colnames(data_A)[1:(ncol(data_A)-1)]
		}
		
		M_data=as.data.frame(M_data)

	time_1=proc.time() - ptm
	G1_Discrete=list(M_data=M_data,time_1=time_1)
	write.csv(M_data,file=paste0("./",folder,"/",filename,"_Discrete_data",".csv"))

	return(G1_Discrete)
}
