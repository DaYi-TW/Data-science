MFSS_feature_estimation_for_all_dataset=function(data_A,G1_Discrete,criteria){ 

	List_time <-list()
	result_D=matrix(,,100)
	M_data=G1_Discrete$M_data
	time_1=G1_Discrete$time_1
ptm <- proc.time()	
	

	
	a2=parSapply(cl,1:ncol(M_data),function(i,criteria,data_A,M_data){
		answer=criteria(data_A,M_data[i])
		return(answer)		
	},criteria,data_A,M_data)
	a2=data.frame(t(matrix(unlist(a2),ncol=length(a2))))
	rownames(a2)=colnames(M_data)
	colnames(a2)="IG_value"
	M_result=a2
	result_D=as.data.frame(M_result[1])

List_time[[1]]=(proc.time() - ptm)+time_1

	G1=list(result=result_D,List_time=List_time[[1]],M_data=M_data)
	
	


	return(G1)
}