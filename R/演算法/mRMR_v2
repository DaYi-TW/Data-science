mRMR=function(data_A,folder,result_D,test_data,s,Discrete,G1,criteria,option,test_M_data,beta_value,seed,evalresult){
	Discrete_data=as.data.frame(matrix(,nrow(data_A),))
	multi_criterion_pre=as.data.frame(matrix(,10,))
	multi_criterion=as.data.frame(matrix(,10,))
	temp_matrix=as.data.frame(matrix(,1,ncol(s)))
	
#	if(sum((result_D[ncol(result_D)]==colnames(test_data)[ncol(test_data)])=="TRUE")>0){
#		result_D=result_D[-which(result_D[,ncol(result_D)]==colnames(test_data)[ncol(test_data)]),]
#	}
	for(i in 1:nrow(result_D)){
		nnn1=""
		a=test_M_data
		position1=(nrow(as.data.frame(which(rowSums(!is.na(s))>0)))+1)			#屬性，計算要放在s表的起始位置
		position2=(nrow(as.data.frame(which(rowSums(!is.na(s))>0)))+ncol(a)-which(colnames(a)==paste0(result_D[i,ncol(result_D)])))    #屬性，計算要放在s表的終止位置
		a1=parSapply(cl,2:ncol(result_D),function(j,test_M_data,result_D,temp_matrix,i,position1,position2){
																	#放到ary變數去，準備進行配對
			ary=test_M_data[which(colnames(test_M_data)==result_D[i,j])]
			nnn1=paste0(result_D[i,j])
			return(list(ary=ary,nnn1=nnn1))
		},test_M_data,result_D,temp_matrix,i,position1,position2)
		ary=data.frame(matrix(unlist(a1[1,]),ncol=length(a1[1,])))
		nnn=data.frame(matrix(unlist(a1[2,]),ncol=length(a1[2,])))
		for(m in 1:length(a1[1,])){
			a=a[-which(colnames(a)==nnn[,m])]
			nnn1=paste0(nnn1,as.character(nnn[,m]))
			names(ary)[m]=as.character(nnn[,m])
			temp_matrix[,m]=as.character(nnn[,m])
		}	
		ste=0
			a2=parSapply(cl,1:(ncol(a)),function(k,evalresult,a,temp_matrix,ste,ary,multi_criterion_pre,multi_criterion,result_D,position1,nnn1,data_A,cl,test_M_data,option,Discrete_data,beta_value){
				temp_matrix[1,(ncol(temp_matrix)-1)]=colnames(a[k])					#把候選屬性紀錄在S表中
				nnn2=""
				nnn2=paste0(nnn1,colnames(a[k]))
				t1=cbind(ary,a[k])													#將ary與候選屬性a[k]組合起來準備進行離散運算
				print(paste0("G",ncol(result_D),"_",position1+ste))
				ste=ste+1
				
#---------------------------------mRMR---------------------------------------
				count=MI(t1,test_M_data)
#				f=cbind(a[k],data_A[ncol(data_A)])
#				for(k in 1:ncol(f)){
#					f[,k]=as.factor(f[,k])
#				}
#				colnames(f)[ncol(f)]='class'
#				answer=as.data.frame(InfoGainAttributeEval(class~.,data=f))-(1/(ncol(t1)))*count
				IG_a=evalresult[which(evalresult[2]==names(a[k])),1]
				answer=IG_a-(1/(ncol(t1)))*count
				temp_matrix[1,(ncol(result_D)+1)]=answer			
#---------------------------------mRMR---------------------------------------				
				
		
				return(list(Discrete_data=Discrete_data,temp_matrix=temp_matrix))
				
			},evalresult,a,temp_matrix,ste,ary,multi_criterion_pre,multi_criterion,result_D,position1,nnn1,data_A,cl,test_M_data,option,Discrete_data,beta_value)
			if(i ==1){
				aa2_Discrete_data=data.frame(matrix(unlist(a2[1,]),ncol=length(a2[1,])))
				aa2_temp_matrix=data.frame(matrix(unlist(a2[2,]),nrow=length(a2[2,]),byrow=T))
			}else{
				aa2_Discrete_data_temp=data.frame(matrix(unlist(a2[1,]),ncol=length(a2[1,])))
				aa2_temp_matrix_temp=data.frame(matrix(unlist(a2[2,]),nrow=length(a2[2,]),byrow=T))

				aa2_Discrete_data=cbind(aa2_Discrete_data,aa2_Discrete_data_temp)
				aa2_temp_matrix=rbind(aa2_temp_matrix,aa2_temp_matrix_temp)
			}
		
	}

	ans=list(s=aa2_temp_matrix,Discrete_data=aa2_Discrete_data)
	return(ans)
}
