interior_cor_first=function(data_A,result_D,test_data,s,method,Discrete,G1,beta_value,criteria,d,option,test_M_data,filename,simulation){
	Discrete_data=as.data.frame(matrix(,nrow(data_A),))
	multi_criterion_pre=as.data.frame(matrix(,10,))
	multi_criterion=as.data.frame(matrix(,10,))
	temp_matrix=as.data.frame(matrix(,1,ncol(s)))
	
	if(sum((result_D[ncol(result_D)]==colnames(test_data)[ncol(test_data)])=="TRUE")>0){
		result_D=result_D[-which(result_D[,ncol(result_D)]==colnames(test_data)[ncol(test_data)]),]
	}
	for(i in 1:nrow(result_D)){
		nnn1=""
		a=test_data
		position1=(nrow(as.data.frame(which(rowSums(!is.na(s))>0)))+1)			#屬性，計算要放在s表的起始位置
		position2=(nrow(as.data.frame(which(rowSums(!is.na(s))>0)))+ncol(a)-which(colnames(a)==paste0(result_D[i,ncol(result_D)])))    #屬性，計算要放在s表的終止位置
		a1=parSapply(cl,2:ncol(result_D),function(j,test_data,result_D,temp_matrix,i,position1,position2){
																	#放到ary變數去，準備進行配對
			ary=test_data[which(colnames(test_data)==result_D[i,j])]
			nnn1=paste0(result_D[i,j])
			return(list(ary=ary,nnn1=nnn1))
		},test_data,result_D,temp_matrix,i,position1,position2)
		ary=data.frame(matrix(unlist(a1[1,]),ncol=length(a1[1,])))
		nnn=data.frame(matrix(unlist(a1[2,]),ncol=length(a1[2,])))
		for(m in 1:length(a1[1,])){
			nnn1=paste0(nnn1,as.character(nnn[,m]))
			names(ary)[m]=as.character(nnn[,m])
			temp_matrix[,m]=as.character(nnn[,m])
		}
		
		ste=0

		msg=tryCatch({
			op=as.data.frame(read.csv(file=paste0("C:/R/Result/",filename,"_multi_criterion_",(ncol(ary)+1),".csv"),1))
			for(h in 1:nrow(op)){	
				rownames(op)[h]=paste0(op[h,1])
			}
			op=op[-1]
			}, error = function(e) {
			conditionMessage(e) #錯誤偵測，因為跑第一次時不會有multi_criterion這個資料，所以要做出一個可以判別的
		})	
	
		if(length(msg)==1){
		print("Do not use multi_criterion data")
		multi_criterion=""
			a2=parSapply(cl,(which(colnames(ary[ncol(ary)])==colnames(a))+1):(ncol(a)),function(k,a,Discrete,temp_matrix,ste,ary,criteria,multi_criterion_pre,multi_criterion,result_D,position1,nnn1,data_A,simulation,cl,test_M_data,option,Discrete_data){
				temp_matrix[1,(ncol(temp_matrix)-1)]=colnames(a[k])					#把候選屬性紀錄在S表中
				nnn2=""
				nnn2=paste0(nnn1,colnames(a[k]))
				t1=cbind(ary,a[k])													#將ary與候選屬性a[k]組合起來準備進行離散運算
				print(paste0("G",ncol(result_D),"_",position1+ste))
			
				t2=FCM_C_AutoSelect(t1,Discrete)			
				
	
				t_result=criteria(data_A,t2)		#進行剛剛離散值t，再進行評估法的計算
				initial=as.data.frame(t_result[1])
				
				if(simulation==3){
					interior_cor=abs(interior(t1,test_M_data,option))
					level_value=level(initial,interior_cor,data_A)
					index=initial/level_value
					temp_matrix[1,(ncol(result_D)+1)]=index
				}else{
					index=initial
					temp_matrix[1,(ncol(result_D)+1)]=index
				}
				interior_IG=abs(interior(t1,test_M_data,"IG"))
				interior_GR=abs(interior(t1,test_M_data,"GR"))
				interior_COR=abs(interior(t1,test_M_data,"COR"))
				interior_CHI=abs(interior(t1,test_M_data,"CHI"))
				interior_SU=abs(interior(t1,test_M_data,"SU"))
				
				IG_value=as.data.frame(IG(data_A,t2))
				GR_value=as.data.frame(GR(data_A,t2))
				COR_value=abs(as.data.frame(COR(data_A,t2)))
				CHI_value=as.data.frame(CHI(data_A,t2))
				SU_value=as.data.frame(SU(data_A,t2))
				
				
				multi_criterion_pre[1,]=IG_value
				multi_criterion_pre[2,]=GR_value
				multi_criterion_pre[3,]=COR_value	
				multi_criterion_pre[4,]=CHI_value
				multi_criterion_pre[5,]=SU_value
				multi_criterion_pre[6,]=interior_IG
				multi_criterion_pre[7,]=interior_GR
				multi_criterion_pre[8,]=interior_COR
				multi_criterion_pre[9,]=interior_CHI
				multi_criterion_pre[10,]=interior_SU
				
				multi_criterion=multi_criterion_pre
				colnames(multi_criterion)=nnn2
	
	
				Discrete_data=as.data.frame(t2)										#將離散值記錄起來
				names(Discrete_data)=nnn2
		
				
				ste=ste+1
				
				return(list(multi_criterion=multi_criterion,Discrete_data=Discrete_data,temp_matrix=temp_matrix))
				
			},a,Discrete,temp_matrix,ste,ary,criteria,multi_criterion_pre,multi_criterion,result_D,position1,nnn1,data_A,simulation,cl,test_M_data,option,Discrete_data)
			if(i ==1){
				aa2_multi_criterion=data.frame(matrix(unlist(a2[1,]),ncol=length(a2[1,])))
				aa2_Discrete_data=data.frame(matrix(unlist(a2[2,]),ncol=length(a2[2,])))
				aa2_temp_matrix=data.frame(matrix(unlist(a2[3,]),nrow=length(a2[3,]),byrow=T))
				for(m in 1:length(a2[1,])){
					names(aa2_multi_criterion)[m]=names(a2[1,][[m]])
					names(aa2_Discrete_data)[m]=names(a2[1,][[m]])
				}
			}else{
				aa2_multi_criterion_temp=data.frame(matrix(unlist(a2[1,]),ncol=length(a2[1,])))
				aa2_Discrete_data_temp=data.frame(matrix(unlist(a2[2,]),ncol=length(a2[2,])))
				aa2_temp_matrix_temp=data.frame(matrix(unlist(a2[3,]),nrow=length(a2[3,]),byrow=T))
				for(m in 1:length(a2[1,])){
					names(aa2_multi_criterion_temp)[m]=names(a2[1,][[m]])
					names(aa2_Discrete_data_temp)[m]=names(a2[1,][[m]])
				}
				aa2_multi_criterion=cbind(aa2_multi_criterion,aa2_multi_criterion_temp)
				aa2_Discrete_data=cbind(aa2_Discrete_data,aa2_Discrete_data_temp)
				aa2_temp_matrix=rbind(aa2_temp_matrix,aa2_temp_matrix_temp)
			}
		}else{
			#此處為如果multi_criterion跟我要做的部分重疊，那麼直接使用multi_criterion的資料，避免實驗環境不同
			print("using multi_criterion data")
			
			a2=parSapply(cl,(which(colnames(ary[ncol(ary)])==colnames(a))+1):(ncol(a)),function(k,Discrete,msg,a,temp_matrix,ste,ary,criteria,multi_criterion_pre,multi_criterion,result_D,position1,nnn1,data_A,simulation,cl,test_M_data,option,Discrete_data){
				
				feature=colnames(a[k])
				temp_matrix[1,(ncol(temp_matrix)-1)]=feature					#把候選屬性紀錄在S表中
				nnn2=""
				nnn2=paste0(nnn1,feature)
				#------------------------------------------------------
				if(length(msg[which(rownames(msg)==option),which(colnames(msg)==paste0(nnn1,feature))])==0){
					t1=cbind(ary,a[k])													#將ary與候選屬性a[k]組合起來準備進行離散運算
					print(paste0("G",ncol(result_D),"_",position1+ste))
				
					t2=FCM_C_AutoSelect(t1,Discrete)			
					
		
					t_result=criteria(data_A,t2)		#進行剛剛離散值t，再進行評估法的計算
					initial=as.data.frame(t_result[1])
					
					if(simulation==3){
						interior_cor=abs(interior(t1,test_M_data,option))
						level_value=level(initial,interior_cor,data_A)
						index=initial/level_value
						temp_matrix[1,(ncol(result_D)+1)]=index
					}else{
						index=initial
						temp_matrix[1,(ncol(result_D)+1)]=index
					}
					interior_IG=abs(interior(t1,test_M_data,"IG"))
					interior_GR=abs(interior(t1,test_M_data,"GR"))
					interior_COR=abs(interior(t1,test_M_data,"COR"))
					interior_CHI=abs(interior(t1,test_M_data,"CHI"))
					interior_SU=abs(interior(t1,test_M_data,"SU"))
					
					IG_value=as.data.frame(IG(data_A,t2))
					GR_value=as.data.frame(GR(data_A,t2))
					COR_value=abs(as.data.frame(COR(data_A,t2)))
					CHI_value=as.data.frame(CHI(data_A,t2))
					SU_value=as.data.frame(SU(data_A,t2))
					
					
					multi_criterion_pre[1,]=IG_value
					multi_criterion_pre[2,]=GR_value
					multi_criterion_pre[3,]=COR_value	
					multi_criterion_pre[4,]=CHI_value
					multi_criterion_pre[5,]=SU_value
					multi_criterion_pre[6,]=interior_IG
					multi_criterion_pre[7,]=interior_GR
					multi_criterion_pre[8,]=interior_COR
					multi_criterion_pre[9,]=interior_CHI
					multi_criterion_pre[10,]=interior_SU
				
					multi_criterion=multi_criterion_pre
					colnames(multi_criterion)=nnn2
					Discrete_data=as.data.frame(t2)										#將離散值記錄起來
					names(Discrete_data)=nnn2
					ste=ste+1
				}else{
					if(simulation==2){
						temp_matrix[1,(ncol(result_D)+1)]=msg[which(rownames(msg)==option),which(colnames(msg)==paste0(nnn1,feature))]
					}else{
						level_value=level(msg[which(rownames(msg)==paste0(option)),which(colnames(msg)==paste0(nnn1,feature))],msg[which(rownames(msg)==paste0("interior_",option)),which(colnames(msg)==paste0(nnn1,feature))],data_A)							
						temp_matrix[1,(ncol(result_D)+1)]=msg[which(rownames(msg)==option),which(colnames(msg)==paste0(nnn1,feature))]/level_value
					}
				}
				return(list(multi_criterion=multi_criterion,Discrete_data=Discrete_data,temp_matrix=temp_matrix))
				
			},Discrete,msg,a,temp_matrix,ste,ary,criteria,multi_criterion_pre,multi_criterion,result_D,position1,nnn1,data_A,simulation,cl,test_M_data,option,Discrete_data)
			if(i ==1){
				aa2_multi_criterion=data.frame(matrix(unlist(a2[1,]),ncol=length(a2[1,])))
				aa2_Discrete_data=data.frame(matrix(unlist(a2[2,]),ncol=length(a2[2,])))
				aa2_temp_matrix=data.frame(matrix(unlist(a2[3,]),nrow=length(a2[3,]),byrow=T))
				for(m in 1:length(a2[1,])){
					names(aa2_multi_criterion)[m]=names(a2[1,][[m]])
					names(aa2_Discrete_data)[m]=names(a2[1,][[m]])
				}
			}else{
				aa2_multi_criterion_temp=data.frame(matrix(unlist(a2[1,]),ncol=length(a2[1,])))
				aa2_Discrete_data_temp=data.frame(matrix(unlist(a2[2,]),ncol=length(a2[2,])))
				aa2_temp_matrix_temp=data.frame(matrix(unlist(a2[3,]),nrow=length(a2[3,]),byrow=T))
				for(m in 1:length(a2[1,])){
					names(aa2_multi_criterion_temp)[m]=names(a2[1,][[m]])
					names(aa2_Discrete_data_temp)[m]=names(a2[1,][[m]])
				}
				aa2_multi_criterion=cbind(aa2_multi_criterion,aa2_multi_criterion_temp)
				aa2_Discrete_data=cbind(aa2_Discrete_data,aa2_Discrete_data_temp)
				aa2_temp_matrix=rbind(aa2_temp_matrix,aa2_temp_matrix_temp)
			}
			aa2_multi_criterion=aa2_multi_criterion[colSums(!is.na(aa2_multi_criterion)) > 0]
			aa2_Discrete_data=aa2_Discrete_data[colSums(!is.na(aa2_Discrete_data)) > 0]
			
		}
	}
	if(length(msg)==1){
		aaa2_multi_criterion=aa2_multi_criterion
	}else{
		aaa2_multi_criterion=cbind(msg,aa2_multi_criterion)
	}
	
	rownames(aaa2_multi_criterion)[1]="IG"
	rownames(aaa2_multi_criterion)[2]="GR"
	rownames(aaa2_multi_criterion)[3]="COR"
	rownames(aaa2_multi_criterion)[4]="CHI"
	rownames(aaa2_multi_criterion)[5]="SU"
	rownames(aaa2_multi_criterion)[6]="interior_IG"
	rownames(aaa2_multi_criterion)[7]="interior_GR"
	rownames(aaa2_multi_criterion)[8]="interior_COR"
	rownames(aaa2_multi_criterion)[9]="interior_CHI"
	rownames(aaa2_multi_criterion)[10]="interior_SU"
	
	ans=list(s=aa2_temp_matrix,Discrete_data=aa2_Discrete_data,multi_criterion=aaa2_multi_criterion)
	return(ans)
}