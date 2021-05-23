MFSS_framework=function(data_A,filename,folder,Discrete,loop,G1,num,beta_value=0,seed,option,criteria,selection){ 

#---------------------------------前置作業-------------------------------------------------
	dfList <- list()											
	List_time <-list()
	involved <-list()
	CIG <-list()
	involved_attributes <-list()
	temp_data=matrix(,nrow(data_A),5)
	M_data=matrix(,nrow(data_A),ncol(data_A))
	result_D=matrix(,,100)
	files=as.data.frame(matrix(,nrow(data_A),))
	Discrete_data=as.data.frame(matrix(,nrow(data_A),))
	M_data=as.data.frame(M_data)
	
#------------------------------------------------------------------------------------------
	
	
	
	
#--------------------------------處理G1.r產生出來的資料------------------------------------
	result_D=as.data.frame(G1$result)
	List_time[[1]]=G1$List_time
	M_data=G1$M_data
	rrs=cutup(data_A,G1)
	test_data=rrs$C
	
	for(i in 1 :nrow(result_D)){
		rownames(result_D)[i]=i
	}
	
	test_M_data=rrs$B
	

	
	#x=which(result_D==0)
	#n=as.data.frame(x)
	#if(length(x)!=0){
 	#for(i in 1:nrow(n)){
	#	x=which(result_D==0)
	#	n=as.data.frame(x)
	#	test_data=test_data[-n[1,]]
	#	test_M_data=test_M_data[-n[1,]]
	#	result_D=as.data.frame(result_D[-n[1,],])
	#}
	#}
	colnames(result_D)[1]="result_D"
	v2=colnames(data_A[1])                                      #把欄位名稱標上
	if(nrow(result_D)>1){                                       #把欄位名稱標上
		for(i in 2:nrow(result_D)){                             #把欄位名稱標上
			v2=rbind(v2,colnames(data_A[i]))                    #把欄位名稱標上
		}                                                       #把欄位名稱標上
	}else{                                                      #把欄位名稱標上
		v2=colnames(data_A[1])                                  #把欄位名稱標上
	}                                                           #把欄位名稱標上
	result_D=cbind(result_D,v2)                                 #把欄位名稱標上
	colnames(result_D)[1]="v1"									#把欄位名稱標上
	result_D=result_D[order(-result_D[1]), ]					#由大到小排序
		
	max_value=as.data.frame(matrix(,100,3))
	n=nrow(as.data.frame(which(rowSums(!is.na(max_value))>0)))+1
	max_value[n,1]=result_D[1,1]
	max_value[n,2]=colMeans(as.data.frame(result_D[1:10,1]))
	max_value[n,3]=colMeans(as.data.frame(result_D[1:20,1]))
	
	write.xlsx(result_D,file=paste0("C:/R/Result/",folder,"/",filename,"_",option,"_result_",num,".xlsx"),sheetName=paste0("round_",1), append=TRUE)
	evalresult=result_D
	result_D=result_D[1:num,]
	
	result_D=result_D[1:nrow(as.data.frame(which(rowSums(!is.na(result_D))>0))),]
	#if(option!="IG"){
	#	CIG=calculate_IG(result_D,test_data,1,option,beta_value,Discrete,method,test_M_data)	
	#	write.xlsx(CIG,file=paste0("Result/",filename,"_",option,"_calculate_IG.xlsx"),sheetName=paste0("calculate_IG",1), append=TRUE)
	#}	
	
	
	
	
	dfList[[1]]=result_D
	quantity=ncol(data_A)-1
	involved[[1]]=nrow(result_D)
	involved_attributes[[1]]=table(as.matrix(result_D[2:ncol(result_D)]))
	write.xlsx(result_D,file=paste0("C:/R/Result/",folder,"/",filename,"_",option,"_",num,".xlsx"),sheetName=paste0("round_",1))
	
	write.csv(test_M_data,file=paste0("C:/R/Result/",folder,"/",filename,"_",option,"_",num,"_Discrete_value_G",1,".csv"))
	write.csv(test_data,file=paste0("C:/R/Result/",folder,"/",filename,"_",option,"_test_data_",num,".csv"))
	print(paste0("G1_",nrow(result_D),"_MAX_",result_D[1,1],"_Average_",colMeans(result_D[1])))
	
#-------------------------------------------------------------------------------------	
	

	
	
	
	
	
#--------------------------------進行配對---------------------------------------------
if(loop>1){
	for(times in 2:(loop)){
		print(paste0("----------G",times,"----------"))
		#print(result_D)
		#if(sum((result_D[ncol(result_D)]==colnames(test_data)[ncol(test_data)])=="TRUE")!=nrow(result_D)){

		ptm <- proc.time()									  #前置作業
		s=matrix(,ncol(M_data)*nrow(result_D),(times+1))      #前置作業
		s=as.data.frame(s)       
		d=test_data                                           #前置作業
		a=d                                                   #前置作業                                                
		if(ncol(d) != 0){
			without_interior=selection(data_A,folder,result_D,test_data,s,Discrete,G1,criteria,option,test_M_data,beta_value,seed,evalresult)
			s=without_interior$s
			Discrete_data=without_interior$Discrete_data
		}
				
		List_time[[times]]=proc.time() - ptm
		
		
		if(nrow(as.data.frame(which((!is.na(s[ncol(s)]))>0)))==0){
			print(paste0("stop",(times)))
			break
		}else{
							#進行S表的整理
			s[ncol(s)]=as.numeric(as.character(s[,ncol(s)]))
			s=s[order(-s[ncol(s)]),]												#S表依照評估法所產生的數值，由大到小排序
			s=s[1:nrow(as.data.frame(which((!is.na(s[ncol(s)]))>0))),]
			s=s[colSums(!is.na(s)) > 0]
			s=s[1:nrow(as.data.frame(which(rowSums(!is.na(s))>0))),]
			quantity=rbind(quantity,nrow(s))
			result_D=s[1:(times+1)]												#將S表的資料記錄在result_D變數裡，以供之後所用
			
			x=result_D[,ncol(result_D)]												#將最後一欄評估法所產生的評估值放到第一欄去
			for(i in (ncol(result_D)):2){                                           #將最後一欄評估法所產生的評估值放到第一欄去
				result_D[i]=result_D[i-1]                                           #將最後一欄評估法所產生的評估值放到第一欄去
			}                                                                       #將最後一欄評估法所產生的評估值放到第一欄去
			result_D[1]=x                                                           #將最後一欄評估法所產生的評估值放到第一欄去
			
			result_D=result_D[order(-result_D[1]), ]
			
			
			
			
			
			
			
			
			n=nrow(as.data.frame(which(rowSums(!is.na(max_value))>0)))+1
			max_value[n,1]=result_D[1,1]
			s1=0
			for(i in 1:10){
				s1=s1+result_D[i,1]
			}
			a1=s1/10
			s2=0
			for(i in 1:20){
				s2=s2+result_D[i,1]
			}
			a2=s2/20
			max_value[n,2]=a1
			max_value[n,3]=a2
			
			
			
			
			result_D=result_D[1:num,]												#看要留幾個當成種子使用
			
			
			
			
			
			#percent= 1
			
			#result_D=result_D[1: ceiling(((nrow(dfList[[times]])*percent))),]
			
			result_D=result_D[1:nrow(as.data.frame(which(rowSums(!is.na(result_D))>0))),]
			#if(option!="IG"){
			#CIG=calculate_IG(result_D,test_data,times,option,beta_value,Discrete,method,Discrete_data)
			#write.xlsx(CIG,file=paste0("Result/",filename,"_",option,"_calculate_IG.xlsx"),sheetName=paste0("calculate_IG",times+1), append=TRUE)
			#}

			

			
			involved[[times]]=length(table(as.matrix(result_D[2:ncol(result_D)])))
			involved_attributes[[times]]=table(as.matrix(result_D[2:ncol(result_D)]))
			dfList[[times]]=result_D
			if(is.na(Discrete_data)[1]=="FALSE"){
				write.csv(Discrete_data,file=paste0("C:/R/Result/",folder,"/",filename,"_s_",option,"_",num,"_",times,".csv"))
			}
			write.xlsx(s,file=paste0("C:/R/Result/",folder,"/",filename,"_",option,"_result_",num,".xlsx"),sheetName=paste0("round_",times), append=TRUE)
			write.xlsx(result_D,file=paste0("C:/R/Result/",folder,"/",filename,"_",option,"_",num,".xlsx"),sheetName=paste0("round_",times), append=TRUE)
			
			}
		#}
				
	}
}


cost=data.frame(matrix(unlist(List_time),nrow=length(List_time),byrow=T),stringsAsFactors=FALSE)
cost=cost[colSums(!is.na(cost)) > 0]
colnames(cost)[1]="user"
colnames(cost)[2]="system"   
colnames(cost)[3]="elapsed"  
write.csv(cost,file=paste0("C:/R/Result/",folder,"/",filename,"_",option,"_timeComplexity_",num,".csv"))
max_value=max_value[1:nrow(as.data.frame(which(rowSums(!is.na(max_value))>0))),]
colnames(max_value)[1]="MAX"
colnames(max_value)[2]="AVG10"
colnames(max_value)[3]="AVG20"
for(i in 1:nrow(max_value)){
	if(i==1){
		final_IG=max_value[i,]
	}else{
		final_IG=cbind(final_IG,max_value[i,])
	}
}
quantity=as.data.frame(quantity)
test_data=cbind(test_data,data_A[ncol(data_A)])
write.csv(test_data,file=paste0("C:/R/Result/",folder,"/",filename,"_test_data_",option,"_",num,".csv"))
write.csv(final_IG,file=paste0("C:/R/Result/",folder,"/",filename,"_IG_",option,"_",num,".csv"))

answer=as.data.frame(matrix(,,4))
	for(p in 1:length(dfList))
	{
		answer[p,1]=dfList[[p]][1,1]
		answer[p,2]=List_time[[p]][3]
		answer[p,3]=quantity[p,1]
		answer[p,4]=involved[[p]]
	}
colnames(answer)[1]="IG_MAX"
colnames(answer)[2]="elapsed"
colnames(answer)[3]="quantity"
colnames(answer)[4]="involved"
final=list(test_M_data=test_M_data,final_IG=final_IG,answer=answer)
write.csv(answer,file=paste0("C:/R/Result/",folder,"/",filename,"_",option,"_",num,"_INDEX_time",".csv"))

print(answer)
return(final)
}
