classANA_method_v5=function(data_orginal,filename,folder,methodname,num,startnum,loop){ 

max_value_J48=as.data.frame(matrix(,100,3))
max_value_SMO=as.data.frame(matrix(,100,3))
max_value_LMT=as.data.frame(matrix(,100,3))
max_value_NB=as.data.frame(matrix(,100,3))
max_value_kNN=as.data.frame(matrix(,100,3))

max_value_J48_roc=as.data.frame(matrix(,100,3))
max_value_SMO_roc=as.data.frame(matrix(,100,3))
max_value_LMT_roc=as.data.frame(matrix(,100,3))
max_value_NB_roc=as.data.frame(matrix(,100,3))
max_value_kNN_roc=as.data.frame(matrix(,100,3))

data_A=as.data.frame(read.csv(file=paste0("C:/R/Result/",filename,"_Discrete_data",".csv")))
data_A=data_A[-1]
data_B=data_orginal
test_data=data_B
for(times in startnum:loop){
	result_D=as.data.frame(read.xlsx(file=paste0("C:/R/Result/",folder,"/",filename,"_",methodname,"_",num,".xlsx"),times))
	
	print(paste0("step",times))
	max_value=as.data.frame(matrix(,100,3))
	
	result_D=result_D[-1]
	
	for(i in 1:ncol(result_D)){result_D[,i]=as.character(result_D[,i])}
	
	#result_D=result_D[1:num,]
	
	temp_data=matrix(,nrow(data_A),5)
	M_data=matrix(,nrow(data_A),ncol(data_A))
	
	s=matrix(,nrow(result_D),ncol(result_D))
	a=matrix(,nrow(result_D),10)
	aa=matrix(,0,10)
	

	if(times==1){
		temp=as.data.frame(data_A[which(colnames(data_A)==result_D[1,2])])
		s=temp
	}else{
		temp=as.data.frame((data_A[which(colnames(data_A)==result_D[1,2])]))
		if(ncol(result_D)>2){
			for(i in 3:ncol(result_D)){
				temp=cbind(temp,as.data.frame(data_A[which(colnames(data_A)==result_D[1,i])]))
			}
		}
		s=as.data.frame(temp)
	}
	for(j in 1:ncol(s)){
		s[,j]=as.factor(s[,j])
	}
	a=classANA(as.data.frame(cbind(s,as.data.frame(as.factor(test_data[,ncol(test_data)])))))
	aa=rbind(aa,cbind(a$J48_ac,a$J48_roc,a$SMO_ac,a$SMO_roc,a$LMT_ac,a$LMT_roc,a$NB_ac,a$NB_roc,a$kNN_ac,a$kNN_roc))
	
	if(nrow(result_D)>1){
		for(timeaa in 2:nrow(result_D)){
			temp=as.data.frame(data_A[which(colnames(data_A)==result_D[timeaa,2])])
			
			if(ncol(result_D)>2){
				for(i in 3:ncol(result_D)){
					temp=cbind(temp,as.data.frame(data_A[which(colnames(data_A)==result_D[timeaa,i])]))
				}
			}
			s=as.data.frame(temp)
			for(j in 1:ncol(s)){
				s[,j]=as.factor(s[,j])
			}
			a=classANA(as.data.frame(cbind(s,as.data.frame(as.factor(test_data[,ncol(test_data)])))))
			aa=rbind(aa,cbind(a$J48_ac,a$J48_roc,a$SMO_ac,a$SMO_roc,a$LMT_ac,a$LMT_roc,a$NB_ac,a$NB_roc,a$kNN_ac,a$kNN_roc))
			if(timeaa==10){
				aa10=aa
			}
		}
	}
	for(i in 1:nrow(aa)){rownames(aa)[i]=i}
	aa=as.data.frame(aa)
	colnames(aa)[1]="$J48_ac";
	colnames(aa)[2]="$J48_roc";
	colnames(aa)[3]="$SMO_ac";
	colnames(aa)[4]="$SMO_roc";
	colnames(aa)[5]="$LMT_ac";
	colnames(aa)[6]="$LMT_roc";
	colnames(aa)[7]="$NB_ac";
	colnames(aa)[8]="$NB_roc";
	colnames(aa)[9]="$kNN_ac";
	colnames(aa)[10]="$kNN_roc";
	
	result=cbind(result_D,aa);
	result=as.data.frame(result);
	

	
	J48_ac=result[order(-result[which(colnames(result)=="$J48_ac")]), ]
	SMO_ac=result[order(-result[which(colnames(result)=="$SMO_ac")]), ]
	LMT_ac=result[order(-result[which(colnames(result)=="$LMT_ac")]), ]
	NB_ac=result[order(-result[which(colnames(result)=="$NB_ac")]), ]
	kNN_ac=result[order(-result[which(colnames(result)=="$kNN_ac")]), ]

	
	result_J48_ac_max=J48_ac[1,which(colnames(result)=="$J48_ac")]
	J48_ac_avg10=colMeans(as.data.frame(J48_ac[1:10,which(colnames(result)=="$J48_ac")]))
	J48_ac_avg20=colMeans(as.data.frame(J48_ac[1:20,which(colnames(result)=="$J48_ac")]))
	
	max_value_J48[times,1]=result_J48_ac_max
	max_value_J48[times,2]=J48_ac_avg10
	max_value_J48[times,3]=J48_ac_avg20
	
	
	result_SMO_ac_max=SMO_ac[1,which(colnames(result)=="$SMO_ac")]
	SMO_ac_avg10=colMeans(as.data.frame(SMO_ac[1:10,which(colnames(result)=="$SMO_ac")]))
	SMO_ac_avg20=colMeans(as.data.frame(SMO_ac[1:20,which(colnames(result)=="$SMO_ac")]))

	max_value_SMO[times,1]=result_SMO_ac_max
	max_value_SMO[times,2]=SMO_ac_avg10
	max_value_SMO[times,3]=SMO_ac_avg20
	
	
	
	
	result_LMT_ac_max=LMT_ac[1,which(colnames(result)=="$LMT_ac")]
	LMT_ac_avg10=colMeans(as.data.frame(LMT_ac[1:10,which(colnames(result)=="$LMT_ac")]))
	LMT_ac_avg20=colMeans(as.data.frame(LMT_ac[1:20,which(colnames(result)=="$LMT_ac")]))
		
	max_value_LMT[times,1]=result_LMT_ac_max
	max_value_LMT[times,2]=LMT_ac_avg10
	max_value_LMT[times,3]=LMT_ac_avg20
	
	
	
	
	result_NB_ac_max=NB_ac[1,which(colnames(result)=="$NB_ac")]
	NB_ac_avg10=colMeans(as.data.frame(NB_ac[1:10,which(colnames(result)=="$NB_ac")]))
	NB_ac_avg20=colMeans(as.data.frame(NB_ac[1:20,which(colnames(result)=="$NB_ac")]))
			
	max_value_NB[times,1]=result_NB_ac_max
	max_value_NB[times,2]=NB_ac_avg10
	max_value_NB[times,3]=NB_ac_avg20
	

	
	
	result_kNN_ac_max=kNN_ac[1,which(colnames(result)=="$kNN_ac")]
	kNN_ac_avg10=colMeans(as.data.frame(kNN_ac[1:10,which(colnames(result)=="$kNN_ac")]))
	kNN_ac_avg20=colMeans(as.data.frame(kNN_ac[1:20,which(colnames(result)=="$kNN_ac")]))
			
	max_value_kNN[times,1]=result_kNN_ac_max
	max_value_kNN[times,2]=kNN_ac_avg10
	max_value_kNN[times,3]=kNN_ac_avg20
	
	
	J48_roc=result[order(-result[which(colnames(result)=="$J48_roc")]), ]
	SMO_roc=result[order(-result[which(colnames(result)=="$SMO_roc")]), ]
	LMT_roc=result[order(-result[which(colnames(result)=="$LMT_roc")]), ]
	NB_roc=result[order(-result[which(colnames(result)=="$NB_roc")]), ]
	kNN_roc=result[order(-result[which(colnames(result)=="$kNN_roc")]), ]
	
	
	result_J48_roc_max=J48_roc[1,which(colnames(result)=="$J48_roc")]
	J48_roc_avg10=colMeans(as.data.frame(J48_roc[1:10,which(colnames(result)=="$J48_roc")]))
	J48_roc_avg20=colMeans(as.data.frame(J48_roc[1:20,which(colnames(result)=="$J48_roc")]))
	
	max_value_J48_roc[times,1]=result_J48_roc_max
	max_value_J48_roc[times,2]=J48_roc_avg10
	max_value_J48_roc[times,3]=J48_roc_avg20
	
	
	result_SMO_roc_max=SMO_roc[1,which(colnames(result)=="$SMO_roc")]
	SMO_roc_avg10=colMeans(as.data.frame(SMO_roc[1:10,which(colnames(result)=="$SMO_roc")]))
	SMO_roc_avg20=colMeans(as.data.frame(SMO_roc[1:20,which(colnames(result)=="$SMO_roc")]))

	max_value_SMO_roc[times,1]=result_SMO_roc_max
	max_value_SMO_roc[times,2]=SMO_roc_avg10
	max_value_SMO_roc[times,3]=SMO_roc_avg20
	
	
	
	
	result_LMT_roc_max=LMT_roc[1,which(colnames(result)=="$LMT_roc")]
	LMT_roc_avg10=colMeans(as.data.frame(LMT_roc[1:10,which(colnames(result)=="$LMT_roc")]))
	LMT_roc_avg20=colMeans(as.data.frame(LMT_roc[1:20,which(colnames(result)=="$LMT_roc")]))
		
	max_value_LMT_roc[times,1]=result_LMT_roc_max
	max_value_LMT_roc[times,2]=LMT_roc_avg10
	max_value_LMT_roc[times,3]=LMT_roc_avg20
	
		
	result_NB_roc_max=NB_roc[1,which(colnames(result)=="$NB_roc")]
	NB_roc_avg10=colMeans(as.data.frame(NB_roc[1:10,which(colnames(result)=="$NB_roc")]))
	NB_roc_avg20=colMeans(as.data.frame(NB_roc[1:20,which(colnames(result)=="$NB_roc")]))
		
	max_value_NB_roc[times,1]=result_NB_roc_max
	max_value_NB_roc[times,2]=NB_roc_avg10
	max_value_NB_roc[times,3]=NB_roc_avg20
	
	
	
	result_kNN_roc_max=kNN_roc[1,which(colnames(result)=="$kNN_roc")]
	kNN_roc_avg10=colMeans(as.data.frame(kNN_roc[1:10,which(colnames(result)=="$kNN_roc")]))
	kNN_roc_avg20=colMeans(as.data.frame(kNN_roc[1:20,which(colnames(result)=="$kNN_roc")]))
			
	max_value_kNN_roc[times,1]=result_kNN_roc_max
	max_value_kNN_roc[times,2]=kNN_roc_avg10
	max_value_kNN_roc[times,3]=kNN_roc_avg20
	
	#write.csv(result,file=paste0("C:/R/Result/",filename,"_",methodname,"_",num,"_",times,".csv"))
	if(times==1){
	write.xlsx(result,file=paste0("C:/R/Result/",folder,"/",filename,"_",methodname,"_process_",num,".xlsx"),sheetName=paste0("round_",times))
	}else{
	write.xlsx(result,file=paste0("C:/R/Result/",folder,"/",filename,"_",methodname,"_process_",num,".xlsx"),sheetName=paste0("round_",times), append=TRUE)
	}
}
max_value_J48=as.data.frame(max_value_J48)
max_value_SMO=as.data.frame(max_value_SMO)
max_value_LMT=as.data.frame(max_value_LMT)
max_value_NB=as.data.frame(max_value_NB)
max_value_kNN=as.data.frame(max_value_kNN)

max_value_J48=max_value_J48[1:nrow(as.data.frame(which(rowSums(!is.na(max_value_J48))>0))),]
max_value_SMO=max_value_SMO[1:nrow(as.data.frame(which(rowSums(!is.na(max_value_SMO))>0))),]
max_value_LMT=max_value_LMT[1:nrow(as.data.frame(which(rowSums(!is.na(max_value_LMT))>0))),]
max_value_NB=max_value_NB[1:nrow(as.data.frame(which(rowSums(!is.na(max_value_NB))>0))),]
max_value_kNN=max_value_kNN[1:nrow(as.data.frame(which(rowSums(!is.na(max_value_kNN))>0))),]

max_value_J48_roc=max_value_J48_roc[1:nrow(as.data.frame(which(rowSums(!is.na(max_value_J48_roc))>0))),]
max_value_SMO_roc=max_value_SMO_roc[1:nrow(as.data.frame(which(rowSums(!is.na(max_value_SMO_roc))>0))),]
max_value_LMT_roc=max_value_LMT_roc[1:nrow(as.data.frame(which(rowSums(!is.na(max_value_LMT_roc))>0))),]
max_value_NB_roc=max_value_NB_roc[1:nrow(as.data.frame(which(rowSums(!is.na(max_value_NB_roc))>0))),]
max_value_kNN_roc=max_value_kNN_roc[1:nrow(as.data.frame(which(rowSums(!is.na(max_value_kNN_roc))>0))),]

colnames(max_value_J48)[1]="MAX"
colnames(max_value_J48)[2]="AVG10"
colnames(max_value_J48)[3]="AVG20"
colnames(max_value_SMO)[1]="MAX"
colnames(max_value_SMO)[2]="AVG10"
colnames(max_value_SMO)[3]="AVG20"
colnames(max_value_LMT)[1]="MAX"
colnames(max_value_LMT)[2]="AVG10"
colnames(max_value_LMT)[3]="AVG20"
colnames(max_value_NB)[1]="MAX"
colnames(max_value_NB)[2]="AVG10"
colnames(max_value_NB)[3]="AVG20"
colnames(max_value_kNN)[1]="MAX"
colnames(max_value_kNN)[2]="AVG10"
colnames(max_value_kNN)[3]="AVG20"


colnames(max_value_J48_roc)[1]="MAX"
colnames(max_value_J48_roc)[2]="AVG10"
colnames(max_value_J48_roc)[3]="AVG20"
colnames(max_value_SMO_roc)[1]="MAX"
colnames(max_value_SMO_roc)[2]="AVG10"
colnames(max_value_SMO_roc)[3]="AVG20"
colnames(max_value_LMT_roc)[1]="MAX"
colnames(max_value_LMT_roc)[2]="AVG10"
colnames(max_value_LMT_roc)[3]="AVG20"
colnames(max_value_NB_roc)[1]="MAX"
colnames(max_value_NB_roc)[2]="AVG10"
colnames(max_value_NB_roc)[3]="AVG20"
colnames(max_value_kNN_roc)[1]="MAX"
colnames(max_value_kNN_roc)[2]="AVG10"
colnames(max_value_kNN_roc)[3]="AVG20"
for(i in 1:loop){
	if(num==1){
		if(i==1){
			result_J48=max_value_J48[1,1]
			result_SMO=max_value_SMO[1,1]
			result_LMT=max_value_LMT[1,1]
			result_NB=max_value_NB[1,1]
			result_kNN=max_value_kNN[1,1]
					
			result_J48_roc=max_value_J48_roc[1,1]
			result_SMO_roc=max_value_SMO_roc[1,1]
			result_LMT_roc=max_value_LMT_roc[1,1]
			result_NB_roc=max_value_NB_roc[1,1]
			result_kNN_roc=max_value_kNN_roc[1,1]
		}else{
			result_J48=cbind(result_J48,max_value_J48[i,1])
			result_SMO=cbind(result_SMO,max_value_SMO[i,1])
			result_LMT=cbind(result_LMT,max_value_LMT[i,1])
			result_NB=cbind(result_NB,max_value_NB[i,1])
			result_kNN=cbind(result_kNN,max_value_kNN[i,1])
			
			result_J48_roc=cbind(result_J48_roc,max_value_J48_roc[i,1])
			result_SMO_roc=cbind(result_SMO_roc,max_value_SMO_roc[i,1])
			result_LMT_roc=cbind(result_LMT_roc,max_value_LMT_roc[i,1])
			result_NB_roc=cbind(result_NB_roc,max_value_NB_roc[i,1])
			result_kNN_roc=cbind(result_kNN_roc,max_value_kNN_roc[i,1])
		}
	}else{
		if(i==1){
		result_J48=max_value_J48[i,]
		result_SMO=max_value_SMO[i,]
		result_LMT=max_value_LMT[i,]
		result_NB=max_value_NB[i,]
		result_kNN=max_value_kNN[i,]
		         
		result_J48_roc=max_value_J48_roc[i,]
		result_SMO_roc=max_value_SMO_roc[i,]
		result_LMT_roc=max_value_LMT_roc[i,]
		result_NB_roc=max_value_NB_roc[i,]
		result_kNN_roc=max_value_kNN_roc[i,]
		}else{
		result_J48=cbind(result_J48,max_value_J48[i,])
		result_SMO=cbind(result_SMO,max_value_SMO[i,])
		result_LMT=cbind(result_LMT,max_value_LMT[i,])
		result_NB=cbind(result_NB,max_value_NB[i,])
		result_kNN=cbind(result_kNN,max_value_kNN[i,])
		
		result_J48_roc=cbind(result_J48_roc,max_value_J48_roc[i,])
		result_SMO_roc=cbind(result_SMO_roc,max_value_SMO_roc[i,])
		result_LMT_roc=cbind(result_LMT_roc,max_value_LMT_roc[i,])
		result_NB_roc=cbind(result_NB_roc,max_value_NB_roc[i,])
		result_kNN_roc=cbind(result_kNN_roc,max_value_kNN_roc[i,])
	}

}
}

#result_J48=as.data.frame(result_J48)
#result_SMO=as.data.frame(result_SMO)
#result_LMT=as.data.frame(result_LMT)
#result_NB=as.data.frame(result_NB)
#result_kNN=as.data.frame(result_kNN)
#
#result_J48_roc=as.data.frame(result_J48_roc)
#result_SMO_roc=as.data.frame(result_SMO_roc)
#result_LMT_roc=as.data.frame(result_LMT_roc)
#result_NB_roc=as.data.frame(result_NB_roc)
#result_kNN_roc=as.data.frame(result_kNN_roc)

ans_ac=as.data.frame(rbind(result_J48,result_SMO,result_LMT,result_NB,result_kNN))
ans_roc=as.data.frame(rbind(result_J48_roc,result_SMO_roc,result_LMT_roc,result_NB_roc,result_kNN_roc))


rownames(ans_ac)[1]="J48"
rownames(ans_ac)[2]="SMO"
rownames(ans_ac)[3]="LMT"
rownames(ans_ac)[4]="NB"
rownames(ans_ac)[5]="kNN"

rownames(ans_roc)[1]="J48"
rownames(ans_roc)[2]="SMO"
rownames(ans_roc)[3]="LMT"
rownames(ans_roc)[4]="NB"
rownames(ans_roc)[5]="kNN"
write.csv(ans_ac,file=paste0("C:/R/Result/",folder,"/",filename,"_ans_ac_",methodname,"_",num,".csv"))
write.csv(ans_roc,file=paste0("C:/R/Result/",folder,"/",filename,"_ans_roc_",methodname,"_",num,".csv"))

#excel_process(data_orginal,loop,filename,num,methodname)

ans=list(ans_ac=ans_ac,ans_roc=ans_roc)

return(ans)
}