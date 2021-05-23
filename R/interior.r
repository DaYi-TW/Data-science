interior=function(t1,test_M_data,criteria){ 
d=as.data.frame(t1)
Fi=d[ncol(d)]
Fs=d[-ncol(d)]
correlation=0
de1=test_M_data[which(colnames(test_M_data)==colnames(Fi))]
for(i in 1 :ncol(Fs)){
	
	de2=test_M_data[which(colnames(test_M_data)==colnames(Fs[i]))]
	correlation=correlation+as.data.frame(criteria(de1,de2))
	
	
	#if(decision=="COR"){
	#	correlation=correlation+as.data.frame(cor(de1,de2))
	#}else if(decision=="CHI"){
	#	a=as.data.frame(cbind(de1,de2))
	#	Xsq=chisq.test(t(table(a)))
	#	correlation=correlation+as.data.frame(Xsq[1])
	#}else if(decision=="IG"){
	#	a=cbind(de1,de2)
    #
	#	for(j in 1:ncol(a)){
	#		a[j]=as.factor(a[,j])
	#	}
	#	colnames(a)[2]="class"
	#	correlation=correlation+as.data.frame(InfoGainAttributeEval(class~.,data=a))
	#}else if(decision=="GR"){
	#	a=cbind(de1,de2)
    #
	#	for(j in 1:ncol(a)){
	#		a[j]=as.factor(a[,j])
	#	}
	#	colnames(a)[2]="class"
	#	correlation=correlation+as.data.frame(GainRatioAttributeEval(class~.,data=a))
	#}
}
answer=correlation/ncol(Fs)
colnames(answer)[1]="correlation"
rownames(answer)[1]="value"
return(answer)
}