#輸入變數: class_label 分類類別； fea 待評估屬性
#輸出變數: eval_value 屬性使用SU的評估值，值越大重要度越高
SU=function(class_label,fea){ 
eval_value=as.data.frame(matrix(,ncol(fea),2))
colnames(eval_value)=c("feature","SU")
eval_value[,1]=colnames(fea)
eval_value[,2]=suppressWarnings(sapply(1:ncol(fea),FUN=function(i,fea,class_label){
	est=cbind(fea[i],class_label)
	est=as.data.frame(lapply(est,as.factor))
	colnames(est)[ncol(est)]='class'
	return(2*(InfoGainAttributeEval(class~.,data=est))/ (entropy(est[,1])+entropy(est[,2]) ) )
},fea=fea,class_label=class_label))
return(eval_value)	
}
