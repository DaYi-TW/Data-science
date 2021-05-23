#輸入變數:class_lab:類別屬性,fea:欲評估屬性
#輸出變數:eval_value:屬性的IG值
IG=function(class_label,fea){ 
fea=as.data.frame(fea)
eval_value=as.data.frame(matrix(,ncol(fea),2))
colnames(eval_value)=c("feature","IG")
eval_value[,1]=colnames(fea)
eval_value[,2]=sapply(1:ncol(fea),FUN=function(i,fea,class_label){
	eval=cbind(fea[i],class_label)
	eval=as.data.frame(lapply(eval,as.factor))
	colnames(eval)[ncol(eval)]='class'
	return(InfoGainAttributeEval(class ~ . , data = eval))
},fea=fea,class_label=class_label)
return(eval_value)	
}
