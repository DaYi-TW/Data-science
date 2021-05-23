#輸入: fea1 測試屬性1(可以是已挑選屬性組S)；fea2 測試屬性2 (可以是候選屬性alpha)
#輸出: fea1與fea2之間的MI值，值越高冗餘越高
MI=function(fea1,fea2){
ans=as.data.frame(sapply(1:ncol(fea1),FUN=function(i,fea1,fea2){
	eval=as.data.frame(cbind(fea1[i],fea2))
	eval=as.data.frame(lapply(eval,as.factor))
	colnames(eval)[ncol(eval)]='class'			
	return(InfoGainAttributeEval(class ~ . , data = eval))
},fea1=fea1,fea2=fea2))
colnames(ans)=colnames(fea2)
return(ans)
}
