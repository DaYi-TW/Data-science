entropy=function(fea){
    #計算input feature or target class label 的原始亂度
	fea=as.data.frame(fea)
	info=abs(sum((table(fea)/nrow(fea))*log(table(fea)/nrow(fea),2)))
    return(info)
}
