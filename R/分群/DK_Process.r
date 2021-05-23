DK_Process=function(Jm){
	ste=1
	
	for(i in 1:nrow(Jm$res$v)-1){
		for(j in (i+1):nrow(Jm$res$v)){
			if(ste==1){
				max_value=colSums(abs(as.data.frame(Jm$res$v[i,]-Jm$res$v[j,])))
			}else{
				max_value=cbind(max_value,colSums(abs(as.data.frame(Jm$res$v[i,]-Jm$res$v[j,]))))
			}
		}
	ste=ste+1
	}
	
	return(max(as.data.frame(max_value)))
}