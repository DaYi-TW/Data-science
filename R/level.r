level=function(initial,interior_cor,data_A,k_rate){ 
	#num=length(table(data_A[ncol(data_A)]))
	num=3
	area=(initial/3)
	if(num==2){
		b=area
	}else{
		for(i in 1:(num-1)){
			if(i==1){
				b=area*i
			}else{
				b=rbind(b,area*i)
			}
		}
	}
	c=as.data.frame(as.numeric(interior_cor)<b)
	colnames(c)="level"
	answer=(sum(c$level == 'FALSE', na.rm=TRUE)+1)
	if(answer==1){
		result=1
	}else if(answer==2){
		result=9999999999999
	}else if(answer==3){
		result=9999999999999
	}
	return(result)	
}