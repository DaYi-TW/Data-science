FCM_C_AutoSelect=function(t1,m,seed=1){

	k1=2
	x=t1
	for(i in ncol(x)){
		x[i]=as.numeric(x[,i])
	}
	repeat{
		k2=k1+1
		if(k1==2){
			Jm1=Jm(x,k1,m,seed)
		}else{
			Jm1=Jm2
		}
		Jm2=Jm(x,k2,m,seed)
		DK1=DK_Process(Jm1)
		DK2=DK_Process(Jm2)
		Vpbmf1=Vpbmf(Jm1,k1,DK1)
		Vpbmf2=Vpbmf(Jm2,k2,DK2)
		if(Vpbmf1>Vpbmf2){
			result=list(cluster=as.data.frame(Jm1$res$cluster),v=as.data.frame(Jm1$res$v))
			return(result)
			break;
		}
		k1=k1+1
	}

}
