Jm=function(x,k,m_value,seed){
	set.seed(seed)
	# Initialize the prototype matrix using K-means++ algorithm
	v <- inaparc::kmpp(x, k=k)$v
	# Initialize the memberships degrees matrix 
	u <- inaparc::imembrand(nrow(x), k=k)$u
	
	res.fcm <- fcm(x, centers=v,memberships=u,m=m_value)
	Jm=0
	E=0
	for(i in 1:nrow(x)){
		for(j in 1:k){
			non=rowSums(abs(as.data.frame((x[i,]-res.fcm$v[j,]))))
			if(j==1){
				E=E+(res.fcm$u[i,j])*non
			}
			Jm=Jm+((res.fcm$u[i,j])^m_value)*non^2
		}
	}
	
	result=list(Jm=Jm,res=res.fcm,E=E)
	return(result)
}