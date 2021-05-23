cutup=function(data_A,G1){ 
#n=ceiling(sqrt(ncol(data_A)-1))
n=ncol(data_A)-1
res=as.data.frame(G1$result)
temp=as.data.frame(G1$M_data)

for(i in 1 :nrow(res)){
	if(i==1){
		a=colnames(data_A)[i]
	}else{
		a=rbind(a,colnames(data_A)[i])
	}
}
res=cbind(res,a)
res=res[order(-res[1]),]
for(b in 1:n){
 if(b==1){
	B=temp[(which(colnames(temp)==res[b,2]))]
	C=data_A[(which(colnames(data_A)==res[b,2]))]
 }else{
	B=cbind(B,temp[(which(colnames(temp)==res[b,2]))])
	C=cbind(C,data_A[(which(colnames(data_A)==res[b,2]))])
 }
}
ans=list(C=C,B=B)
return(ans)
}

