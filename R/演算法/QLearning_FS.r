#輸入: s_terminal:學習終點；learning_rate:學習比率；discount_factor:折扣因子；Q:Q值表；exp:經驗表；threshold：收斂的嚴謹程度(0~1之間)
#輸出: Q:收斂後的Q值表；iter:學習迭代數
Qlearning_FS=function(s_0,s_terminal,learning_rate,discount_factor,exp,threshold){
	states =unique(exp[,1])
	Actions=unique(exp[,2])
	Q=matrix(0,nrow=length(states),ncol=length(Actions),dimnames=list(states,Actions))
	convergence=matrix(c(-1,0),2,1,dimnames=list(c("previous","now"),c("value")))#判別有無收斂
	iter=0
	if(threshold==1){
	while((convergence[1,1]!=convergence[2,1])){
		iter=iter+1
		Q=learningEpisode_FS(s_terminal,learning_rate,discount_factor,Q,exp)
		if(iter==1){
			convergence[1,1]=convergence[2,1]
			convergence[2,1]=Q[s_0,2]
		}else{
			convergence[1,1]=convergence[2,1]
			convergence[2,1]=Q[s_0,2]
		}
	}
	}else{
	while((convergence[1,1]/convergence[2,1])<threshold){
		iter=iter+1
		Q=learningEpisode_FS(s_terminal,learning_rate,discount_factor,Q,exp)
		if(iter==1){
			convergence[1,1]=convergence[2,1]
			convergence[2,1]=Q[s_0,2]
		}else{
			convergence[1,1]=convergence[2,1]
			convergence[2,1]=Q[s_0,2]
		}
	}
	}
	return(list(Q=Q,iter=iter))
}


