#輸入 s_terminal:學習終點；learning_rate:學習比率；discount_factor:折扣因子；Q:Q值表；experience:經驗表
#輸出 Q:Q值表
learningEpisode_FS=function(s_terminal,learning_rate,discount_factor,Q,experience){
Qold=Q
x1=experience[1]
x2=experience[2]
	#普通運算
	#for(i in 1:nrow(experience)){
	#	response=experience[i,]
	#	if(response$State!=s_terminal){
	#		Q[response$State,response$Action]=Qold[response$State,response$Action]+learning_rate*(response$Reward+discount_factor*max(Qold[response$NextState,])-Qold[response$State,response$Action])
	#	}
	#}
	#Q[s_terminal,]=0
	#平行化運算
	Qv=parSapply(cl,1:nrow(experience),function(i,Qold,experience,s_terminal,learning_rate,discount_factor){
		response=experience[i,]
		if(response$State!=s_terminal){
			Qold[response$State,response$Action]+learning_rate*(response$Reward+discount_factor*max(Qold[response$NextState,])-Qold[response$State,response$Action])
		}else{
			0	
		}
	},Qold,experience,s_terminal,learning_rate,discount_factor)	
	Q=matrix(Qv,nrow= nrow(unique(x1)),ncol=nrow(unique(x2)),byrow = TRUE,dimnames=list(unique(x1[,1]),unique(x2[,1])))
	#Q[s_terminal,]=10000
return(Q)
}

	

