#輸入 discrete_data 離散化資料；num 欲建置環境屬性數；layer；欲放置高相關屬性之層數(0=只有最高相關度的一個特徵；1＝第一層：曼哈頓距離為1的周圍特徵，以此類推) ；
#     class_label 目標分類屬性；position: 學習終點位置(請輸入:座標，EX:c(5,5))
#輸出 env 學習環境；experience 經驗表 ；fea_label用於學習的特徵標籤
RL_2D_Environment=function(discrete_data,num,layer,class_label,position){
 #-----------------------建立環境------------------------
 fea_IG=IG(class_label,discrete_data)	#評估屬性
 IG_sorted=fea_IG[order(-fea_IG[2]),]	#將其排序
 fea_label=IG_sorted[1:num,1]       	#擷取出具有高相關度的前n名屬性
 length_data=ceiling(num^(1/2))      	#計算建置環境所需的長度
 env=array(, dim = rep(length_data,2))  #建置空環境
 #------------設置相關度最高的一群屬性----------

 step_num=1
 for(kk in 0:layer){
 for(j in 1:length_data){
 for(i in 1:length_data){
 	if(sum(abs(c(i,j)-position))==kk){
 		env[i,j]=fea_label[step_num]
 		step_num=step_num+1	
 	}
 }
 }
 }
 
 #------------將其餘的特徵亂數設置於環境----------
 random_fea=sample(fea_label[step_num:num])
 count=1
 for(j in 1:length_data){
 for(i in 1:length_data){
	if(is.na(env[i,j])){
		env[i,j]=random_fea[count]
		count=count+1
	}
 }
 }
 #----------------------形成經驗表-------------------------
 experience=matrix(,num*4,4)
 experience=as.data.frame(experience)
 colnames(experience)=c("State","Action","Reward","NextState")
 Actions=c("a1","a2","a3","a4")
 #-------------將多維環境資訊記載於二維經驗表中--------------
 for(j in 1:dim(env)[2]){
 for(i in 1:dim(env)[1]){
	if(! is.na(env[i,j])){
		position1=(nrow(as.data.frame(which(rowSums(!is.na(experience))>0)))+1)
		experience[position1:(position1+3),1]=env[i,j]
		experience[position1:(position1+3),2]=Actions
		experience[position1+0,4]=if(length(tryCatch({env[i-1,j]}, error = function(e) {return(env[i,j])}))==0){env[i,j]}else{tryCatch({env[i-1,j]}, error = function(e) {return(env[i,j])})}
		experience[position1+1,4]=if(length(tryCatch({env[i+1,j]}, error = function(e) {return(env[i,j])}))==0){env[i,j]}else{tryCatch({env[i+1,j]}, error = function(e) {return(env[i,j])})}
		experience[position1+2,4]=if(length(tryCatch({env[i,j-1]}, error = function(e) {return(env[i,j])}))==0){env[i,j]}else{tryCatch({env[i,j-1]}, error = function(e) {return(env[i,j])})}
		experience[position1+3,4]=if(length(tryCatch({env[i,j+1]}, error = function(e) {return(env[i,j])}))==0){env[i,j]}else{tryCatch({env[i,j+1]}, error = function(e) {return(env[i,j])})}
	}
 }
 }
 experience[,3]=parSapply(cl,1:nrow(experience),function(k,fea_IG,discrete_data,experience,IG_sorted){
	if(experience[k,1]==experience[k,4]){
		b=-10 #撞牆的給懲罰值-10
	}else if(experience[k,4]==IG_sorted[1,1]){
		b=10000 #探索到終點 給獎勵值10000
	}else{
		#計算屬性聯合效用:IG/MI
		b=as.numeric(fea_IG[which(fea_IG==experience[k,4]),2]/MI(discrete_data[which(names(discrete_data)==experience[k,1])],discrete_data[which(names(discrete_data)==experience[k,4])]))
	}
	return(b)
 },fea_IG,discrete_data,experience,IG_sorted)
 return(list(env=env,experience=experience,fea_label=fea_label))
}
