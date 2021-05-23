#輸入 discrete_data 離散化資料；num 欲建置環境屬性數；layer；欲放置高相關屬性之層數(0=只有最高相關度的一個特徵；1＝第一層：曼哈頓距離為1的周圍特徵，以此類推) ；
#     class_label 目標分類屬性；position: 學習終點位置(請輸入:座標，EX:c(5,5,5,5,5,5))
#輸出 env 學習環境；experience 經驗表 ；fea_label用於學習的特徵標籤

RL_6D_Environment=function(discrete_data,num,layer,class_label,position){
 #-----------------------建立環境------------------------
 fea_IG=IG(class_label,discrete_data) #評估屬性
 IG_sorted=fea_IG[order(-fea_IG[2]),] #將其排序
 fea_label=IG_sorted[1:num,1]         #擷取出具有高相關度的前n名屬性
 length_data=ceiling(num^(1/6))       #計算建置環境所需的長度
 env=array(, dim = rep(length_data,6))  #建置空環境
 #------------設置相關度最高的一群屬性----------
 step_num=1
 for(kk in 0:layer){
 for(m in 1:length_data){
 for(p in 1:length_data){
 for(y in 1:length_data){
 for(k in 1:length_data){
 for(i in 1:length_data){
 for(j in 1:length_data){
 if(sum(abs(c(i,j,k,y,p,m)-position))==kk){
  env[i,j,k,y,p,m]=fea_label[step_num]
  step_num=step_num+1
 }
 }
 }
 }
 }
 }
 }
 }
 #------------將其餘的特徵亂數設置於環境----------
 random_fea=sample(fea_label[step_num:num])
 count=1
 for(m in 1:length_data){
 for(p in 1:length_data){
 for(y in 1:length_data){
 for(k in 1:length_data){
 for(i in 1:length_data){
 for(j in 1:length_data){
 if(is.na(env[i,j,k,y,p,m])){
  env[i,j,k,y,p,m]=random_fea[count]
  count=count+1
 }
 }
 }
 }
 }
 }
 }
 #----------------------形成經驗表-------------------------
 experience=matrix(,num*12,4)
 experience=as.data.frame(experience)
 colnames(experience)=c("State","Action","Reward","NextState")
 Actions=c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12")
 #-------------將多維環境資訊記載於二維經驗表中--------------
 
 for(m in 1:length_data){
 for(p in 1:length_data){
 for(y in 1:length_data){
 for(k in 1:length_data){
 for(j in 1:length_data){
 for(i in 1:length_data){
 if(! is.na(env[i,j,k,y,p,m])){
     position1=(nrow(as.data.frame(which(rowSums(!is.na(experience))>0)))+1)
     experience[position1:(position1+11),1]=env[i,j,k,y,p,m]
     experience[position1:(position1+11),2]=Actions
     experience[position1+0,4]=if(length(tryCatch({env[i-1,j,k,y,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])}))==0){env[i,j,k,y,p,m]}else{tryCatch({env[i-1,j,k,y,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])})}
     experience[position1+1,4]=if(length(tryCatch({env[i+1,j,k,y,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])}))==0){env[i,j,k,y,p,m]}else{tryCatch({env[i+1,j,k,y,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])})}
     experience[position1+2,4]=if(length(tryCatch({env[i,j-1,k,y,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])}))==0){env[i,j,k,y,p,m]}else{tryCatch({env[i,j-1,k,y,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])})}
     experience[position1+3,4]=if(length(tryCatch({env[i,j+1,k,y,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])}))==0){env[i,j,k,y,p,m]}else{tryCatch({env[i,j+1,k,y,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])})}
     experience[position1+4,4]=if(length(tryCatch({env[i,j,k-1,y,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])}))==0){env[i,j,k,y,p,m]}else{tryCatch({env[i,j,k-1,y,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])})}
     experience[position1+5,4]=if(length(tryCatch({env[i,j,k+1,y,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])}))==0){env[i,j,k,y,p,m]}else{tryCatch({env[i,j,k+1,y,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])})}
     experience[position1+6,4]=if(length(tryCatch({env[i,j,k,y-1,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])}))==0){env[i,j,k,y,p,m]}else{tryCatch({env[i,j,k,y-1,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])})}
     experience[position1+7,4]=if(length(tryCatch({env[i,j,k,y+1,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])}))==0){env[i,j,k,y,p,m]}else{tryCatch({env[i,j,k,y+1,p,m]}, error = function(e) {return(env[i,j,k,y,p,m])})}
     experience[position1+8,4]=if(length(tryCatch({env[i,j,k,y,p-1,m]}, error = function(e) {return(env[i,j,k,y,p,m])}))==0){env[i,j,k,y,p,m]}else{tryCatch({env[i,j,k,y,p-1,m]}, error = function(e) {return(env[i,j,k,y,p,m])})}
     experience[position1+9,4]=if(length(tryCatch({env[i,j,k,y,p+1,m]}, error = function(e) {return(env[i,j,k,y,p,m])}))==0){env[i,j,k,y,p,m]}else{tryCatch({env[i,j,k,y,p+1,m]}, error = function(e) {return(env[i,j,k,y,p,m])})}
     experience[position1+10,4]=if(length(tryCatch({env[i,j,k,y,p,m-1]}, error = function(e) {return(env[i,j,k,y,p,m])}))==0){env[i,j,k,y,p,m]}else{tryCatch({env[i,j,k,y,p,m-1]}, error = function(e) {return(env[i,j,k,y,p,m])})}
     experience[position1+11,4]=if(length(tryCatch({env[i,j,k,y,p,m+1]}, error = function(e) {return(env[i,j,k,y,p,m])}))==0){env[i,j,k,y,p,m]}else{tryCatch({env[i,j,k,y,p,m+1]}, error = function(e) {return(env[i,j,k,y,p,m])})}
 }
 }
 }
 }
 }
 }
 }
 experience[,3]=parSapply(cl,1:nrow(experience),function(k,fea_IG,discrete_data,experience,IG_sorted){
	if(experience[k,1]==experience[k,4]){
		b=-10 #撞牆的給懲罰值-10
	}else if(experience[k,4]==IG_sorted[1,1]){
		b=1000
	}else{
		#計算屬性聯合效用:IG/MI
		b=as.numeric(fea_IG[which(fea_IG==experience[k,4]),2]/MI(discrete_data[which(names(discrete_data)==experience[k,1])],discrete_data[which(names(discrete_data)==experience[k,4])]))
	}
	return(b)
 },fea_IG,discrete_data,experience,IG_sorted)
 return(list(env=env,experience=experience,fea_label=fea_label))
}
