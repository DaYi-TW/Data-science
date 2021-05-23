RL_2D_8_action_Environment=function(filename,folder,denum,layer,block,data_A,temp_grid=NULL,c4=NULL,c1=NULL,raw_data_De=NULL,stee,selection){
 as=as.data.frame(read.csv(file=paste0("Result/",folder,"/",filename,"_Discrete_data.csv"),1,stringsAsFactors=FALSE))
 raw_data=as[-1]
 raw_data_De=SF.asDecisionTable(cbind(raw_data,data_A[ncol(data_A)]))
 if(colnames(data_A)[ncol(data_A)]!="class"){
 data_A=transpose_dataset(data_A)
 }
 if(is.null(temp_grid)&&is.null(c4)){

 a=IG(data_A,raw_data)
 a[2]=rownames(a)
 a1=a
 cc=a[order(-a[1]),]
 ccc=cc[,2]

 #-----------------------------------------------
 length_data=ceiling(denum^(1/2))
 temp_grid=array(, dim = rep(length_data,2))

 step_num=0


 for(i in length_data:1){
 for(j in length_data:1){
 if(sum(sum(j,i)==seq(sum(dim(temp_grid)),by=-1,length=layer))>0){
 	step_num=step_num+1
	temp_grid[j,i]=ccc[step_num]


 }
 }
 }


  print(step_num)

 n=step_num
 print((denum-n))
 c0=cc[1:n,2]
 #-----------------------------------------------
 for(i in 1:length(c0)){
 if(i ==1){
 c1=which(colnames(raw_data)==c0[i])
 }else{
 c1=c(c1,which(colnames(raw_data)==c0[i]))
 }
 }
 c2=a1[-c1,]
 if(selection=="FRST"){
 #--------------------------FRST--------------------------
 independent=0
 while(independent!=1){
 c3=sample(c(c2[,2]),(denum-n))
 for(j in 1:length(c3)){
 if(j==1){
 c4=which(colnames(raw_data)==c3[j])
 }else{
 c4=c(c4,which(colnames(raw_data)==c3[j]))
 }
 }
 c5=c(c4,c1)
 independent=FRST(raw_data_De,c5)
 }
 #----------------------------------------------------
 }else if(selection=="IG"){
 #---------------------------IG-------------------------
 for(p in (n+1):denum){
 if(p ==(n+1)){
 c4=which(colnames(raw_data)==cc[p,2])
 }else{
 c4=c(c4,which(colnames(raw_data)==cc[p,2]))
 }
 }
 c4=sample(c4,denum-n)
 c5=c(c4,c1)
 }
 #-----------------------------------------------------
 c4_n=colnames(data_A)[c4]

 ste1=1
 c1_n=colnames(data_A)[c1]


 for(i in 1:length_data){
 for(j in 1:length_data){
 if(is.na(temp_grid[j,i])){
	temp_grid[j,i]=c4_n[ste1]
	ste1=ste1+1

 }
 }
 }




 }else if((is.null(temp_grid)&&!is.null(c4))||(!is.null(temp_grid)&&!is.null(c4))){

 c4=sample(c4,length(c4))
 c5=c(c4,c1)
 c4_n=colnames(data_A)[c4]
 temp_grid=as.data.frame(matrix(,sqrt(denum),sqrt(denum)),stringsAsFactors=FALSE)
 ste=1
 step_num=1
 h=ff(denum,n,temp_grid)
 for(i in 1:ceiling(sqrt(length(c4)))){
 for(j in 1:ceiling(sqrt(length(c4)))){
 if(sum(ste==h)==0){
 temp_grid[j,i]=c4_n[step_num]
 step_num=step_num+1
 }
 ste=ste+1
 }
 }
 ste1=1
 c1_n=colnames(data_A)[c1]
 for(i in ncol(temp_grid):(ncol(temp_grid)-(floor(sqrt(n)))+1)){
 for(j in nrow(temp_grid):(nrow(temp_grid)-(ceiling(sqrt(n)))+1)){
 temp_grid[j,i]=c1_n[ste1]
 ste1=ste1+1
 }
 }
 }else if(!is.null(temp_grid)){
 ta=as.data.frame(read.xlsx(file=paste0("Result/",folder,"/",filename,"_RL_temp_grid.xlsx"),stee,stringsAsFactors=FALSE))[-1]

 temp_grid=array(unmatrix(t(ta),TRUE),rep(nrow(ta),2))
 pre=unmatrix_function(data_A,temp_grid,,2)
 c1=pre$c1
 c1_n=colnames(data_A)[c1]
 c4=pre$c4
 c5=c(c4,c1)
 }


 name=colnames(data_A)[c5]

 temp_GridSequence=matrix(,length(name)*8,5)
 temp_GridSequence=as.data.frame(temp_GridSequence)
 names(temp_GridSequence)[1]="State"
 names(temp_GridSequence)[2]="Action"
 names(temp_GridSequence)[3]="Reward"
 names(temp_GridSequence)[4]="NextState"
 names(temp_GridSequence)[5]="Block"
 Actions=c("a1","a2","a3","a4","a5","a6","a7","a8")



 for(i in 1:dim(temp_grid)[2]){
 for(j in 1:dim(temp_grid)[1]){
 if(! is.na(temp_grid[i,j])){
 position1=(nrow(as.data.frame(which(rowSums(!is.na(temp_GridSequence))>0)))+1)
 temp_GridSequence[position1:(position1+7),1]=temp_grid[i,j]
 temp_GridSequence[position1:(position1+7),2]=Actions
 temp_GridSequence[position1+0,4]=if(length(tryCatch({temp_grid[i-1,j]}, error = function(e) {return(temp_grid[i,j])}))==0){temp_grid[i,j]}else if(tryCatch({temp_grid[i-1,j]}, error = function(e) {return("0")})==0){temp_grid[i,j]}else if(is.na(temp_grid[i-1,j])){temp_grid[i,j]}else{tryCatch({temp_grid[i-1,j]}, error = function(e) {return(temp_grid[i,j])})}
 temp_GridSequence[position1+1,4]=if(length(tryCatch({temp_grid[i+1,j]}, error = function(e) {return(temp_grid[i,j])}))==0){temp_grid[i,j]}else if(tryCatch({temp_grid[i+1,j]}, error = function(e) {return("0")})==0){temp_grid[i,j]}else if(is.na(temp_grid[i+1,j])){temp_grid[i,j]}else{tryCatch({temp_grid[i+1,j]}, error = function(e) {return(temp_grid[i,j])})}
 temp_GridSequence[position1+2,4]=if(length(tryCatch({temp_grid[i,j-1]}, error = function(e) {return(temp_grid[i,j])}))==0){temp_grid[i,j]}else if(tryCatch({temp_grid[i,j-1]}, error = function(e) {return("0")})==0){temp_grid[i,j]}else if(is.na(temp_grid[i,j-1])){temp_grid[i,j]}else{tryCatch({temp_grid[i,j-1]}, error = function(e) {return(temp_grid[i,j])})}
 temp_GridSequence[position1+3,4]=if(length(tryCatch({temp_grid[i,j+1]}, error = function(e) {return(temp_grid[i,j])}))==0){temp_grid[i,j]}else if(tryCatch({temp_grid[i,j+1]}, error = function(e) {return("0")})==0){temp_grid[i,j]}else if(is.na(temp_grid[i,j+1])){temp_grid[i,j]}else{tryCatch({temp_grid[i,j+1]}, error = function(e) {return(temp_grid[i,j])})}

 temp_GridSequence[position1+4,4]=if(length(tryCatch({temp_grid[i-1,j-1]}, error = function(e) {return(temp_grid[i,j])}))==0){temp_grid[i,j]}else if(tryCatch({temp_grid[i-1,j-1]}, error = function(e) {return("0")})==0){temp_grid[i,j]}else if(is.na(temp_grid[i-1,j-1])){temp_grid[i,j]}else{tryCatch({temp_grid[i-1,j-1]}, error = function(e) {return(temp_grid[i,j])})}
 temp_GridSequence[position1+5,4]=if(length(tryCatch({temp_grid[i+1,j+1]}, error = function(e) {return(temp_grid[i,j])}))==0){temp_grid[i,j]}else if(tryCatch({temp_grid[i+1,j+1]}, error = function(e) {return("0")})==0){temp_grid[i,j]}else if(is.na(temp_grid[i+1,j+1])){temp_grid[i,j]}else{tryCatch({temp_grid[i+1,j+1]}, error = function(e) {return(temp_grid[i,j])})}
 temp_GridSequence[position1+6,4]=if(length(tryCatch({temp_grid[i+1,j-1]}, error = function(e) {return(temp_grid[i,j])}))==0){temp_grid[i,j]}else if(tryCatch({temp_grid[i+1,j-1]}, error = function(e) {return("0")})==0){temp_grid[i,j]}else if(is.na(temp_grid[i+1,j-1])){temp_grid[i,j]}else{tryCatch({temp_grid[i+1,j-1]}, error = function(e) {return(temp_grid[i,j])})}
 temp_GridSequence[position1+7,4]=if(length(tryCatch({temp_grid[i-1,j+1]}, error = function(e) {return(temp_grid[i,j])}))==0){temp_grid[i,j]}else if(tryCatch({temp_grid[i-1,j+1]}, error = function(e) {return("0")})==0){temp_grid[i,j]}else if(is.na(temp_grid[i-1,j+1])){temp_grid[i,j]}else{tryCatch({temp_grid[i-1,j+1]}, error = function(e) {return(temp_grid[i,j])})}




 }
 }
 }



 a=(parSapply(cl,1:nrow(temp_GridSequence),function(k,data_A,raw_data,raw_data_De,temp_GridSequence,temp_grid){
 s=1
 if(temp_GridSequence[k,1]==temp_GridSequence[k,4]){
 b=-10
 }else{
 b=as.numeric(IG(data_A[ncol(data_A)],raw_data[which(names(raw_data)==temp_GridSequence[k,4])])-(1/s)*MI(cbind(raw_data[which(names(raw_data)==temp_GridSequence[k,1])],raw_data[which(names(raw_data)==temp_GridSequence[k,4])]),raw_data))
 #b=FRST(raw_data_De,c(which(names(raw_data_De)==temp_GridSequence[k,1]),which(names(raw_data_De)==temp_GridSequence[k,4])))-1
 }
 return(b)
 },data_A,raw_data,raw_data_De,temp_GridSequence,temp_grid))


 a=as.data.frame(a)
 temp_GridSequence[3]=a

 for(i in 1:nrow(temp_GridSequence)){

 state=as.numeric(raw_data[,which(names(raw_data)==temp_GridSequence[i,1])])
 next_state=as.numeric(raw_data[,which(names(raw_data)==temp_GridSequence[i,4])])
 temp_GridSequence[i,5]=abs(cor(state,next_state))


 }
 avg=temp_GridSequence[,5]
 return(list(temp_grid=temp_grid,temp_GridSequence=temp_GridSequence,name=name,raw_data=raw_data,c4=c4,c1=c1,raw_data_De=raw_data_De,avg=avg))
 }


