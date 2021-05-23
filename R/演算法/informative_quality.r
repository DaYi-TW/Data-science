informative_quality=function(data_A,filename,folder){

dataset_informative_quality=as.data.frame(matrix(,1,3))
dataset_informative_quality[1]=abs(sum((table(data_A[ncol(data_A)])/nrow(data_A[ncol(data_A)]))*log(table(data_A[ncol(data_A)])/nrow(data_A[ncol(data_A)]),2)))
dataset_informative_quality[2]=abs(length(table(data_A[ncol(data_A)]))*((1/length(table(data_A[ncol(data_A)])))*log((1/length(table(data_A[ncol(data_A)]))),2)))
dataset_informative_quality[3]=dataset_informative_quality[1]/dataset_informative_quality[2]
colnames(dataset_informative_quality)[1]="H"
colnames(dataset_informative_quality)[2]="Hmax"
colnames(dataset_informative_quality)[3]="balance"
print(dataset_informative_quality)

write.csv(dataset_informative_quality,file=paste0("C:/R/Result/",folder,"/",filename,"_imformative_quality.csv"))
return(informative_quality)
}