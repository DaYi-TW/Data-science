#輸入: s_0:挑選起點之屬性；s_terminal:挑選的終點(亦即學習終點)；model:輸入學習收斂的model；discrete_data:離散化資料；class_label:目標分類屬性；Actions:行動方案；experience:經驗表
#輸出: output屬性集合 ；featureAcc屬性準確度；featureROC屬性的ROC
examFeature=function(s_0,s_terminal,model,discrete_data,class_label,Actions,experience){
  print("learning finish")
  Q_table=model$Q
  OptimalPolicy=as.data.frame(Actions[max.col(Q_table)])
  OptimalPolicy[,1]=as.character(OptimalPolicy[,1])
  rownames(OptimalPolicy)=rownames(Q_table)
  output=pick_featureset(s_0,s_terminal,OptimalPolicy,experience)
  test=output_data(discrete_data,output,class_label)
  test=as.data.frame(lapply(test,as.factor))
  AccROC=acc_test(test,length(output))
  featureAcc=AccROC$acc_res
  featureROC=AccROC$roc_res
  return(list(output=output,featureAcc=featureAcc,featureROC=featureROC))
}

pick_featureset=function(s_0,s_terminal,OptimalPolicy,experience){
  state=s_0
  ste=1
  while(state !=s_terminal){

    action_optimal=OptimalPolicy[which(rownames(OptimalPolicy)==state),]
    NextState_optimal=experience[which(experience$State==state & experience$Action==action_optimal),]$NextState
    if(ste==1){
      output=c(state,NextState_optimal)
    }else{
      output=c(output,NextState_optimal)
    }
    state=NextState_optimal
    ste=ste+1
  }
  return(output)
}


ACC=function(set){
  ac=classANA(set)
  acc=cbind(ac$J48_ac,ac$SMO_ac,ac$LMT_ac,ac$NB_ac,ac$kNN_ac)
  roc=cbind(ac$J48_roc,ac$SMO_roc,ac$LMT_roc,ac$NB_roc,ac$kNN_roc)
  return(list(acc=acc,roc=roc))
}

output_data=function(discrete_data,output,class_label){
  for(i in 1:length(output)){
    if(i==1){
      test=discrete_data[which(colnames(discrete_data)==output[i])]
    }else{
      test=cbind(test,discrete_data[which(colnames(discrete_data)==output[i])])
    }
  }
  test=as.data.frame(cbind(test,class_label[ncol(class_label)]))
}


acc_test=function(test,feature_num){
  pb <- txtProgressBar(min = 0, max = feature_num, style = 3)
  for(i in 1:feature_num){
    res=ACC(cbind(test[(ncol(test)-i):(ncol(test)-1)],test[ncol(test)]))
    if(i==1){
      acc_res=res$acc
      roc_res=res$roc
    }else{
      acc_res=rbind(acc_res,res$acc)
      roc_res=rbind(roc_res,res$roc)
    }
    setTxtProgressBar(pb, i)
  }
  return(list(acc_res=acc_res,roc_res=roc_res))
}



pick_featureset=function(s_terminal,Q,exp,num){
  ans=list()
  exp=exp[which(exp[,1]!=exp[,4]),]
  exp=exp[which(exp[,1]!=s_terminal),]
  score=Q[s_terminal,1]
  feature=s_terminal
  subsets=cbind(score,feature)
  ans[[1]]=subsets
  pb <- txtProgressBar(min = 1, max = num, style = 3)
  for(i in 2:num){
    if(subsets!=NULL){
      setTxtProgressBar(pb, i)
      previous=exp[which(exp[,4]==subsets[1,i]),]
      feature=as.data.frame(subsets[1,-1])
      score=as.data.frame(subsets[1,1])
      if(nrow(previous)>1){
        for(p in 2:nrow(previous)){
          feature=rbind(feature,subsets[1,-1])
          score=rbind(score,subsets[1,1])
        }
      }
      if(nrow(subsets)>1){
        for(j in 2:nrow(subsets)){
          temp=exp[which(exp[,4]==subsets[j,i]),]
          previous=rbind(previous,temp)

          if(nrow(previous)>1){
            for(p in 1:nrow(temp)){
              feature=rbind(feature,subsets[j,-1])
              score=rbind(score,subsets[j,1])
            }
          }
        }
      }
      subsets=NULL

      if(nrow(previous)>1){
      #print(nrow(previous))
      for(k in 1:nrow(previous)){
        p_state=previous[k,1]
        p_action=previous[k,2]
        if(sum(feature[k,]==p_state)==0){
          if(names(which.max(Q[p_state,]))==p_action){
            scores_t=as.numeric(score[k,1])+Q[p_state,p_action]
            features_t=as.data.frame(cbind(feature[k,],p_state))
            subsets_t=cbind(scores_t,features_t)
            if(is.null(subsets)){
              subsets=cbind(scores_t,features_t)
              }else{
                subsets=rbind(subsets,subsets_t)
              }
            }
          }
        }
      }
      subsets=unique(as.data.frame(subsets))
      ans[[i]]=subsets
    }
  }
  return(ans)
}

acc_eval=function(class_label,Discrete_data,subset){
  acc=as.data.frame(matrix(,length(subset),5,dimnames=list(1:length(subset),c("C4.5","SVM","LMT","kNN","NB"))))
  roc=as.data.frame(matrix(,length(subset),5,dimnames=list(1:length(subset),c("C4.5","SVM","LMT","kNN","NB"))))
  pb <- txtProgressBar(min = 0, max = length(subset),style=3)
  for(i in 1:length(subset)){
    setTxtProgressBar(pb, i)
    suboptimal=subset[[i]][which.max(subset[[i]][,1]),]
    f_n=as.character(suboptimal[2:length(suboptimal)])
    index=sapply(1:length(f_n),FUN=function(i,Discrete_data,f_n){which(colnames(Discrete_data)==f_n[i])},Discrete_data,f_n)
    TD=cbind(Discrete_data[index],class_label)
    TD=as.data.frame(lapply(TD,as.factor))
    res=classANA(TD)
    acc[i,]=as.data.frame(res[seq(1, 9, by=2 )])
    roc[i,]=as.data.frame(res[seq(2, 10, by=2 )])
  }
  return(list(acc=acc,roc=roc))
}
