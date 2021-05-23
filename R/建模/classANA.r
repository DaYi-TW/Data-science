classANA=function(x)
{
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
colnames(x)[ncol(x)]="class";
x=as.data.frame(x)
m1=J48(class~., data = x);
m2=SMO(class~., data = x);
m3=LMT(class~., data = x);
m6=NB(class ~ ., data = x)
m7=IBk(class ~ .,data= x)

## Use 10 fold cross-validation.
e1 <- evaluate_Weka_classifier(m1, numFolds = 10, complexity = TRUE, seed = 123, class = TRUE);
e2 <- evaluate_Weka_classifier(m2, numFolds = 10, complexity = TRUE, seed = 123, class = TRUE);
e3 <- evaluate_Weka_classifier(m3, numFolds = 10, complexity = TRUE, seed = 123, class = TRUE);
#e4 <- evaluate_Weka_classifier(m4, numFolds = 10, complexity = TRUE, seed = 123, class = TRUE);
#e5 <- evaluate_Weka_classifier(m5, numFolds = 10, complexity = TRUE, seed = 123, class = TRUE);
e6 <- evaluate_Weka_classifier(m6, numFolds = 10, complexity = TRUE, seed = 123, class = TRUE);
e7 <- evaluate_Weka_classifier(m7, numFolds = 10, complexity = TRUE, seed = 123, class = TRUE);

a=6*(ncol(e1$confusionMatrix)-1)+5;
b=6*ncol(e1$confusionMatrix);

ROC1=sum(e1$detailsClass[a:b]*rowSums(e1$confusionMatrix))/sum(rowSums(e1$confusionMatrix));
ROC2=sum(e2$detailsClass[a:b]*rowSums(e2$confusionMatrix))/sum(rowSums(e2$confusionMatrix));
ROC3=sum(e3$detailsClass[a:b]*rowSums(e3$confusionMatrix))/sum(rowSums(e3$confusionMatrix));
#ROC4=sum(e4$detailsClass[a:b]*rowSums(e4$confusionMatrix))/sum(rowSums(e4$confusionMatrix));
#ROC5=sum(e5$detailsClass[a:b]*rowSums(e5$confusionMatrix))/sum(rowSums(e5$confusionMatrix));
ROC6=sum(e6$detailsClass[a:b]*rowSums(e6$confusionMatrix))/sum(rowSums(e6$confusionMatrix));
ROC7=sum(e7$detailsClass[a:b]*rowSums(e7$confusionMatrix))/sum(rowSums(e7$confusionMatrix));

return(list(J48_ac=e1$details[1],J48_roc=ROC1,SMO_ac=e2$details[1],SMO_roc=ROC2,LMT_ac=e3$details[1],LMT_roc=ROC3, NB_ac=e6$details[1],NB_roc=ROC6, kNN_ac=e7$details[1],kNN_roc=ROC7));

}
