library("class")
library("gmodels")

irisdata=iris

#randomising the order of data
irisdata=irisdata[sample(nrow(irisdata)),]

#Checking for NAs
print(which(is.na(irisdata)))

#Normalising the data
normalise=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
irisdata_normal=as.data.frame(lapply(irisdata[,1:4],normalise))
#Allocating 70% of data to training set
train_set=irisdata_normal[1:105,]
#Allocating 30% data to test set
test_set=irisdata_normal[106:150,]

#making Train and test labels
train_set_labels=irisdata[1:105,5]
test_set_labels=irisdata[106:150,5]

#KNN function with function for optimal value of k using accuracy
TestK=function(x){
knn_predicted_values=knn(train_set,test_set,train_set_labels,k=x,use.all = TRUE)
#Accuracy
accuracy=(confmatrix$t[1,1]+confmatrix$t[2,2]+confmatrix$t[3,3])/length(test_set_labels)
return(accuracy)
}
chooseK=lapply(1:25,TestK)
#Plotting for finding best K value
plot(1:25,chooseK,xlab = "K Values",ylab = "Respective accuracy values",main = "K VS Respective Accuracy")
#making confusion matrix
confmatrix=CrossTable(test_set_labels,knn_predicted_values,prop.chisq = FALSE)
#Truepositive rate for virginica
TPR=(confmatrix$t[3,3])/sum(confmatrix$t[3,1:3])
#Truenegative rate for virginica
TNR=(confmatrix$t[1,1]+confmatrix$t[2,2])/sum(confmatrix$t[1:2,1:3])
#Falsepositive rate for virginica
FPR=1-TPR
#Falsenegative rate for virginica
FNR=1-TNR
#Missclassification rate or error rate=1-accuracy
misclass=(FP+FN)/length(test_set_labels)
#Precision for virginica
precision=confmatrix$t[3,3]/sum(confmatrix$t[1:3,3])
#Null Error rate:In our example majority class is versicolor
NullErrorRate=sum(confmatrix$t[1:2,1:3])/sum(confmatrix$t) #instead of sum(confmatrix$t),length(test_set_labels) can also be written
#Recall(Sensitivity) for Virginica
###Better way to do above

install.packages("caret")
library(caret)
temp=confusionMatrix(knn_predicted_values,test_set_labels, positive = NULL,prevalence = NULL,dnn = c("Predicted","Refrence"))
print(temp)
