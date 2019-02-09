getwd()
rm(list=ls())
install.packages("tm")

library("tm")
library(e1071)
library(gmodels)
setwd("C:/Users/Keshav/Desktop/Data science/Machine-Learning-with-R-datasets-master")
sms_raw=read.csv("sms_spam.csv",stringsAsFactors = FALSE)
sms_raw$type=factor(sms_raw$type)
sms_corpus=VCorpus(VectorSource(sms_raw$text))
summary(sms_corpus)
inspect(sms_corpus[1:20])
corpus_cleaned=tm_map(sms_corpus,removeNumbers)
corpus_cleaned=tm_map(corpus_cleaned,removeWords,stopwords())
corpus_cleaned=tm_map(corpus_cleaned,removePunctuation)
corpus_cleaned=tm_map(corpus_cleaned,stripWhitespace)

sms_dtm=DocumentTermMatrix(corpus_cleaned)
inspect(sms_dtm[,1:20])
str(sms_dtm)

sms_Freqterm=findFreqTerms(sms_dtm,5)
sms_reduced=DocumentTermMatrix(sms_corpus,list(dictionary=sms_Freqterm))
inspect(sms_Freqterm)


convert_counts=function(x){
  x=ifelse(x>0,1,0)
  x=factor(x,levels =c(0,1),labels =c("No","Yes"))
}

sms_final=apply(sms_reduced,2,convert_counts)

sms_final_train=sms_final[1:4169,]
sms_final_test=sms_final[4170:5574,]
sms_raw_train=sms_raw[1:4169,]
sms_raw_test=sms_raw[4170:5574,]
length(sms_raw_test$type)
nrow(sms_final_test)


Naive_model=naiveBayes(sms_final_train,sms_raw_train$type,laplace = 2)
summary(Naive_model)
View(Naive_model)
Naive_predicted=predict(Naive_model,sms_final_test)
table(Naive_predicted)


CrossTable(Naive_predicted,sms_raw_test$type,dnn=c("predicted","Actual"))

