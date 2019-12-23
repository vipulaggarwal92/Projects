Training <- read.csv("D:/Fall 2019/Data Mining/Assignment 3/IMB 623 VMWare- Digital Buyer Journey/Training.csv")
traindata<-Training[,-1]
library(randomForest)
traindata$target<-as.factor(traindata$target)
attach(traindata)

#Finding % of 9999 values in each variable
data9999 <- cbind(names(traindata),
                  sapply(traindata,function (x)
                  { row1 = which(x==9999)
                  length(row1)/nrow(Training)
                  } ))
#Removing features with 70% or more 9999 values
col9999 <- which(data9999[,2]>0.7)
y<- traindata[,-col9999]