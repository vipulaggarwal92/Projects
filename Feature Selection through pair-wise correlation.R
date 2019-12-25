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

#Finding % of Unknown values in each variable
dataUnknown <- cbind(names(y),
                     sapply(y,function (x)
                     { row1 = which(x=='Unknown')
                     length(row1)/nrow(Training)
                     } ))
#Removing features with 70% or more Unknown values
colUnknown <- which(dataUnknown[,2]>0.7)
y<- y[,-colUnknown]

#Removing features with 70% or more NA values
x <- y[ lapply( y, function(x) sum(is.na(x)) / length(x) ) < 0.3 ]


#Removing features that have zero variance (constant values)
zerovariancepredlist = apply(x, 2, function(x) length(unique(x))== 1 )
tdata = x[,!zerovariancepredlist]


#Separating numerical features
col_list <- unlist(lapply(tdata, is.numeric))
num_data <- tdata[ , col_list]



library(caret)
#Checking pair-wise correlation and keeping only one out of highly correlated pairs with 90% cutoff.
correlationmat = cor(num_data[,1:ncol(num_data)])
highlycorrelatedmat = unlist(findCorrelation(correlationmat,cutoff = 0.9))
vecind = c()
for (i in 1:length(highlycorrelatedmat)){
  vecind = c(vecind,i)
}
num_data_new = num_data[,-vecind]