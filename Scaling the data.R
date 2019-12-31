library(readxl)
train_data <- read_excel("D:/Fall 2019/Data Mining/Assignment 4/IMB651-XLS-ENG.xlsx",sheet = "Training Data for Multi-Class M")

#Using Smote, upscale and downscale to balance the data
library(caret)
library(dplyr)

train_data$MaritalStatus <- as.factor(train_data$MaritalStatus)
train_data$BedCategory <- as.factor(train_data$BedCategory)
train_data$State <- as.factor(train_data$State)
train_data$Country <- as.factor(train_data$Country)
train_data$NPS_Status <- as.factor(train_data$NPS_Status)

data_train_multi_downscale<-downSample(x=train_data[, -which(names(train_data) %in% c("NPS_Status"))],
                                       y=train_data$NPS_Status)
data_train_multi_downscale$NPS_Status <- data_train_multi_downscale$Class
data_train_multi_downscale <- data_train_multi_downscale[, -which(names(train_data) %in% c("NPS_Status"))]

data_train_multi_upscale<-upSample(x=train_data[, -which(names(train_data) %in% c("NPS_Status"))],
                                   y=train_data$NPS_Status)
data_train_multi_upscale$NPS_Status <- data_train_multi_upscale$Class
data_train_multi_upscale <- data_train_multi_upscale[, -which(names(train_data) %in% c("NPS_Status"))]

table(train_data$NPS_Status)
table(data_train_multi_downscale$NPS_Status)
table(data_train_multi_upscale$NPS_Status)