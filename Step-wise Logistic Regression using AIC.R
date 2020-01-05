library(readxl)
train_data <- read_excel("D:/Fall 2019/Data Mining/Assignment 4/IMB651-XLS-ENG.xlsx",sheet = "Training Data or Binary Class")
test_data <- read_excel("D:/Fall 2019/Data Mining/Assignment 4/IMB651-XLS-ENG.xlsx",sheet = "Test Data for Binary Class")


train_data$NPS_Status = as.factor(train_data$NPS_Status)
test_data$NPS_Status = as.factor(test_data$NPS_Status)

library(MASS)
library(dplyr)
stepmod = glm(NPS_Status~.,data = train_data,family = binomial) %>% stepAIC(trace = TRUE)
summary(stepmod)
proba = predict(stepmod,test_data,type = "response")
confmat = table(round(proba),test_data$NPS_Status)
confmat
accuracy <- sum(diag(confmat))/sum(confmat)
accuracy