
train_data <- read_excel("D:/Fall 2019/Data Mining/Assignment 4/IMB651-XLS-ENG.xlsx",sheet = "Training Data or Binary Class")
test_data <- read_excel("D:/Fall 2019/Data Mining/Assignment 4/IMB651-XLS-ENG.xlsx",sheet = "Test Data for Binary Class")

#Removing irrelevant features (long process of feature selection)
train_data <- subset(train_data, select =-c(MaritalStatus,BedCategory,State,Country,EM_DOCTOR,AE_PATIENTSTATUSINFO,DOC_TREATMENTEXPLAINATION,DOC_TREATMENTEFFECTIVENESS,NS_NURSESATTITUDE,NS_NURSEPATIENCE,OVS_OVERALLSTAFFATTITUDE))

library(fastAdaboost)
library(caret)
set.seed(1234)
#Ada-Boost for Binary
Binary_adaboost = adaboost(NPS_Status~. , data=data.frame(train_data), 10)
pred <- predict(Binary_adaboost,newdata=test_data, type="response")

confustionmatrix = table(pred$class,test_data$NPS_Status)
confustionmatrix

cat('\n','Accuracy ', '\n')
accuracy <- sum(diag(confustionmatrix))/sum(confustionmatrix)
accuracy
