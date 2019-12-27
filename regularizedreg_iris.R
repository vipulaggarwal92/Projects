library(LiblineaR)

data(iris)
attach(iris)


x <- iris[,1:4]  # inputs
y <- factor(iris[,5]) # target

# Constructing train and test data
train <- sample(1:dim(iris)[1],100)
xTrain <- x[train,]
xTest <- x[-train,]
yTrain <- y[train]
yTest <- y[-train]

# We need to normalize data before running LiblineaR
# To normalize data, you can use scale(data, center, scale) function.
# The formula newx = (x-center)/scale is used in this function. 

# Normalizing data
s <- scale(xTrain,center=TRUE,scale=TRUE)


LogisticRegModel <- LiblineaR(data = s,target = yTrain, type = 0, cost = 0.8)
LogisticRegModelCross <- LiblineaR(data = s,target = yTrain, type= 0, cost= 0.8, cross = 5)

# Find the best model with the best cost parameter via 10-fold cross-validations
tryTypes <- c(0:7)
tryCosts <- c(1000,1,0.001)
bestCost <- NA
bestAcc <- 0
bestType <- NA

for(ty in tryTypes){
  for(co in tryCosts){
    acc <- LiblineaR(data=s, target=yTrain, type=ty, cost=co, cross=5, verbose=FALSE)
    cat("Results for C=",co," : ",acc," accuracy.\n",sep="")
    if(acc>bestAcc){
      bestCost <- co
      bestAcc <- acc
      bestType <- ty
    }
  }
}

cat("Best model type is:",bestType,"\n")
cat("Best cost is:",bestCost,"\n")
cat("Best accuracy is:",bestAcc,"\n")

# Re-train best model with best cost value.
m <- LiblineaR(data=s,target=yTrain,type=bestType,cost=bestCost)

# Scale the test data
s2 <- scale(xTest,attr(s,"scaled:center"),attr(s,"scaled:scale"))

# Make prediction
p <- predict(m,s2)
p

# Display confusion matrix
res <- table(p$predictions,yTest)
print(res)


