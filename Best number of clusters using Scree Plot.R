library(readxl)
data <- read_excel("D:/Fall 2019/Data Mining/Assignment 5/prospect.xls")
clust_data <- subset(data, select=-c(ID,LOC))

#Checking the number of rows with missing data
sum(is.na(clust_data))

clust_data_na <- data.frame(na.omit(clust_data))


clust_data_final = clust_data_na
#Coverting relevant variables into factors
clust_data_final[,3] <- as.factor(clust_data_na[,3])
clust_data_final[,4] <- as.factor(clust_data_na[,4])
clust_data_final[,5] <- as.factor(clust_data_na[,5])
clust_data_final[,6] <- as.factor(clust_data_na[,6])
clust_data_final[,7] <- as.factor(clust_data_na[,7])

#Separating numerical variables and scaling them
mins <- apply(clust_data_final[,1:2], 2, min)
maxs <- apply(clust_data_final[,1:2], 2, max)
scaled_data <- as.data.frame(scale(clust_data_final[,1:2], center = mins, scale = (maxs-mins)))
summary(scaled_data)

scaled_data_final <- cbind(scaled_data,clust_data_final[,3:7])

library(clustMixType)

bestkplot <- function(data, nc=25, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  
  for (i in 2:nc)
  {
    set.seed(seed)
    wss[i] <- sum(kproto(data, k=i)$withinss)
  }
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
}

bestkplot(scaled_data_final, nc=25) 