Training <- read.csv("D:/Fall 2019/Data Mining/Assignment 3/IMB 623 VMWare- Digital Buyer Journey/Training.csv")
x<-Training[,-1]
zerovariancepredlist = apply(x, 2, function(x) length(unique(x))== 1 )
tdata = x[,!zerovariancepredlist]
