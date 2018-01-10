# criminal
#predict criminal using different methods
#https://www.analyticsvidhya.com/blog/2016/08/practicing-machine-learning-techniques-in-r-with-mlr-package/
#https://www.hackerearth.com/challenge/competitive/predict-the-criminal/machine-learning/predict-the-criminal/

setwd("C:/Users/soumya.banerjee01/Documents/R/criminal")
library(data.table)
library(FSelector) #for generateFilterValuesData()
train <- fread("criminal_train.csv", na.strings = "-1")
test <- fread("criminal_test.csv", na.strings = "-1")
train1 <- read.csv("criminal_train.csv", na.strings = "-1")
library(mlr)
summarizeColumns(train)
str(train)
boxplot(train$ANALWT_C)
summary(train)
which(is.na(train$IFATHER))
train <- train[-which(is.na(train$IFATHER)),]

table(train$NRCH17_2)

#convert integer to factor
for(f in colnames(train[,c(2:72)])) {
  if(class(train[,c(2:72)][[f]])=="integer") {
    train[,c(2:72)][[f]] <- as.factor(factor(train[,c(2:72)][[f]]))
  }
}

for(f in colnames(test[,c(2:71)])) {
  if(class(test[,c(2:71)][[f]])=="integer") {
    test[,c(2:71)][[f]] <- as.factor(factor(test[,c(2:71)][[f]]))
  }
}

#2 variables have NA. apply mode
table(train$NRCH17_2)
head(which(is.na(train$NRCH17_2)))
train[which(is.na(train$NRCH17_2)),"NRCH17_2"] <- "0"

table(train$POVERTY3)
head(which(is.na(train$POVERTY3)))
train[which(is.na(train$POVERTY3)),"POVERTY3"] <- "3"

summarizeColumns(train)
lev_train=list()
lev_test=list()
#explore both datasets
for(f in colnames(train[,c(2:72)])) {
  if(class(train[,c(2:72)][[f]])=="factor"){
    lev_train[[f]] <- unique(train[,c(2:72)][[f]])
    #print(table(train[,c(2:72)][[f]])) 
  }
}

for(f in colnames(test[,c(2:71)])) {
  if(class(test[,c(2:71)][[f]])=="integer"){
    lev_test[[f]] <- unique(test[,c(2:71)][[f]])
    #print(table(test[,c(2:71)][[f]])) 
  }
}

for(f in c(2:71)) {
  print(sort(lev_test[[1]])==sort(lev_train[[1]]))
}

#create a task
traintask <- makeClassifTask(data = train, target = "Criminal",
                             positive = "1")
test$Criminal <- "0"          #test$Criminal <- factor(x=c("0","1"))
testtask <- makeClassifTask(data = test, target = "Criminal")
