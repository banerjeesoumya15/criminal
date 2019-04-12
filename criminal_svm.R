#SVM

library(mlr)

train <- read.csv("criminal_train.csv", na.strings = "-1")
test <- read.csv("criminal_test.csv", na.strings = "-1")

which(is.na(train$IFATHER))
train <- train[-c(19231,44282),]

summarizeColumns(train)

table(train$NRCH17_2)
head(which(is.na(train$NRCH17_2)))
train[which(is.na(train$NRCH17_2)),"NRCH17_2"] <- 0L

table(train$POVERTY3)
head(which(is.na(train$POVERTY3)))
train[which(is.na(train$POVERTY3)),"POVERTY3"] <- 3L

summarizeColumns(test)
test$Criminal <- c(0,1)
table(test$NRCH17_2)
which(is.na(test$NRCH17_2))
test[which(is.na(test$NRCH17_2)),"NRCH17_2"] <- 0L

table(test$POVERTY3)
head(which(is.na(test$POVERTY3)))
test[which(is.na(test$POVERTY3)),"POVERTY3"] <- 3L

test$Criminal <- c(0L,1L)
