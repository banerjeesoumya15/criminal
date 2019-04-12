setwd("C:/Users/soumya.banerjee01/Documents/R/criminal")
setwd("C:/Users/Soumya/Documents/R/criminal2")
#SVM algorithm
#https://www.analyticsvidhya.com/blog/2016/08/practicing-machine-learning-techniques-in-r-with-mlr-package/

#load libraries
library(data.table)
library(mlr)
library(kernlab)
#library(rpart)

train <- read.csv("criminal_train.csv", na.strings = "-1")
test <- read.csv("criminal_test.csv", na.strings = "-1")
train <- train[-c(19231,44282),]

table(train$NRCH17_2)
table(train$POVERTY3)
table(test$NRCH17_2)
table(test$POVERTY3)
summarizeColumns(train)
summarizeColumns(test)

train[which(is.na(train$NRCH17_2)),"NRCH17_2"] <- 0L
test[which(is.na(test$NRCH17_2)),"NRCH17_2"] <- 0L
train[which(is.na(train$POVERTY3)),"POVERTY3"] <- 3L
test[which(is.na(test$POVERTY3)),"POVERTY3"] <- 3L

#convert integer to factor
for(i in colnames(train[,c(2:68,70:72)])) {
  
    train[,c(2:68,70:72)][[i]] <- as.factor(factor(train[,c(2:68,70:72)][[i]]))
  
}
for(i in colnames(test[,c(2:68,70:71)])) {
  
    test[,c(2:68,70:71)][[i]] <- as.factor(factor(test[,c(2:68,70:71)][[i]]))
  
}

test$Criminal <- factor(x=c("0","1"))
summarizeColumns(train)
summarizeColumns(test)

setDT(train)[,.N/nrow(train),Criminal]
setDT(test)[,.N/nrow(test),Criminal]

#create task
traintask <- makeClassifTask(data = train, target = "Criminal")
testtask <- makeClassifTask(data = test, target = "Criminal")

#load svm
ksvm <- makeLearner("classif.ksvm", predict.type = "response")

#set parameters
pssvm <- makeParamSet(
  makeDiscreteParam("C", values = 2^c(-8,-4,-2,0)), #cost parameters
  makeDiscreteParam("sigma", values = 2^c(-8,-4,0,4)) #RBF kernel parameter
)

#specify search function
ctrl <- makeTuneControlGrid()

#set cross validation
#set_cv <- makeResampleDesc("RepCV", stratify = T)
set_cv <- makeResampleDesc("CV", iters=3L, stratify = T)

#tune model
res <- tuneParams(ksvm, task = top_task, resampling = set_cv,
                  par.set = pssvm, control = ctrl,
                  measures = acc,
                  show.info = T)

#set the model with best params
t.svm <- setHyperPars(ksvm, par.vals = res$x)

#train the model
par.svm <- train(ksvm, traintask)

#test
predict.svm <- predict(par.svm, testtask)

#submission file
submit <- data.frame(PERID=test$PERID, Criminal=predict.svm$data$response)
write.csv(submit, file = "submit_svm.csv",quote = FALSE,row.names = FALSE)

#selecting top 6 important features
top_task <- filterFeatures(traintask, method = "randomForest.importance", abs = 36)

