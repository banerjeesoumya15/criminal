
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
test$Criminal <- "0"
testtask <- makeClassifTask(data = test, target = "Criminal")

#normalise the data
traintask <- normalizeFeatures(traintask, method = "standardize",
                               cols = "ANALWT_C")
testtask <- normalizeFeatures(testtask, method = "standardize",
                               cols = "ANALWT_C")
#feature importance
im_feat <- generateFilterValuesData(traintask, 
            method = c("information.gain","chi.squared"))
plotFilterValues(im_feat, n.show = 20)

##### modelling #####

#1 Quadratic Discriminant Analysis (QDA)
#load qda
qda.learner <- makeLearner("classif.qda", predict.type = "response")
#train model
qmodel <- train(qda.learner, traintask)
#qda not working

####################### 2 Logistic Regression #####################
logistic.learner <- makeLearner("classif.logreg", predict.type = "response")
 #cross validation (cv) accuracy
cv.logistic <- crossval(learner = logistic.learner, task = traintask,
                        iters = 3, stratify = TRUE, measures = acc,
                        show.info = F)
#not working

######################### 3 Decision Tree ########################
#need to install rpart package
library(rpart)

#make tree learner
makeatree <- makeLearner("classif.rpart", predict.type = "response")

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV", iters=3L)

#search for hyper paramaters
gs <- makeParamSet(
  makeIntegerParam("minsplit",lower = 10,upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp",lower = 0.001,upper = 0.2)
)
#do a grid search
gscontrol <- makeTuneControlGrid()
#hypertune the parameters
stune <- tuneParams(learner = makeatree, resampling = set_cv,
                    task = traintask, par.set = gs, control = gscontrol,
                    measures = acc)
#check best parameters
stune$x
#cross validation result
stune$y

#using hyperparameters for modeling
t.tree <- setHyperPars(makeatree, par.vals = stune$x)
#train the model
t.rpart <- train(t.tree, traintask)
getLearnerModel(t.rpart)

#make predictions
tpmodel <- predict(t.rpart, testtask)

#create a submission file
submit <- data.frame(PERID = test$PERID, Criminal = tpmodel$data$response)
write.csv(submit, "submit_decision_tree.csv", row.names = F, quote = FALSE)

############################ 4 Random Forest #######################
