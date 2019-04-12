#load libraries
library(data.table)
library(mlr)
library(rpart)

#load data
train <- read.csv("criminal_train.csv", na.strings = "-1")
test <- read.csv("criminal_test.csv", na.strings = "-1")

summarizeColumns(train)
summarizeColumns(test)

d <- boxplot(train$ANALWT_C)
length(d$out)

#explore data
which(is.na(train$IFATHER))
train <- train[-which(is.na(train$IFATHER)),]

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
summarizeColumns(train)
summarizeColumns(test)

#2 variables have NA. apply mode
table(train$NRCH17_2)
train[which(is.na(train$NRCH17_2)),"NRCH17_2"] <- "0"
table(test$NRCH17_2)
test[which(is.na(test$NRCH17_2)),"NRCH17_2"] <- "0"

table(train$POVERTY3)
train[which(is.na(train$POVERTY3)),"POVERTY3"] <- "3"
table(test$POVERTY3)
test[which(is.na(test$POVERTY3)),"POVERTY3"] <- "3"

summarizeColumns(train)
summarizeColumns(test)
str(train)
str(test)

#create a task
traintask <- makeClassifTask(data = train, target = "Criminal",
                             positive = "1")
test$Criminal <- factor(c("0","1"))
testtask <- makeClassifTask(data = test, target = "Criminal")

#normalise the data
traintask <- normalizeFeatures(traintask, method = "standardize",
                               cols = "ANALWT_C")
testtask <- normalizeFeatures(testtask, method = "standardize",
                              cols = "ANALWT_C")

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

#using hyperparameters for modeling
t.tree <- setHyperPars(makeatree, par.vals = stune$x)
#train the model
t.rpart <- train(t.tree, traintask)

#make predictions
tpmodel <- predict(t.rpart, testtask)

#create a submission file
submit <- data.frame(PERID = test$PERID, Criminal = tpmodel$data$response)
write.csv(submit, "submit_decision_tree.csv", row.names = F, quote = FALSE)
