# criminal
#predict criminal using different methods

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
