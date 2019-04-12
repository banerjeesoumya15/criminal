#https://www.analyticsvidhya.com/blog/2016/08/practicing-machine-learning-techniques-in-r-with-mlr-package/

library(randomForest)

rf <- makeLearner("classif.randomForest", 
                  predict.type = "response", 
                  par.vals = list(ntree = 200, mtry = 3))
rf$par.vals <- list(
  importance = TRUE
)

rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 8, upper = 15),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)

#gscontrol <- makeTuneControlGrid()
rancontrol <- makeTuneControlRandom(maxit = 10L)
set_cv <- makeResampleDesc("CV",iters = 3L)

rf_tune <- tuneParams(learner = rf, resampling = set_cv,
                      task = traintask, par.set = rf_param,
                      control = rancontrol,
                      measures = list(tpr,tnr,fpr,fnr,acc),
                      show.info = T)

#using hyper parameters for modeling
rf.tree <- setHyperPars(rf, par.vals = rf_tune$x)

#train a model
rforest <- train(rf.tree, traintask)
getLearnerModel(rforest)

#make predictions
rfmodel <- predict(rforest, testtask)
