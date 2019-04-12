#to compare all levels of test and train

table(train$IFATHER)
table(test$IFATHER)

for (f in colnames(train[,c(2:68,70:72)])) {
  print(table(train[,c(2:68,70:72)][[f]]))
  print(c("-----------------",f))
  print(table(test[,c(2:68,70:72)][[f]]))
}

HLNVSOR remove 94 from train
HLNVNEED same
HLNVREF same
HLNVOFFR same
HLNVCOST same
HLCNOTMO remove 85 from train
PRXYDATA remove 97 from train
PRXRETRY remove 97 from train

table(train$PRXRETRY)
table(test$PRXRETRY)
#train[which(train$PRXRETRY==97L),"PRXRETRY"] <- 99L
train <- train[-which(train$PRXRETRY==97L),]

# not needed
table(train$PRXYDATA)
table(test$PRXYDATA)
#train[which(train$PRXYDATA==97L),"PRXYDATA"] <- 99L
train <- train[-which(train$PRXYDATA==97L),]

# table(train$MEDICARE)
# table(test$MEDICARE)
# test[which(test$MEDICARE==98L),"MEDICARE"] <- 2L
# 
# table(train$CAIDCHIP)
# table(test$CAIDCHIP)
# test[which(test$CAIDCHIP==98L),"CAIDCHIP"] <- 2L
# 
# table(train$CHAMPUS)
# table(test$CHAMPUS)
# test[which(test$CHAMPUS==98L),"CHAMPUS"] <- 2L
# 
# table(train$PRVHLTIN)
# table(test$PRVHLTIN)
# test[which(test$PRVHLTIN==98L),"PRVHLTIN"] <- 1L
# 
# table(train$HLTINNOS)
# table(test$HLTINNOS)
# test[which(test$HLTINNOS==98L),"HLTINNOS"] <- 99L
# 
table(train$HLCNOTMO)
table(test$HLCNOTMO)
#train[which(train$HLCNOTMO==85L),"HLCNOTMO"] <- 99L
train <- train[-which(train$HLCNOTMO==85L),]

table(train$HLNVCOST)
table(test$HLNVCOST)
#train[which(train$HLNVCOST==94L),"HLNVCOST"] <- 99L
train <- train[-which(train$HLNVCOST==94L),]

# not needed
table(train$HLNVOFFR)
table(test$HLNVOFFR)
#train[which(train$HLNVOFFR==94L),"HLNVOFFR"] <- 99L
train <- train[-which(train$HLNVOFFR==94L),]

# not needed
table(train$HLNVREF)
table(test$HLNVREF)
#train[which(train$HLNVREF==94L),"HLNVREF"] <- 99L
train <- train[-which(train$HLNVREF==94L),]

# not needed
table(train$HLNVNEED)
table(test$HLNVNEED)
#train[which(train$HLNVNEED==94L),"HLNVNEED"] <- 99L
train <- train[-which(train$HLNVNEED==94L),]

# not needed
table(train$HLNVSOR)
table(test$HLNVSOR)
#train[which(train$HLNVSOR==94L),"HLNVSOR"] <- 99L
train <- train[-which(train$HLNVSOR==94L),]

#convert to factor
for (f in colnames(train[,c(2:68,70:72)])) {
  train[,c(2:68,70:72)][[f]] <- as.factor(factor(train[,c(2:68,70:72)][[f]]))
}
for (f in colnames(test[,c(2:68,70:72)])) {
  test[,c(2:68,70:72)][[f]] <- as.factor(factor(test[,c(2:68,70:72)][[f]]))
}
summarizeColumns(train)
summarizeColumns(test)
