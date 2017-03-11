# data models 2016
#training2 <- training[,c(3:17,32:46,ncol(training))]
training2 <- training[,c(3:ncol(training))]
# remove seed difference from training? Yes, it dominates in sample and overfits.
# In fact, scoreRatio dominates so much that the overfitting happens anyways.
# Try LASSO
training2 <- select(training2, -seedDiff)
#
# Next steps: 
# Add more variables: conference, seed, others

# ---------------------------------
####### Correlation
# identifying correlated predictors. Remove those with high correlation
require(corrplot)
descrCor <- cor(training2)
summary(descrCor[upper.tri(descrCor)])
# remove linearly correlated variables > corr_cutoff
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
training2 <- training2[,-highlyCorDescr]
descrCor <- cor(training2)
corrplot(descrCor)
#
# ---------------------------------
####### Collinearity
#
comboInfo <- findLinearCombos(training2)
comboInfo
if (is.null(comboInfo$remove)==FALSE) {
  training2 <- training2[, -comboInfo$remove]
}
#
# ---------------------------------
# Partition the data
set.seed(3456)
trainIndex <- createDataPartition(training2$y_score, p = .8,
                                  list = FALSE,
                                  times = 1)
train <- training2[ trainIndex,]
valid  <- training2[-trainIndex,]

# ---------------------------------
## models
# The model
model <- "glmnet"
require(glmnet)

grid <- expand.grid(.alpha=c(0.5,0.9),
                  .lambda=seq(0.0001,0.01,0.001))

ctrl <- trainControl(method="repeatedcv",
                     number=10,
                     repeats=10)

set.seed(1)
modelTune <- train(y_score ~., data=train, 
                   method="glmnet",
                   metric="RMSE",
                   tuneGrid=grid,
                   trControl=ctrl,
                   verbose=FALSE)
#######
# ---------------------------------
# The results and in Sample error (logLoss)
modelPred <- predict(modelTune, train)
trainScores <- train$y_score
train <- as.data.frame(train)
train2 <- select(train,-y_score) # remove yi from data
compare <- cbind(trainScores, modelPred) # put together yi and pred_yi
compare <- as.data.frame(compare)
compare <- mutate(compare, z_score = ifelse(trainScores>0,1,0),
                  sigmoid = .logistic(modelPred,.3))
# calculate logLoss
logLoss <- .logLoss(compare[,c("sigmoid","z_score")])

# ---------------------------------
# validation dataset out of Sample error
modelPred <- predict(modelTune, valid)
validScores <- valid$y_score # keep validation yi somewhere
valid <- as.data.frame(valid)
valid2 <- select(valid,-y_score) # remove yi from data
compare <- cbind(validScores, modelPred) # put together yi and pred_yi
compare <- as.data.frame(compare)
compare <- mutate(compare, z_score = ifelse(validScores>0,1,0),
                  sigmoid = .logistic(modelPred,.3))
# calculate logLoss
logLoss <- .logLoss(compare[,c("sigmoid","z_score")])

# ---------------------------------
# testing NEW DATA
newData <- as.data.frame(valid2)
newData <- select(newData,-y_score) # remove yi from data
modelPred <- predict(modelTune, newdata = newData) # obtain predictions on new data

compare <- cbind(validScores, modelPred) # put together yi and pred_yi
compare <- as.data.frame(compare)
compare <- mutate(compare, z_score = ifelse(validScores>0,1,0),
                  sigmoid = .logistic(modelPred,.3))
# ---------------------------------
# calculate logLoss
logLoss <- .logLoss(compare[,c("sigmoid","z_score")])



# variable importance
modelImp <- varImp(modelTune, scale = TRUE)
modelImp
#gbmImp <- arrange(gbmImp$importance, -Overall)
modelImp <- data.frame(variable = rownames(modelImp$importance), 
                       importance = modelImp$importance$Overall)
modelImp <- arrange(modelImp, -importance)

