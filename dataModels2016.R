# data models 2016 -------------------------
#training2 <- training[,c(3:17,32:46,ncol(training))]
training2 <- training[,c(3:ncol(training))]
# remove seed difference from training? Yes, it dominates in sample and overfits.
# In fact, scoreRatio dominates so much that the overfitting happens anyways.
# 
training2 <- select(training2, -seedDiff)
# ---------------------------------
####### Correlation
# identifying correlated predictors. Remove those with high correlation
require(corrplot)
descrCor <- cor(training2)
summary(descrCor[upper.tri(descrCor)])
# remove linearly correlated variables > corr_cutoff
highlyCorDescr <- findCorrelation(descrCor, cutoff = .95)
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
model <- "lm"

set.seed(1)
modelTune <- train(y_score ~., data=train, 
                 method="lm",
                 metric="RMSE", 
                 verbose=FALSE)
#######
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
# testing NEW DATA 2016
testing2 <- testing[,c(3:ncol(testing))]
testTeams <- as.character(testing[,c(2)])
# remove seed difference from training? Yes, it dominates in sample and overfits.
# In fact, scoreRatio dominates so much that the overfitting happens anyways.
# 
testing2 <- select(testing2, -seedDiff)

newData <- as.data.frame(testing2)
modelPred <- predict(modelTune, newdata = newData) # obtain predictions on new data
final <- cbind(testTeams,modelPred)
final <- as.data.frame(final)
final$modelPred <- as.numeric(as.character(final$modelPred))
# Final submission format -------------
final <- mutate(final, sigmoid = .logistic(modelPred,.3))
# ---------------------------------
# read 538 data
five38 <- read.csv("data/fiveThirtyEightPredictions.csv")
five38 <- merge(five38, teams)



# variable importance
modelImp <- varImp(modelTune, scale = TRUE)
modelImp
#gbmImp <- arrange(gbmImp$importance, -Overall)
modelImp <- data.frame(variable = rownames(modelImp$importance), 
                       importance = modelImp$importance$Overall)
modelImp <- arrange(modelImp, -importance)
