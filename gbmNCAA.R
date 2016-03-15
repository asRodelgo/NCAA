# data models 2016
#training2 <- training[,c(3:17,32:46,ncol(training))]
training2 <- training[,c(3:ncol(training))]
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
# The model
model <- "gbm"
require(pROC)
require(e1071)

grid <- expand.grid(.interaction.depth = c(6),
                    .n.trees=seq(100,1000,by=100),
                    .shrinkage=c(0.003,0.005,0.01),
                    .n.minobsinnode=5)

ctrl <- trainControl(method="repeatedcv",
                     number=10,
                     repeats=10)

set.seed(1)
modelTune <- train(y_score ~., data=train, 
                   method="gbm",
                   metric="RMSE",
                   tuneGrid=grid,
                   trControl=ctrl,
                   verbose=FALSE)
#######
# ---------------------------------
# The results
modelPred <- predict(modelTune, train)
# validation dataset
validScores <- valid$y_score # keep original yi somewhere
valid <- as.data.frame(valid)
valid2 <- select(valid,-y_score) # remove yi from data

# NEW DATA GOES HERE
newData <- valid2
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

