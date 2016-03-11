# data models 2016
training2 <- training[,c(3:17,32:46,ncol(training))]
#
# Next steps: 
# Add more variables: conference, seed, others
# Change measure from RMSE to logloss

####### Correlation
# identifying correlated predictors. Remove those with high correlation
require(corrplot)
descrCor <- cor(training2)
summary(descrCor[upper.tri(descrCor)])
# remove linearly correlated variables > corr_cutoff
highlyCorDescr <- findCorrelation(descrCor, cutoff = .95)
training3 <- training2[,-highlyCorDescr]
descrCor <- cor(training3)
corrplot(descrCor)
#
####### Collinearity
#
comboInfo <- findLinearCombos(training3)
comboInfo
if (is.null(comboInfo$remove)==FALSE) {
  training3 <- training3[, -comboInfo$remove]
}
#
# Partition the data
set.seed(3456)
trainIndex <- createDataPartition(training3$y_score, p = .8,
                                  list = FALSE,
                                  times = 1)
train <- training3[ trainIndex,]
valid  <- training3[-trainIndex,]

## models
model <- "lm"

set.seed(1)
lmTune <- train(y_score ~., data=train, 
                 method="lm",
                 metric="RMSE", 
                 verbose=FALSE)
#######
# The results

lmPred <- predict(lmTune, train)
str(lmPred)
validScores <- valid$y_score
valid2 <- select(valid,-y_score)
lmPred <- predict(lmTune, newdata = valid2)
compare <- cbind(validScores, lmPred)

# variable importance
lmImp <- varImp(lmTune, scale = TRUE)
lmImp
#gbmImp <- arrange(gbmImp$importance, -Overall)
lmImp <- data.frame(variable = rownames(lmImp$importance), 
                     importance = lmImp$importance$Overall)
lmImp <- arrange(lmImp, -importance)

