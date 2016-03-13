# Compute logloss function
# input data.frame must have 2 columns, first one the predicted probs and 
# second one the actual outcome: 1 if team A beat team B and 0 otherwise. Ex:
# 0.567 1
# 0.243 0
# 0.890 0
# ---------------------------
# log loss function
.logLoss <- function(predProbs){
  
  names(predProbs) <- c("prob","y")
  predProbs <- mutate(predProbs, partialSum = -(y*log(prob) + (1-y)*log(1-prob)))
  logLoss <- sum(predProbs$partialSum)/nrow(predProbs)
  return(logLoss)
}
#predProbs <- data.frame(a = c(0.5,0.7,0.2,0.8,0.85), b= c(0,1,0,1,1))
#.logLoss(predProbs)

# --------------------------
# logistic function. z = sum(wi*xi)
.logistic <- function(z,l){
  #theta <- 1/(1 + exp(-z))
  #l <- 0.2
  theta <- 1/(1 + exp(-l*z))
  return(theta)
}

#val <- seq(-20,20,1)
#plot(val,.logistic(val,0.3))

