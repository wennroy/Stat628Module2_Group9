rm(list=ls())

# Read the data ----------------------------------------
# setwd("D:/WISC/stat628/Module 2/project")
BodyFatData <- read.csv('../BodyFat.csv')

del_index = c(33, 39, 42, 48, 76, 96, 163, 172, 182, 216, 221)
BodyFat = BodyFatData[-del_index,]
BodyFat = subset(BodyFat, select = -c(IDNO,DENSITY))

# Create K-fold ----------------------------

## I see many cv function are written like this.
## The number in each fold may not be equal.

K = 10
k_fold_data <- function(K = 10, data){
  n = nrow(data)
  f <- ceiling(n/K)
  s <- sample(rep(1L:K, f), n)
  n.s <- table(s)
  print(n.s)
  
  test_data = list()
  train_data = list()
  for (i in c(1:K)){
    test_data[[i]] = data[which(s==i),]
    train_data[[i]] = data[which(s!=i),]
  }
  return(list(test_data,train_data))
}

res = k_fold_data(data = BodyFat)

MSE_lm_train = numeric(K) # Simple linear model
MSE_lm_test = numeric(K) # Simple linear model
MSE_NR6_train = numeric(K) # Nonparametirc model with 3 variables
MSE_NR6_test = numeric(K) # Nonparametirc model with 3 variables
MSE_NR3_train = numeric(K) # Nonparametirc model with 6 variables
MSE_NR3_test = numeric(K) # Nonparametirc model with 6 variables

library(mgcv)
library(leaps)

cal_MSE <- function(y_test, y_pred){
  n = length(y_test)
  return(sum((y_test-y_pred)^2)/n)
}

for (i in c(1:K)){
  BodyFat_train = res[[2]][[i]]
  BodyFat_test = res[[1]][[i]]
  
  # Best Subset
  Y = BodyFat_train$BODYFAT
  n = nrow(BodyFat_train)
  X = subset(BodyFat_train, select = -c(BODYFAT))
  model = regsubsets(y = Y, x = X, nvmax = dim(X)[2])
  ss = summary(model)
  
  rsq = ss$rsq
  rss = ss$rss
  adjr2 = ss$adjr2
  cp = ss$cp ## Equivalent to AIC
  bic = ss$bic
  
  ## Choose the BIC criteria
  best_num_variables = which.min(bic)
  lm_best_index = which(ss$which[best_num_variables,][2:15])
  fit_best = lm(Y~., data = cbind(Y,X[,lm_best_index]))
  # summary(fit_best)
  pred_y = predict(fit_best, newdata = BodyFat_test[,(lm_best_index+1)])
  MSE_lm_train[i] = rss[best_num_variables] / (n) # For easier to compare, set the denominator to be n.
  MSE_lm_test[i] = cal_MSE(BodyFat_test$BODYFAT, pred_y)
  
  fit_sm6 <- gam(BODYFAT~ 1 + s(AGE) + WEIGHT + s(ABDOMEN) + s(THIGH) + FOREARM + s(WRIST),data = BodyFat_train)
  # summary(fit_sm6)
  pred_y = predict(fit_sm6,newdata = BodyFat_test)
  MSE_NR6_train[i] = sum(fit_sm6$residuals)/n
  MSE_NR6_test[i] = cal_MSE(BodyFat_test$BODYFAT, pred_y)
  
  fit_sm3 <- gam(BODYFAT~ 1 + WEIGHT + s(ABDOMEN)+ s(WRIST),data = BodyFat_train)
  # summary(fit_sm3)
  pred_y = predict(fit_sm3,newdata = BodyFat_test)
  MSE_NR3_train[i] = sum(fit_sm3$residuals)/n
  MSE_NR3_test[i] = cal_MSE(BodyFat_test$BODYFAT, pred_y)
}



(result = data.frame(lm_test = mean(MSE_lm_test),lm_train = mean(MSE_lm_train),
                    NR3_test = mean(MSE_NR3_test), NR3_train = mean(MSE_NR3_train),
                    NR6_test = mean(MSE_NR6_test), NR6_train = mean(MSE_NR6_train)))
print(result)