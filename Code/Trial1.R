rm(list=ls())

# Read the data ----------------------------------------
setwd("D:/WISC/stat628/Module 2/Stat628Module2_Group9")
BodyFatData <- read.csv('../BodyFat.csv')

del_index = c(33, 39, 42, 48, 76, 96, 163, 172, 182, 216, 221)
BodyFat = BodyFatData[-del_index,]
BodyFat = subset(BodyFat, select = -c(IDNO,DENSITY))

# Create K-fold ----------------------------

## I see many cv functions are written like this.
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

MSE_lm_train = numeric(K) # Simple linear model BIC method based
MSE_lm_test = numeric(K) # Simple linear model, BIC method based
MSE_lm3_train = numeric(K) # Simple linear model, 3 variables model based BODYFAT~1+WEIGHT+ABDOMEN+WRIST
MSE_lm3_test = numeric(K) # Simple linear model, 3 variables model based
MSE_NR6_train = numeric(K) # Nonparametirc model with 6 variables
MSE_NR6_test = numeric(K) # Nonparametirc model with 6 variables
MSE_NR8_train = numeric(K) # Nonparametirc model with 8 variables
MSE_NR8_test = numeric(K) # Nonparametirc model with 8 variables
MSE_lasso_train = numeric(K) # Lasso
MSE_lasso_test = numeric(K) # Lasso
MSE_NRVS_train = numeric(K) # Nonparametirc model with 8 variables but not all variables are significant
MSE_NRVS_test = numeric(K)
MSE_NRVS2_train = numeric(K) # Nonparametirc model with 5 variables. NR6 - FOREARM.
MSE_NRVS2_test = numeric(K)

library(mgcv)
library(leaps)
library(glmnet)

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
  
  ## Choose the BIC criteria method based
  best_num_variables = which.min(bic)
  lm_best_index = which(ss$which[best_num_variables,][2:15])
  fit_best = lm(Y~., data = cbind(Y,X[,lm_best_index]))
  print(fit_best$rank)
  
  ## 3 variables model based lm
  fit_lm3 = lm(BODYFAT~1+WEIGHT+ABDOMEN+WRIST, data = BodyFat_train)
  MSE_lm3_train[i] = mean(residuals(fit_lm3)^2)
  MSE_lm3_test[i] = mean((predict(fit_lm3, newdata = BodyFat_test) 
                     - BodyFat_test$BODYFAT)^2)
  
  
  ## Lasso
  fit_lasso <- glmnet(x = as.matrix(BodyFat_train[, 2:ncol(BodyFat_train)]), y = BodyFat_train[, 1])
  fit_lasso_cv <- cv.glmnet(x = as.matrix(BodyFat_train[, 2:ncol(BodyFat_train)]), y = BodyFat_train[, 1])
  bestlam1 = fit_lasso_cv$lambda.min
  # bestlam2 = fit_lasso_cv$lambda.1se
  pred_lasso1 <- predict(fit_lasso, s = bestlam1, newx = as.matrix(BodyFat_test[,2:ncol(BodyFat_train)]))
  # pred_lasso2 <- predict(fit_lasso, s = bestlam2, newx = as.matrix(BodyFat_test[,2:ncol(BodyFat_train)]))
  MSE_lasso_test[i]<- mean((BodyFat_test$BODYFAT - pred_lasso1) ** 2)
  MSE_lasso_train[i] <- mean((BodyFat_train$BODYFAT - predict(fit_lasso, s = bestlam1, newx = as.matrix(BodyFat_train[,2:ncol(BodyFat_train)])))^2)
  # RMSE_lasso2 <- sqrt(mean((BodyFat_test$BODYFAT - pred_lasso2) ** 2))
  # coeff_lasso1 <- predict(fit_lasso, s = bestlam1, type = 'coefficient')
  
  
  # summary(fit_best)
  pred_y = predict(fit_best, newdata = BodyFat_test[,(lm_best_index+1)])
  MSE_lm_train[i] = rss[best_num_variables] / (n) # For easier to compare, set the denominator to be n.
  MSE_lm_test[i] = cal_MSE(BodyFat_test$BODYFAT, pred_y)
  
  fit_sm6 <- mgcv::gam(BODYFAT~ 1 + AGE + WEIGHT
                       + s(ABDOMEN)
                       + s(THIGH)
                       + FOREARM + s(WRIST)
                       ,data = BodyFat_train, method = 'REML')
  # summary(fit_sm6)
  pred_y = predict(fit_sm6,newdata = BodyFat_test)
  MSE_NR6_train[i] = mean(residuals(fit_sm6)^2)
  MSE_NR6_test[i] = cal_MSE(BodyFat_test$BODYFAT, pred_y)
  
  
  fit_sm08 <- mgcv::gam(BODYFAT~ 1 + AGE  + WEIGHT
                             + s(NECK)  + s(ABDOMEN)
                             + s(THIGH) + s(BICEPS)
                             + FOREARM + s(WRIST)
                             ,data = BodyFat_train, method = 'REML')
  # summary(fit_sm08)
  pred_y = predict(fit_sm08,newdata = BodyFat_test)
  MSE_NR8_train[i] = mean(residuals(fit_sm08)^2)
  MSE_NR8_test[i] = cal_MSE(BodyFat_test$BODYFAT, pred_y)
  
  
  # fit_sm_nx_lm <- mgcv::gam(BODYFAT~ 1 + AGE + WEIGHT
  #                           + s(ABDOMEN)
  #                           + s(THIGH)
  #                           + s(WRIST)
  #                           ,data = BodyFat_train, method = 'REML')
  # # summary(fit_sm_nx_lm)
  # pred_y = predict(fit_sm_nx_lm,newdata = BodyFat_test)
  # MSE_NRVS_train[i] = mean(residuals(fit_sm_nx_lm)^2)
  # MSE_NRVS_test[i] = cal_MSE(BodyFat_test$BODYFAT, pred_y)
  # 
  # fit_sm_nx_lm2 <- mgcv::gam(BODYFAT~ 1 + AGE  + WEIGHT
  #                            + s(NECK)  + s(ABDOMEN)
  #                            + s(THIGH) + s(BICEPS)
  #                            + FOREARM + s(WRIST)
  #                            ,data = BodyFat_train, method = 'REML')
  # summary(fit_sm_nx_lm2)
  # pred_y = predict(fit_sm_nx_lm2,newdata = BodyFat_test)
  # MSE_NRVS2_train[i] = mean(residuals(fit_sm_nx_lm2)^2)
  # MSE_NRVS2_test[i] = cal_MSE(BodyFat_test$BODYFAT, pred_y)
}


(result = data.frame(lm_test = mean(MSE_lm_test),lm_train = mean(MSE_lm_train),
                     lm3_test = mean(MSE_lm3_test), lm3_train = mean(MSE_lm3_train),
                     lasso_test = mean(MSE_lasso_test), lasso_train = mean(MSE_lasso_train),
                    NR6_test = mean(MSE_NR6_test), NR6_train = mean(MSE_NR6_train),
                    NR8_test = mean(MSE_NR8_test), NR8_train = mean(MSE_NR8_train)
                    # NR_VS_test = mean(MSE_NRVS_test), MSE_NRVS_train = mean(MSE_NRVS_train),
                    # NR_VS2_test = mean(MSE_NRVS2_test), MSE_NRVS2_train = mean(MSE_NRVS2_train)
))
print(result)

(result_rmse = data.frame(lm_test = sqrt(mean(MSE_lm_test)),lm_train = sqrt(mean(MSE_lm_train)),
                          lm3_test = sqrt(mean(MSE_lm3_test)), lm3_train = sqrt(mean(MSE_lm3_train)),
                          lasso_test = sqrt(mean(MSE_lasso_test)), lasso_train = sqrt(mean(MSE_lasso_train)),
                     NR6_test = sqrt(mean(MSE_NR6_test)), NR6_train = sqrt(mean(MSE_NR6_train)),
                     NR8_test = sqrt(mean(MSE_NR8_test)), NR8_train = sqrt(mean(MSE_NR8_train))
                     # NR_VS_test = sqrt(mean(MSE_NRVS_test)), MSE_NRVS_train = sqrt(mean(MSE_NRVS_train)),
                     # NR_VS2_test = sqrt(mean(MSE_NRVS2_test)), MSE_NRVS2_train = sqrt(mean(MSE_NRVS2_train))
  ))


print(result_rmse)

