rm(list = ls())
library(MASS)
library(caret)
library(glmnet)
library(ISLR)
library(car)
library(mgcv)
library(leaps)
# set.seed(1234)

BodyFatData <- read.csv('BodyFat.csv')

# 数据清理
BodyFat <- BodyFatData[-c(33, 39, 42, 48, 76, 96, 163, 172, 182, 216, 221), ]
attach(BodyFat)

# cv
K = 10
k_fold_data <- function(K = 10, data){
  n = nrow(data)
  f <- ceiling(n/K)
  s <- sample(rep(1L:K, f), n)
  #s <- c(25, 24, 24, 24, 24, 25, 25, 22, 23, 25)
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

# 画出BODYFAT和BODYFAT的关系图，离群点都删???
# plot(BODYFAT, BODYFAT, which = c(1, 2))

n = dim(BodyFat)[1]
#plot(BodyFat[, 4:ncol(BodyFat)])
# corrplot::corrplot(cor(dat1[,1:4]),diag = FALSE)

rmse_df <- data.frame()

for (i in 1:10)
{
  BodyFat_train = res[[2]][[i]]
  BodyFat_test = res[[1]][[i]]
  
  fit <- lm(BODYFAT ~ .-IDNO-DENSITY, data = BodyFat_train)
  RMSE_fit_train <- sqrt(mean((BodyFat_train$BODYFAT - predict(fit,newdata = BodyFat_train))^2))
  RMSE_fit_test <- sqrt(mean((BodyFat_test$BODYFAT - predict(fit,newdata = BodyFat_test))^2))
  
  
  fit_BIC <- step(fit, k = log(n), trace = F)
  RMSE_fit_BIC_train <- sqrt(mean((BodyFat_train$BODYFAT - predict(fit_BIC,newdata = BodyFat_train))^2))
  RMSE_fit_BIC_test <- sqrt(mean((BodyFat_test$BODYFAT - predict(fit_BIC,newdata = BodyFat_test))^2))
  
  #fit_sm6 <- gam(BODYFAT~ 1 + s(AGE) + WEIGHT + s(ABDOMEN) + s(THIGH) + FOREARM + s(WRIST),data = BodyFat_train)
  #pred_sm6 = predict(fit_sm6,newdata = BodyFat_test)
  #RMSE_spline_6 <- sqrt(mean((BodyFat_test$BODYFAT - pred_sm6)^2))
  
  #fit_sm3 <- gam(BODYFAT~ 1 + WEIGHT + s(ABDOMEN)+ s(WRIST),data = BodyFat_train)
  #pred_sm3 = predict(fit_sm3,newdata = BodyFat_test)
  #RMSE_spline_3 <- sqrt(mean((BodyFat_test$BODYFAT - pred_sm3)^2))
  
  fit_sm03 <- mgcv::gam(BODYFAT~ 1 + AGE  + WEIGHT
                        + s(NECK)  + s(ABDOMEN)
                        + s(THIGH) + s(BICEPS)
                        + FOREARM + s(WRIST)
                        ,data = BodyFat_train, method = 'REML')
  pred_sm03_train = predict(fit_sm03,newdata = BodyFat_train)
  pred_sm03_test = predict(fit_sm03,newdata = BodyFat_test)
  RMSE_spline_03_train <- sqrt(mean((BodyFat_train$BODYFAT - pred_sm03_train)^2))
  RMSE_spline_03_test <- sqrt(mean((BodyFat_test$BODYFAT - pred_sm03_test)^2))
  
  fit_sm_nx_lm <- mgcv::gam(BODYFAT~ 1 + AGE + WEIGHT
                            + s(ABDOMEN)
                            + s(THIGH)
                            + FOREARM + s(WRIST)
                            ,data = BodyFat_train, method = 'REML')
  pred_nx_train = predict(fit_sm_nx_lm,newdata = BodyFat_train)
  pred_nx_test = predict(fit_sm_nx_lm,newdata = BodyFat_test)
  RMSE_spline_nx_train <- sqrt(mean((BodyFat_train$BODYFAT - pred_nx_train)^2))
  RMSE_spline_nx_test <- sqrt(mean((BodyFat_test$BODYFAT - pred_nx_test)^2))
  
  fit_lasso <- glmnet(x = as.matrix(BodyFat_train[, 4:ncol(BodyFat_train)]), y = BodyFat_train[, 2])
  fit_lasso_cv <- cv.glmnet(x = as.matrix(BodyFat_train[, 4:ncol(BodyFat_train)]), y = BodyFat_train[, 2])
  bestlam1 = fit_lasso_cv$lambda.min
  #bestlam2 = fit_lasso_cv$lambda.1se
  pred_lasso1_train <- predict(fit_lasso, s = bestlam1, newx = as.matrix(BodyFat_train[,4:ncol(BodyFat_train)]), interval = 'prediction')
  pred_lasso1_test <- predict(fit_lasso, s = bestlam1, newx = as.matrix(BodyFat_test[,4:ncol(BodyFat_test)]), interval = 'prediction')
  #pred_lasso2 <- predict(fit_lasso, s = bestlam2, newx = as.matrix(BodyFat_test[,4:ncol(BodyFat_train)]), interval = 'prediction')
  RMSE_lasso1_train <- sqrt(mean((BodyFat_train$BODYFAT - pred_lasso1_train) ** 2))
  RMSE_lasso1_test <- sqrt(mean((BodyFat_test$BODYFAT - pred_lasso1_test) ** 2))
  #RMSE_lasso2 <- sqrt(mean((BodyFat_test$BODYFAT - pred_lasso2) ** 2))
  coeff_lasso1 <- predict(fit_lasso, s = bestlam1, type = 'coefficient')
  
  rmse_df[i, "linear_train"] <- RMSE_fit_train
  rmse_df[i, "linear_test"] <- RMSE_fit_test
  rmse_df[i, "stepwise_train"] <- RMSE_fit_BIC_train
  rmse_df[i, "stepwise_test"] <- RMSE_fit_BIC_test
  #rmse_df[i, "spline_6"] <- RMSE_spline_6
  #rmse_df[i, "spline_3"] <- RMSE_spline_3
  rmse_df[i, "spline_03_train"] <- RMSE_spline_03_train
  rmse_df[i, "spline_03_test"] <- RMSE_spline_03_test
  rmse_df[i, "spline_nx_train"] <- RMSE_spline_nx_train
  rmse_df[i, "spline_nx_test"] <- RMSE_spline_nx_test
  rmse_df[i, "lasso1_train"] <- RMSE_lasso1_train
  rmse_df[i, "lasso1_test"] <- RMSE_lasso1_test
  #rmse_df[i, "lasso2"] <- RMSE_lasso2
}

summary(fit_BIC)
coeff_lasso1
rmse_df

mean(rmse_df$linear_train)
mean(rmse_df$linear_test)
mean(rmse_df$stepwise_train)
mean(rmse_df$stepwise_test)
#mean(rmse_df$spline_6)
#mean(rmse_df$spline_3)
mean(rmse_df$spline_03_train)
mean(rmse_df$spline_03_test)
mean(rmse_df$spline_nx_train)
mean(rmse_df$spline_nx_test)
mean(rmse_df$lasso1_train)
mean(rmse_df$lasso1_test)
#mean(rmse_df$lasso2)



# 1. 普通回???



fit <- lm(BODYFAT ~ .-IDNO-DENSITY, data = BodyFat_train)
summary(fit)
RMSE_fit <- sqrt(mean((BodyFat_test$BODYFAT - predict(fit,newdata = BodyFat_test))^2))
RMSE_fit

# 根据观察到的结果进行相关性检验（做齐???
cor.test(KNEE, ANKLE, method = "pearson")
cor.test(KNEE, BICEPS, method = "kendall")
cor.test(ANKLE, BICEPS, method = "spearman")

# 多重共线性检???
vif(fit)

# 变量选择：逐步回归 + BIC


fit_BIC <- step(fit, k = log(n), trace = F)
summary(fit_BIC)
RMSE_fit_BIC <- sqrt(mean((BodyFat_test$BODYFAT - predict(fit_BIC,newdata = BodyFat_test))^2))
RMSE_fit_BIC

# 变量选择：逐步回归 + AIC
# fit_AIC <- step(fit, k = 2, trace = F)
# summary(fit_AIC)
# vif(fit_AIC)

# 变量选择：岭回归
index <- sample(1:nrow(BodyFat), size = 0.8 * nrow(BodyFat))
train <- BodyFat[index, ]
test <- BodyFat[-index, ]

fit_ridge <- glmnet(x = as.matrix(train[, 4:ncol(train)]), y = train[, 2], alpha = 0)
par(mfrow = c(1,1))
plot(fit_ridge, xvar = 'lambda', label = T)

fit_ridge_cv <- cv.glmnet(x = as.matrix(train[, 4:ncol(train)]), y = train[, 2], alpha = 0)
best_lambda_ridge <- fit_ridge_cv$lambda.min
# best_lambda_ridge

coeff_ridge <- predict(fit_ridge, s = best_lambda_ridge, type = 'coefficients')
coeff_ridge

pred_ridge <- predict(fit_ridge, s = best_lambda_ridge, newx = as.matrix(test[, 4:ncol(test)]))
RMSE_ridge <- sqrt(mean((test$DENSITY - pred_ridge) ** 2))
RMSE_ridge

# 变量选择：LASSO回归
fit_lasso <- glmnet(x = as.matrix(train[, 4:ncol(train)]), y = train[, 2])
plot(fit_lasso, svar = 'lambda', label = T)

fit_lasso_cv <- cv.glmnet(x = as.matrix(train[, 4:ncol(train)]), y = train[, 2])
# lambda.min 误差最???, lambda.1se 性能优良,自变量个数最???
bestlam1 = fit_lasso_cv$lambda.min
bestlam2 = fit_lasso_cv$lambda.1se


coeff_lasso1 <- predict(fit_lasso, s = bestlam1, type = 'coefficient')
coeff_lasso2 <- predict(fit_lasso, s = bestlam2, type = 'coefficient')
coeff_lasso1
coeff_lasso2

pred_lasso1 <- predict(fit_lasso, s = bestlam1, newx = as.matrix(test[,4:ncol(train)]))
RMSE_lasso1 <- sqrt(mean((test$BODYFAT - pred_lasso1) ** 2))
RMSE_lasso1

pred_lasso2 <- predict(fit_lasso, s = bestlam2, newx = as.matrix(test[,4:ncol(train)]), interval = 'prediction')
RMSE_lasso2 <- sqrt(mean((test$BODYFAT - pred_lasso2) ** 2))
RMSE_lasso2
