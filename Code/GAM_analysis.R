rm(list=ls())

# Read the data ----------------------------------------
setwd("D:/WISC/stat628/Module 2/Stat628Module2_Group9")
BodyFatData <- read.csv('../BodyFat.csv')

del_index = c(33, 39, 42, 48, 76, 96, 163, 172, 182, 216, 221)
BodyFat = BodyFatData[-del_index,]
BodyFat = subset(BodyFat, select = -c(IDNO,DENSITY))

library(mgcv)

# Variable Selection ----------------------------------
# m0
fit_sm0 <- mgcv::gam(BODYFAT~ 1 + s(AGE) + s(WEIGHT) + s(HEIGHT) + s(ADIPOSITY)
                      + s(NECK) + s(CHEST) + s(ABDOMEN) + s(HIP)
                      + s(THIGH) + s(KNEE) + s(ANKLE) + s(BICEPS)
                      + s(FOREARM) + s(WRIST)
                      ,data = BodyFat, method = 'REML', select = TRUE)

summary(fit_sm0)


## Select result£ºRemove ADIPOSITY/CHEST/KNEE/ANKLE
# m01,1
fit_sm01 <- mgcv::gam(BODYFAT~ 1 + s(AGE)  + s(WEIGHT) + s(HEIGHT)
                       + s(NECK)  + s(ABDOMEN) + s(HIP)
                       + s(THIGH) + s(BICEPS)
                       + s(FOREARM) + s(WRIST)
                       ,data = BodyFat, method = 'REML')
summary(fit_sm01)


# Remove Height
# m02
fit_sm02 <- mgcv::gam(BODYFAT~ 1 + AGE  + WEIGHT
                      + s(NECK)  + s(ABDOMEN)+ s(HIP)
                      + s(THIGH) + s(BICEPS)
                      + FOREARM + s(WRIST)
                      ,data = BodyFat, method = 'REML')
summary(fit_sm02)

# Remove HIP
# m03
fit_sm03 <- mgcv::gam(BODYFAT~ 1 + AGE  + WEIGHT
                      + s(NECK)  + s(ABDOMEN)
                      + s(THIGH) + s(BICEPS)
                      + FOREARM + s(WRIST)
                      ,data = BodyFat, method = 'REML')
summary(fit_sm03)


# m04
fit_sm04 <- mgcv::gam(BODYFAT~ 1 + AGE  + WEIGHT
                      + s(NECK)  + s(ABDOMEN)
                      + s(THIGH) 
                      + FOREARM + s(WRIST)
                      ,data = BodyFat, method = 'REML')
summary(fit_sm04)
AIC(fit_sm04)
# m05
fit_sm05 <- mgcv::gam(BODYFAT~ 1 + AGE  + WEIGHT
                      + s(ABDOMEN)
                      + s(THIGH) 
                      + FOREARM + s(WRIST)
                      ,data = BodyFat, method = 'REML')
summary(fit_sm05)
AIC(fit_sm0, fit_sm01, fit_sm02, fit_sm03, fit_sm04, fit_sm05)

# Diagnostic ---------------------------
gam.check(fit_sm05, rep = 500)
qq.gam(fit_sm05, rep = 500, level = .95)

library(gratia)
library(gridExtra)
appraise(fit_sm05,method = 'simulate')

par(mfrow = c(1,3))
draw(fit_sm05,scale = 'fixed',select = c(1,2,3))


plot(fit_sm05, pages = 1, scheme = 1, all.terms = TRUE, seWithMean = TRUE)

plot(fit_sm05,select = 1, shade = TRUE, scale = 0, seWithMean = TRUE)
plot(fit_sm05,select = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
plot(fit_sm05,select = 3, shade = TRUE, scale = 0, seWithMean = TRUE)

cor(cbind(BodyFat$AGE,BodyFat$WEIGHT,BodyFat$FOREARM,BodyFat$THIGH,BodyFat$ABDOMEN))

## Check adj R2 degree of freedom 
gamR2 <- function(gam){
  R2 <- 1-((sum(residuals(gam)^2))/
             (sum((gam$y - mean(gam$y))^2)))
  R2adj <- 1- ((1 - R2) * (length(gam$y) - 1)/ # Total df = n-1
                 (fit_sm05$df.residual)) # Residuals df = df.residual = trace(S)
  a <- data.frame(R2, R2adj)
  return(a)
}

gamR2(fit_sm05)

# Prediction ---------------------------------------
new_x = data.frame(AGE = 23, WEIGHT = 154,
                              ABDOMEN= 85.2, THIGH = 59,
                              FOREARM = 27.4, WRIST = 17.1)

fit = predict(fit_sm05, newdata = new_x, se = TRUE)
# print(fit)

## CI


upr = fit$fit + 2*fit$se.fit
dwn = fit$fit - 2*fit$se.fit


print(data.frame(fit = fit$fit, upr = upr, dwn = dwn))


## devtools::install_github("gavinsimpson/schoenberg")
# library("schoenberg")
# ci = confint(fit_sm05, parm ="ABDOMEN",type = 'confidence')

# predict(fit_sm05, newdata = data.frame(AGE = mean(BodyFat$AGE), WEIGHT = mean(BodyFat$WEIGHT),
#                                     ABDOMEN= 100.123, THIGH = mean(BodyFat$THIGH),
#                                     FOREARM = mean(BodyFat$FOREARM), WRIST = mean(BodyFat$FOREARM)), se=TRUE)
# library(gratia)
# fd = fderiv(fit_sm05)
# ci <- confint(fd, type = "confidence")
# ci <- cbind(ci, x = as.vector(fd[['eval']]))
# library("ggplot2")
# ggplot(ci, aes(x = x, y = est, group = term)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
#   geom_line() +
#   facet_wrap( ~ term)

## PI

# fit_gam = fit_sm05
# beta = coef(fit_gam)
# V = vcov(fit_gam)
# 
# num_beta_vecs <- 1000
# Cv <- chol(V)
# 
# nus <- rnorm(num_beta_vecs * length(beta))
# beta_sims <- beta + t(Cv) %*% matrix(nus, nrow = length(beta), ncol = num_beta_vecs)
# dim(beta_sims)
# 
# d_beta <- cbind(summary(fit_gam)$se, apply(beta_sims, 1, sd))
# 
# n_obs <- 100
# sim_idx <- sample.int(nrow(BodyFat), size = n_obs, replace = TRUE)
# sim_dat <- BodyFat[sim_idx, c("AGE", "WEIGHT", "FOREARM", "ABDOMEN", "THIGH", "WRIST")]
# dim(sim_dat)
# 
# covar_sim <- predict(fit_gam, newdata = sim_dat, type = "lpmatrix")
# linpred_sim <- covar_sim %*% beta_sims
# ## Link function: here we are identity function.
# invlink <- function(x) x
# exp_val_sim <- invlink(linpred_sim)
# 
# y_sim <- matrix(rnorm(n = prod(dim(exp_val_sim)), 
#                       mean = exp_val_sim, 
#                       sd = summary(fit_gam)$scale), 
#                 nrow = nrow(exp_val_sim), 
#                 ncol = ncol(exp_val_sim))
# dim(y_sim)
# pred_int_sim <- apply(y_sim, 1, quantile, prob = c(.05, 0.95))
# dim(pred_int_sim)
# 
# pred_int_sim


## Quantile for BodyFat
fit_all = predict(fit_sm05, se = TRUE)
quantile(fit_all$fit, probs = c(0,0.25,.5,.75,1))
quantile(BodyFat$BODYFAT, probs = c(0,0.25,.5,.75,1))


res = BodyFat$BODYFAT - fit_all$fit
n = length(res)
length(which(abs(res)>10))/n
length(which(abs(res)>5))/n
length(which(abs(res)>3))/n
