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
appraise(fit_sm05,method = 'simulate')

draw(fit_sm05,scales = 'fixed')


plot(fit_sm05, pages = 1, scheme = 1, all.terms = TRUE, seWithMean = TRUE)

plot(fit_sm05,select = 1, shade = TRUE, scale = 0, seWithMean = TRUE)
plot(fit_sm05,select = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
plot(fit_sm05,select = 3, shade = TRUE, scale = 0, seWithMean = TRUE)

# Prediction ---------------------------------------
new_x = data.frame(data.frame(AGE = 22, WEIGHT = 154,
                              ABDOMEN= 85.2, THIGH = 59,
                              FOREARM = 27.4, WRIST = 17.1))

fit = predict(fit_sm05, newdata = new_x, se = TRUE)
print(fit)

upr = fit$fit + 1.96*fit$se.fit
dwn = fit$fit - 1.96*fit$se.fit

print(data.frame(fit = fit$fit, upr = upr, dwn = dwn))
