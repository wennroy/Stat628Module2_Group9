getwd()
setwd("E:/wisc/628/module2")

BodyFatData <- read.csv('BodyFat.csv')

##Data Clean##
##head(BodyFatData)

# summary of the data, there are extreme values of bodyfat percentage which is abnormal
summary(BodyFatData[, -1])

# Body fat percentage extreme values
bodyfatper <- which(BodyFatData$BODYFAT<2 | BodyFatData$BODYFAT >40)
BodyFatData[bodyfatper, ]
# 172 - too low 
# 216 - too large
# 182 - 0

#WEIGHT
BodyFatData[which(BodyFatData$WEIGHT==max(BodyFatData$WEIGHT)),]
# 39 - too large

#HEIGHT
BodyFatData[which(BodyFatData$HEIGHT==min(BodyFatData$HEIGHT)),]
# 42 - too small
remove <- c(182,172,216,39,42)
BodyFatData1 <- BodyFatData[-remove,]

#Check siri's equation
# calculate bodyfat using siri's equation
bodyfat_siri <- 495/BodyFatData1$DENSITY - 450 
summary(bodyfat_siri-BodyFatData1$BODYFAT)
plot(BodyFatData1$BODYFAT~bodyfat_siri)
fit <- lm(BodyFatData1$BODYFAT~bodyfat_siri)
par(mfrow = c(1,2))
plot(fit, which = c(1,2)) 
#76,48,96

#Check BMI
# BMI's equation
BMI <- (703*BodyFatData1$WEIGHT)/(BodyFatData1$HEIGHT)^2
summary(BMI-BodyFatData1$ADIPOSITY)
plot(BodyFatData1$ADIPOSITY~BMI)
fit <- lm(BodyFatData1$ADIPOSITY~BMI)
par(mfrow = c(1,2))
plot(fit, which = c(1,2))
#163,221






