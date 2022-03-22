library(readxl)
#MARITIME <- read_excel("Downloads/MARITIME.xlsx")
GDP <- read_excel("Downloads/GDP.xlsx")
MGDP  <- GDP$MaritimeGDP 
TGDP  <- GDP$TotalGDP
LINEAR  <-data.frame(MGDP, TGDP)
linearMod <- lm(TGDP ~ MGDP, data=LINEAR)
print(linearMod)
summary(linearMod)
layout(matrix(c(1,1,2,3),2,2,byrow=T))
#Day x Residuals Plot
plot(linearMod$resid~LINEAR$MGDP[order(LINEAR$MGDP)],
     main="MaritimeGDP x Residuals\nfor Simple Regression",
     xlab="MaritimeGDP", ylab="Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(linearMod$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(linearMod$resid)
qqline(linearMod$resid)

library(fBasics)
jarqueberaTest(linearMod$resid) #Test residuals for normality
#Null Hypothesis: Skewness and Kurtosis are equal to zero
#Residuals X-squared: 4.1228 p Value: 0.1273
#With a p value of 0.1273, we fail to reject the null hypothesis that the skewness and kurtosis of residuals are statistically equal to zero.
library(lmtest) #dwtest
dwtest(linearMod) #Test for independence of residuals
#Null Hypothesis: Errors are serially UNcorrelated
# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(LINEAR), 0.8*nrow(LINEAR))  # row indices for training data
trainingData <- LINEAR[trainingRowIndex, ]  # model training data
testData  <- LINEAR[-trainingRowIndex, ]   # test data
# Build the model on training data
lmMod <- lm(TGDP ~ MGDP, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
pred1a.test <- predict(linearMod, newdata=testData )
data.frame(testData, pred1a.test, distPred)
summary (lmMod)
summary(pred1a.test)
AIC (lmMod)
actuals_preds <- data.frame(cbind(actuals=testData, predicteds=distPred))  # make actuals_predicteds dataframe.
Second.test <- testData$TGDP
par(mfrow=c(1,2))
plot(distPred, Second.test)
abline(a=0, b=1, lty=2)
plot(distPred, pred1a.test)
abline(a=0, b=1, lty=2)
cor.test <- cor(pred1a.test, Second.test)
R2.test <- cor.test^2
R2.test
alpha <- 0.05
conf.Second <- predict(linearMod, data = LINEAR, interval="confidence", level=1-alpha) 
head(conf.Second)
pred.Second <- predict(linearMod, data = LINEAR, interval="prediction", level=1-alpha) 
head(pred.Second)
library(ggplot2)
theme_set(theme_bw())
pl <- ggplot(LINEAR) + geom_point(aes(x=MGDP, y=TGDP), size=2, colour="#993399") + 
  xlab("MaritimeGDP") + ylab("TotalGDP")  
print(pl)
LINEAR[c("fit","lwr.conf", "upr.conf")] <- conf.Second
LINEAR[c("lwr.pred", "upr.pred")] <- pred.Second[,2:3]
pl +
  geom_ribbon(data=LINEAR, aes(x=MGDP, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") + 
  geom_ribbon(data=LINEAR, aes(x=MGDP, ymin=lwr.conf, ymax=upr.conf), alpha=0.2, inherit.aes=F, fill="#339900") +  
  geom_line(data=LINEAR, aes(x=MGDP, y=fit), colour="#339900", size=1)
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
# Min-Max Accuracy Calculation
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# MAPE Calculation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
library(DAAG)
cvResults <- suppressWarnings(CVlm(data=LINEAR, form.lm=TGDP ~ MGDP, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=TRUE));  # performs the CV
attr(cvResults, 'ms')   
scatter.smooth(x=LINEAR$MGDP, y=LINEAR$TGDP, main="TGDP ~ MGDP")  # scatterplot 
plot(linearMod$residuals, pch = 16, col = "red")
ggplot(LINEAR, aes(x = MGDP, y = TGDP)) +
  geom_point() +
  stat_smooth()
cor(LINEAR$MGDP, LINEAR$TGDP)
ggplot(LINEAR, aes(MGDP, TGDP)) +
  geom_point() +
  stat_smooth(method = lm)
confint(linearMod)
# RESIDUAL STANDARD ERROR
sigma(linearMod)*100/mean(LINEAR$TGDP)
par(mfrow=c(2,2))
plot(linearMod)
par(mfrow=c(1,1))

library(e1071)
modelsvm = svm(TGDP ~ MGDP, data=LINEAR)
summary(modelsvm)
jarqueberaTest(modelsvm$resid)
predYsvm = predict(modelsvm, data=LINEAR)
plot(LINEAR)
points(LINEAR$MGDP, predYsvm, col = "red", pch=16)
##Calculate parameters of the SVR model
library(hydroGOF)
#Find value of W
W = t(modelsvm$coefs) %*% modelsvm$SV

#Find value of b
b = modelsvm$rho
summary(predYsvm)
## RMSE for SVR Model

#Calculate RMSE 
RMSEsvm=rmse(predYsvm,LINEAR$TGDP)
## Tuning SVR model by varying values of maximum allowable error and cost parameter

#Tune the SVM model
OptModelsvm=tune(svm, TGDP ~ MGDP, data=LINEAR,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
# YOU CAN ALSO TRY seq(0,0.2,0.01)
#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)
## Select the best model out of 1100 trained models and compute RMSE

#Find out the best model
BstModel=OptModelsvm$best.model

#Predict Y using best model
PredYBst=predict(BstModel,LINEAR)

#Calculate RMSE of the best model 
RMSEBst=rmse(PredYBst,LINEAR$TGDP)
##Calculate parameters of the Best SVR model

#Find value of W

#Find value of b
b = BstModel$rho
## Plotting SVR Model and Tuned Model in same plot
#Actual data (black), SVR model (blue), tuned SVR model (red).
plot(LINEAR, pch=16)
points(LINEAR$MGDP, predYsvm, col = "blue", pch=3)
points(LINEAR$MGDP, PredYBst, col = "red", pch=4)
points(LINEAR$MGDP, predYsvm, col = "blue", pch=3, type="l")
points(LINEAR$MGDP, PredYBst, col = "red", pch=4, type="l")

library(caret)
trainingRowIndex <- sample(1:nrow(LINEAR), 0.8*nrow(LINEAR))  # row indices for training data
train <- LINEAR[trainingRowIndex, ]  # model training data
test  <- LINEAR[-trainingRowIndex, ]   # test data
model_reg = svm(TGDP ~ MGDP, data=train)
jarqueberaTest(model_reg$resid)
print(model_reg)
pred = predict(model_reg, test)

x = 1:length(test$TGDP)
plot(x, test$Funds, pch=18, col="red")
lines(x, pred, lwd="1", col="blue")

mae = MAE(test$TGDP, pred)
rmse = RMSE(test$TGDP, pred)
r2 = R2(test$TGDP, pred, form = "traditional")

cat(" MAE:", mae, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)
cm = data.frame(test , pred)

library(nlme)
library(mgcv)

install.packages("scam")
library("scam")
library("ggplot2")
install.packages("cowplot")
library("cowplot")
library("tidyr")
install.packages("devtools")
devtools::install_github("gavinsimpson/gratia")
library("gratia")
install.packages("ppgam")
library("ppgam")
#Kuwait <-
#Here I can remove the ,bs="cr", I can remove k = 12, default K is 10
# FOR method, any of the following c("GCV.Cp", "GACV.Cp", "REML", "P-REML", "ML", "P-ML")
#family=poisson OR negative.binomial(1) family=Gamma(link="log"),, family="gaussian"
GAM <-gam(LINEAR$TGDP ~ s(LINEAR$MGDP,bs="cr", k = 12),
          family="gaussian",method="ML") 
gam.check(GAM)
summary(GAM)
#WE CAN APPRAISE WITHOUT method = "simulate"
appraise(GAM, method = "simulate")
plot(GAM,residuals=TRUE)
plot(GAM,too.far=0.15)
draw(GAM)
predict(GAM)
pv <- predict(GAM,se=TRUE)
pv$fit
predict(GAM,type="response")
pv$se
pv1 <- predict(GAM,type="response",se=TRUE)
pv1$se
#MIXED MODEL
GAM3<-gamm(LINEAR$TGDP ~ s(LINEAR$MGDP,bs="cr", k = 12),
           correlation = NULL,method="REML") 
m.same<-gam(LINEAR$TGDP ~ s(LINEAR$MGDP,bs="cr", k = 12),
            family=Gamma(link=log),method="ML")
gam.check(m.same)
summary(m.same)
appraise(m.same)
draw(m.same)
predict.gam(m.same)
plot.gam(m.same)


summary(GAM3)
draw(GAM3)
print(GAM3)

model <- lm(LINEAR$TGDP ~ poly(LINEAR$MGDP, 4, raw=TRUE))
summary(model)
plot(model)
M <- modelM <- model.matrix(model)
head(M)
print(coef(model))
poly1.ortho <- poly(MGDP, degree=1)
lm1.ortho <- lm(TGDP ~ poly1.ortho)
M1 <- model.matrix(lm1.ortho)
head(M1)
t(M1)%*%M1
coef(lm1.ortho)
poly2.ortho_ <- poly(MGDP, degree=2)
lm2.ortho <- lm(TGDP ~ poly2.ortho_)
M2 <- model.matrix(lm2.ortho)
head(M2)
t(M2)%*%M2
summary(lm2.ortho)
confint(lm2.ortho)
summary(lm1.ortho)$coefficients
summary(lm2.ortho)$coefficients
#anova(linearMod,  model)
pred = predict(model,data=LINEAR)

plot(x=LINEAR$MGDP, y=LINEAR$TGDP, pch=20, col="grey")

lines(LINEAR$MGDP, predict(lm(TGDP ~ MGDP, data=LINEAR)), type="l", col="orange1", lwd=2)
lines(LINEAR$MGDP, predict(lm(TGDP~I(MGDP^2), data=LINEAR)), type="l", col="pink1", lwd=2)
lines(LINEAR$MGDP, predict(lm(TGDP~I(MGDP^3), data=LINEAR)), type="l", col="yellow2", lwd=2)
lines(LINEAR$MGDP, predict(lm(TGDP~poly(MGDP,3)+poly(MGDP,2), data=LINEAR)), type="l", col="blue", lwd=2)

legend("topleft", 
       legend = c("y~x,  - linear","y~x^2", "y~x^3", "y~x^3+x^2"), 
       col = c("orange","pink","yellow","blue"),
       lty = 1, lwd=3
) 
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(LINEAR), 0.6*nrow(LINEAR))  # row indices for training data
trainingData <- LINEAR[trainingRowIndex, ]  # model training data
testData  <- LINEAR[-trainingRowIndex, ]   # test data
# Build the model on training data
lmMod <- lm(testData$TGDP ~ poly(testData$MGDP, 4, raw=TRUE))
distPred <- predict(lmMod, testData)  # predict distance

data.frame(testData, distPred)
summary (lmMod)
AIC (lmMod)
actuals_preds <- data.frame(cbind(actuals=testData, predicteds=distPred))  # make actuals_predicteds dataframe.
Second.test <- testData$TGDP
par(mfrow=c(1,2))
plot(distPred, Second.test)
abline(a=0, b=1, lty=2)

alpha <- 0.05
conf.Second <- predict(lmMod, data = LINEAR, interval="confidence", level=1-alpha) 
head(conf.Second)
pred.Second <- predict(lmMod, data = LINEAR, interval="prediction", level=1-alpha) 
head(pred.Second)

correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
# Min-Max Accuracy Calculation
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# MAPE Calculation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  



confint(lmMod)
# RESIDUAL STANDARD ERROR
sigma(lmMod)*100/mean(LINEAR$TGDP)
par(mfrow=c(2,2))
plot(lmMod)
par(mfrow=c(1,1))


