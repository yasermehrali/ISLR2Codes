
# Lab: Classification Methods -----------------


## The Stock Market Data ----------------------

###
library(ISLR2)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
###
cor(Smarket)
cor(Smarket[, -9])
###
attach(Smarket)
plot(Volume)

## Logistic Regression ------------------------

###
glm.fits <- glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket, family = binomial
  )
summary(glm.fits)
###
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[, 4]
###
glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(Direction)
###
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"
###
table(glm.pred, Direction)
(507 + 145) / 1250
mean(glm.pred == Direction)
###
train <- (Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]
###
glm.fits <- glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket, family = binomial, subset = train
  )
glm.probs <- predict(glm.fits, Smarket.2005,
    type = "response")
###
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)
###
glm.fits <- glm(Direction ~ Lag1 + Lag2, data = Smarket,
    family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket.2005,
    type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
106 / (106 + 76)
###
predict(glm.fits,
    newdata =
      data.frame(Lag1 = c(1.2, 1.5),  Lag2 = c(1.1, -0.8)),
    type = "response"
  )

### Methods for Logistic Regression ------------

library(ggcorrplot)
ggcorrplot(cor(Smarket[,-9]))
ggcorrplot(cor(Smarket[,-9]),hc.order=T)
library(GGally)
ggpairs(Smarket)
anova(glm.fits)
library(broom)
tidy(glm.fits)
tidy(glm.fits,conf.int=T)
tidy(glm.fits,exponentiate=T,conf.int=T)
glance(glm.fits)
augment(glm.fits,type.predict="link")
augment(glm.fits,type.predict="response")
par(mfrow=c(2,2))
plot(glm.fits)
library(ggfortify)
autoplot(glm.fits)
library(plotmo)
plotmo(glm.fits)
par(mfrow=c(1,1))
# confusionMatrix
library(caret)
# Both arguments factor with same levels
# ?caret::confusionMatrix
conf=confusionMatrix(data=as.factor(glm.pred), reference=Direction.2005)
conf
conf$table
conf$positive
conf$overall
conf$byClass
class(conf)
library(broom)
# ?tidy.confusionMatrix
tidy(conf)
tidy(conf,by_class = F)

library(ROCR)
# Both arguments same class
pred <- prediction(predictions=glm.probs, labels=Direction.2005)
pred

perf <- performance(pred,"tpr")
perf
plot(perf)
library(ggfortify)
autoplot(perf)

perf <- performance(pred,"fpr")
perf
plot(perf)
autoplot(perf)

perf <- performance(pred,"prec")
perf
plot(perf)
autoplot(perf)

perf <- performance(pred,"rec")
perf
plot(perf)
autoplot(perf)

perf <- performance(pred,"sens")
perf
plot(perf)
autoplot(perf)

perf <- performance(pred,"spec")
perf
plot(perf)
autoplot(perf)

perf <- performance(pred,"lift")
perf
plot(perf)
autoplot(perf)

perf <- performance(pred,"rpp")
perf
plot(perf)
autoplot(perf)

# ROC curves
perf <- performance(pred,"tpr","fpr")
perf
plot(perf, colorize=T)
library(ggfortify)
library(viridis)
autoplot(perf)+ scale_color_viridis()
# precision/recall curve (x-axis: recall, y-axis: precision)
perf <- performance(pred, "prec", "rec")
perf
plot(perf, colorize=T)
autoplot(perf)+ scale_color_viridis()
# sensitivity/specificity curve (x-axis: specificity, y-axis: sensitivity)
perf <- performance(pred, "sens", "spec")
perf
plot(perf, colorize=T)
autoplot(perf)+ scale_color_viridis()
# Lift charts
perf <- performance(pred, "lift", "rpp")
perf
plot(perf, colorize=T)
autoplot(perf)+ scale_color_viridis()

library(pROC)
roc_obj=roc(response=Direction.2005, predictor=glm.probs)
roc_obj
plot(roc_obj)
pROC::auc(roc_obj)
ModelMetrics::auc(glm.fits)

## Linear Discriminant Analysis ---------------

###
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket,
    subset = train)
lda.fit
plot(lda.fit)
###
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
###
lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
###
sum(lda.pred$posterior[, 1] >= .5)
sum(lda.pred$posterior[, 1] < .5)
###
lda.pred$posterior[1:20, 1]
lda.class[1:20]
###
sum(lda.pred$posterior[, 1] > .9)

### Methods for Linear Discriminant Analysis ----

# confusionMatrix
library(caret)
# Both arguments factor with same levels
# ?caret::confusionMatrix
conf=confusionMatrix(data=lda.pred$class, reference=Direction.2005)
conf

library(ROCR)
# Both arguments same class
pred <- prediction(predictions=lda.pred$posterior[, 1], labels=Direction.2005)
pred

# ROC curves
perf <- performance(pred,"tpr","fpr")
perf
plot(perf, colorize=T)
library(ggfortify)
library(viridis)
autoplot(perf)+ scale_color_viridis()

## Quadratic Discriminant Analysis -----------

###
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket,
    subset = train)
qda.fit
###
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

### Methods for Quadratic Discriminant Analysis ----

# confusionMatrix
qda.pred <- predict(qda.fit, Smarket.2005)
library(caret)
# Both arguments factor with same levels
# ?caret::confusionMatrix
conf=confusionMatrix(data=qda.pred$class, reference=Direction.2005)
conf

# ROC curves
library(ROCR)
# Both arguments same class
pred <- prediction(predictions=qda.pred$posterior[, 1], labels=Direction.2005)
pred
perf <- performance(pred,"tpr","fpr")
perf
plot(perf, colorize=T)
library(ggfortify)
library(viridis)
autoplot(perf)+ scale_color_viridis()

## Naive Bayes --------------------------------

###
library(e1071)
nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2, data = Smarket,
    subset = train)
nb.fit
###
mean(Lag1[train][Direction[train] == "Down"])
sd(Lag1[train][Direction[train] == "Down"])
###
nb.class <- predict(nb.fit, Smarket.2005)
table(nb.class, Direction.2005)
mean(nb.class == Direction.2005)
###
nb.preds <- predict(nb.fit, Smarket.2005, type = "raw")
nb.preds[1:5, ]

### Methods for Naive Bayes -------------------

# confusionMatrix
library(caret)
# Both arguments factor with same levels
# ?caret::confusionMatrix
conf=confusionMatrix(data=nb.class, reference=Direction.2005)
conf

# ROC curves
library(ROCR)
# Both arguments same class
pred <- prediction(predictions=nb.preds[, 1], labels=Direction.2005)
pred
perf <- performance(pred,"tpr","fpr")
perf
plot(perf, colorize=T)
library(ggfortify)
library(viridis)
autoplot(perf)+ scale_color_viridis()

## $K$-Nearest Neighbors ----------------------

###
library(class)
train.X <- cbind(Lag1, Lag2)[train, ]
test.X <- cbind(Lag1, Lag2)[!train, ]
train.Direction <- Direction[train]
###
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
(83 + 43) / 252
###
knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)
###
dim(Caravan)
attach(Caravan)
summary(Purchase)
348 / 5822
###
standardized.X <- scale(Caravan[, -86])
var(Caravan[, 1])
var(Caravan[, 2])
var(standardized.X[, 1])
var(standardized.X[, 2])
###
test <- 1:1000
train.X <- standardized.X[-test, ]
test.X <- standardized.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
###
table(knn.pred, test.Y)
9 / (68 + 9)
###
knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
5 / 26
knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4 / 15
###
glm.fits <- glm(Purchase ~ ., data = Caravan,
    family = binomial, subset = -test)
glm.probs <- predict(glm.fits, Caravan[test, ],
    type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .5] <- "Yes"
table(glm.pred, test.Y)
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .25] <- "Yes"
table(glm.pred, test.Y)
11 / (22 + 11)

### Methods for Logistic Regression on Caravan Data ----

# confusionMatrix
library(caret)
# Both arguments factor with same levels
conf=confusionMatrix(data=as.factor(glm.pred), reference=test.Y)
conf

library(ROCR)
# Both arguments same class
pred <- prediction(predictions=glm.probs, labels=test.Y)
pred

# ROC curves
perf <- performance(pred,"tpr","fpr")
perf
plot(perf, colorize=T)
library(ggfortify)
library(viridis)
autoplot(perf)+ scale_color_viridis()

## Poisson Regression -------------------------

###
attach(Bikeshare)
dim(Bikeshare)
names(Bikeshare)
###
mod.lm <- lm(
    bikers ~ mnth + hr + workingday + temp + weathersit,
    data = Bikeshare
  )
summary(mod.lm)
###
contrasts(Bikeshare$hr) = contr.sum(24)
contrasts(Bikeshare$mnth) = contr.sum(12)
mod.lm2 <- lm(
    bikers ~ mnth + hr + workingday + temp + weathersit,
    data = Bikeshare
  )
summary(mod.lm2)
###
sum((predict(mod.lm) - predict(mod.lm2))^2)
###
all.equal(predict(mod.lm), predict(mod.lm2))
###
coef.months <- c(coef(mod.lm2)[2:12],
    -sum(coef(mod.lm2)[2:12]))
###
plot(coef.months, xlab = "Month", ylab = "Coefficient",
    xaxt = "n", col = "blue", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A",
    "M", "J", "J", "A", "S", "O", "N", "D"))
###
coef.hours <- c(coef(mod.lm2)[13:35],
    -sum(coef(mod.lm2)[13:35]))
plot(coef.hours, xlab = "Hour", ylab = "Coefficient",
    col = "blue", pch = 19, type = "o")
###
mod.pois <- glm(
    bikers ~ mnth + hr + workingday + temp + weathersit,
    data = Bikeshare, family = poisson
  )
summary(mod.pois)
###
coef.mnth <- c(coef(mod.pois)[2:12],
    -sum(coef(mod.pois)[2:12]))
plot(coef.mnth, xlab = "Month", ylab = "Coefficient",
     xaxt = "n", col = "blue", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
coef.hours <- c(coef(mod.pois)[13:35],
     -sum(coef(mod.pois)[13:35]))
plot(coef.hours, xlab = "Hour", ylab = "Coefficient",
    col = "blue", pch = 19, type = "o")
###
plot(predict(mod.lm2), predict(mod.pois, type = "response"))
abline(0, 1, col = 2, lwd = 3)
###

### Methods for Poisson Regression --------------

anova(mod.pois)
library(broom)
tidy(mod.pois,conf.int=T)
tidy(mod.pois,exponentiate=T,conf.int=T)
glance(mod.pois)
augment(mod.pois,type.predict="response")
par(mfrow=c(2,2))
plot(mod.pois)
library(ggfortify)
autoplot(mod.pois)
library(plotmo)
plotmo(mod.pois)
plotmo(mod.lm)
plotmo(mod.lm2)
par(mfrow=c(1,1))
