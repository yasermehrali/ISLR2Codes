
# Lab: Cross-Validation and the Bootstrap ----


## The Validation Set Approach ---------------

###
library(ISLR2)
set.seed(1)
train <- sample(392, 196)
###
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
###
attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
library(modelr)
mse(lm.fit,Auto[-train,])
###
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, 
              subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
mse(lm.fit2,Auto[-train,])
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, 
              subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
mse(lm.fit3,Auto[-train,])
###
set.seed(2)
train <- sample(392, 196)
lm.fit <- lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
mse(lm.fit,Auto[-train,])
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, 
              subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
mse(lm.fit2,Auto[-train,])
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, 
              subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
mse(lm.fit3,Auto[-train,])

## Leave-One-Out Cross-Validation ------------

###
glm.fit <- glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)
###
lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)
###
library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta
###
cv.error <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

### Methods for Leave-One-Out Cross-Validation ----

library(purrr)
library(modelr)
cv1 <- crossv_loo(Auto)
cv1
i=5
models <- map(cv1$train, ~ lm(mpg ~ poly(horsepower, i), data = .))
errs <- map2_dbl(models, cv1$test, mse)
hist(errs)

## $k$-Fold Cross-Validation ----------------

###
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10

### Methods for $k$-Fold Cross-Validation ----

ts.plot(cv.error.10,xlab="Degree of Polynomial",ylab="MSE",type="b")

library(purrr)
cv2 <- crossv_kfold(Auto,k = 10)
cv2
i=5
models <- map(cv2$train, ~ lm(mpg ~ poly(horsepower, i), data = .))
errs <- map2_dbl(models, cv2$test, mse)
hist(errs)

## The Bootstrap ----------------------------

### Estimating the Accuracy of a Statistic of Interest ----

###
alpha.fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}
###
alpha.fn(Portfolio, 1:100)
###
set.seed(7)
alpha.fn(Portfolio, sample(100, 100, replace = T))
###
boot(Portfolio, alpha.fn, R = 1000)

#### Methods for ... ------------------------

boot.out=boot(Portfolio, alpha.fn, R = 1000)
boot.out
hist(boot.out$t,col="lightblue")
abline(v=alpha.fn(Portfolio, 1:100),col="red",lwd=2)
boot.ci(boot.out, conf = 0.95, type = "all")

### Estimating the Accuracy of a Linear Regression Model ----

###
boot.fn <- function(data, index)
  coef(lm(mpg ~ horsepower, data = data, subset = index))
boot.fn(Auto, 1:392)
###
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
boot.fn(Auto, sample(392, 392, replace = T))
###
boot(Auto, boot.fn, 1000)
###
summary(lm(mpg ~ horsepower, data = Auto))$coef
###
boot.fn <- function(data, index)
  coef(
      lm(mpg ~ horsepower + I(horsepower^2), 
        data = data, subset = index)
    )
set.seed(1)
boot(Auto, boot.fn, 1000)
summary(
    lm(mpg ~ horsepower + I(horsepower^2), data = Auto)
  )$coef
###

#### Methods for ... ------------------------

library(purrr)
boot <- modelr::bootstrap(Auto, 1000)
boot
models <- map(boot$strap, ~ lm(mpg ~ horsepower + I(horsepower^2), data = .))
tidied <- map_df(models, broom::tidy, .id = "id")
tidied
hist(subset(tidied, term == "(Intercept)")$estimate)
hist(subset(tidied, term == "horsepower")$estimate)
hist(subset(tidied, term == "I(horsepower^2)")$estimate)

library(dplyr)
hist(filter(tidied, term == "(Intercept)")$estimate)
hist(filter(tidied, term == "horsepower")$estimate)
hist(filter(tidied, term == "I(horsepower^2)")$estimate)

