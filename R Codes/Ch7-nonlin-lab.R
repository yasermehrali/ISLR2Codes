
# Lab: Non-linear Modeling -----------

###
library(ISLR2)
attach(Wage)

## Polynomial Regression and Step Functions ----

###
fit <- lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))

confint(fit)
library(broom)
tidy(fit,conf.int = T)
glance(fit)
augment(fit)
library(ggplot2)
ggplot(Wage,aes(x=age,y=wage))+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("Linear Regression")
# Fig7.1a
ggplot(Wage,aes(x=age,y=wage))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~poly(x,4))+
  ggtitle("Degree−4 Polynomial")
library(plotmo)
# Fig7.1a
plotmo(fit)
plotmo(fit,pt.col=1)
plotmo(fit,level=0.95,pt.col=1)

####
fit2 <- lm(wage ~ poly(age, 4, raw = T), data = Wage)
coef(summary(fit2))
###
fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4),
            data = Wage)
coef(fit2a)
###
fit2b <- lm(wage ~ cbind(age, age^2, age^3, age^4),
            data = Wage)
coef(fit2b)
###
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid),
                 se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit,
                  preds$fit - 2 * preds$se.fit)
###
# Fig7.1a
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1),
    oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)
###
preds2 <- predict(fit2, newdata = list(age = age.grid),
                  se = TRUE)
max(abs(preds$fit - preds2$fit))
###
fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
###
coef(summary(fit.5))
###
(-11.983)^2
###
fit.1 <- lm(wage ~ education + age, data = Wage)
fit.2 <- lm(wage ~ education + poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ education + poly(age, 3), data = Wage)
anova(fit.1, fit.2, fit.3)
###
fit <- glm(I(wage > 250) ~ poly(age, 4), data = Wage,
           family = binomial)
###
preds <- predict(fit, newdata = list(age = age.grid), se = T)
###
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit,
                        preds$fit - 2 * preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))
###
preds <- predict(fit, newdata = list(age = age.grid),
                 type = "response", se = T)
###
# Fig7.1b
plot(age, I(wage > 250), xlim = agelims, type = "n",
     ylim = c(0, .2))
points(jitter(age), I((wage > 250) / 5), cex = .5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# Fig7.1b
ggplot(Wage,aes(x=age,y=as.numeric(wage > 250)))+
  geom_jitter(aes(x=age,y=as.numeric((wage > 250) / 5)),height = 0)+
  geom_smooth(method = "glm",formula = y~poly(x,4),method.args = list(family = "binomial"))+
  coord_cartesian(ylim = c(0, 0.2))+
  ggtitle("Degree−4 Polynomial Binary Logistic")

library(plotmo)
# Fig7.1b
plotmo(fit)
plotmo(fit,pt.col=1)
plotmo(fit,level=0.95,pt.col=1)

###
table(cut(age, 4))
prop.table(table(cut(age, 4)))
fit <- lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))

# Fig7.2a
ggplot(Wage,aes(x=age,y=wage))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~cut(x, 4), n=901)+
  ggtitle("Piecewise Constant")

library(plotmo)
# Fig7.2a
plotmo(fit, ngrid1=901)
plotmo(fit,pt.col=1, ngrid1=901)
plotmo(fit,level=0.95,pt.col=1, ngrid1=901)

## Splines ---------------------------

###
library(splines)

fit <- lm(wage ~ bs(age), data = Wage)
coef(summary(fit))

fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
coef(summary(fit))

pred <- predict(fit, newdata = list(age = age.grid), se = T)

# Fig7.4
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")
###
dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots")
###
fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid),
                 se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

# Fig7.4
ggplot(Wage,aes(x=age,y=wage))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~bs(x, knots = c(25, 40, 60)),fill="blue")+
  geom_smooth(method = "lm",formula = y~ns(x, df = 4),col="red",fill="red")
ggplot(Wage,aes(x=age,y=wage))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~bs(x, knots = c(25, 40, 60)),aes(color="Cubic Spline"))+
  geom_smooth(method = "lm",formula = y~ns(x, df = 4),aes(color="Natural Cubic Spline"))+
  scale_color_manual(
    name = "Model fit",
    breaks = c("Natural Cubic Spline", "Cubic Spline"),
    values = c("Natural Cubic Spline" = "red", "Cubic Spline" = "blue") 
  )+
  theme(legend.position = "top")

plotmo(fit)
plotmo(fit,pt.col=1)
plotmo(fit,level=0.95,pt.col=1)

plotmo(fit2)
plotmo(fit2,pt.col=1)
plotmo(fit2,level=0.95,pt.col=1)

# Fig7.7
ggplot(Wage,aes(x=age,y=wage))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~poly(x, 15),se=F)+
  geom_smooth(method = "lm",formula = y~ns(x, df = 15),col="red",se=F)
ggplot(Wage,aes(x=age,y=wage))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~poly(x, 15),se=F,aes(color="Polynomial"))+
  geom_smooth(method = "lm",formula = y~ns(x, df = 15),se=F,aes(color="Natural Cubic Spline"))+
  scale_color_manual(
    name = "Model fit",
    breaks = c("Natural Cubic Spline", "Polynomial"),
    values = c("Natural Cubic Spline" = "red", "Polynomial" = "blue") 
  )+
  theme(legend.position = "top")

###
### Smoothing Spline ------------------

fit <- smooth.spline(age, wage, df = 16)
fit$df
fit$spar
fit2 <- smooth.spline(age, wage, cv = TRUE)
fit2$df
fit2$spar

library(broom)
glance(fit)
glance(fit2)
augment(fit)

# Fig7.8
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# Fig7.8
detach("package:mgcv", unload = TRUE)
library(gam)
gam1 <- gam(wage ~ s(age, 16), data = Wage)
plot(gam1)
plotmo(gam1)
plotmo(gam1,pt.col=1)
gam2 <- gam(wage ~ s(age, 6.8), data = Wage)
plot(gam2)
plotmo(gam2)
plotmo(gam2,pt.col=1)

# Fig7.8
library(gam)
# mgcv:
ggplot(Wage,aes(x=age,y=wage))+
  geom_point()+
  geom_smooth()
ggplot(Wage,aes(x=age,y=wage))+
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))
ggplot(Wage,aes(x=age,y=wage))+
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x, sp=fit$spar),se=F,aes(color="16 DF"))+
  geom_smooth(method = "gam", formula = y ~ s(x, sp=fit2$spar),se=F,aes(color="6.8 DF"))+
  scale_color_manual(
    name = NULL,
    breaks = c("16 DF", "6.8 DF"),
    values = c("16 DF" = "red", "6.8 DF" = "blue") 
  )+
  theme(legend.position = "top")+
  ggtitle("Smoothing Spline")

###
### Local Regression -----------------

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ age, span = .2, data = Wage)
fit2 <- loess(wage ~ age, span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)),
      col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)),
      col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

ggplot(Wage,aes(x=age,y=wage))+
  geom_point()+
  geom_smooth(method = loess, method.args = list(span = .2),se=F,aes(col="Span = 0.2"))+
  geom_smooth(method = loess, method.args = list(span = .5),se=F,aes(col="Span = 0.5"))+
  scale_color_manual(
    name = NULL,
    breaks = c("Span = 0.2", "Span = 0.5"),
    values = c("Span = 0.2" = "red", "Span = 0.5" = "blue") 
  )+
  theme(legend.position = "top")+
  ggtitle("Local Regression")

plotmo(fit)
plotmo(fit,pt.col=1)

plotmo(fit2)
plotmo(fit2,pt.col=1)

## GAMs ------------------------------

###
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education,
           data = Wage)
###
detach("package:mgcv", unload = TRUE)
library(gam)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education,
              data = Wage)
###
par(mfrow = c(1, 3))
#Fig7.12
plot(gam.m3, se = TRUE, col = "blue")
###
#Fig7.11
plot.Gam(gam1, se = TRUE, col = "red")

#Fig7.12
plotmo(gam.m3)
plotmo(gam.m3,ylim=NA)
plotmo(gam.m3,pt.col=1)
par(mfrow = c(1, 3))
plotmo(gam.m3,pt.col=1,do.par = F)

#Fig7.11
plotmo(gam1)
plotmo(gam1,ylim=NA)
plotmo(gam1,level=0.95,pt.col=1)
par(mfrow = c(1, 3))
plotmo(gam1,level=0.95,ylim=NA,do.par = F)
plotmo(gam1,level=0.95,pt.col=1,do.par = F)

###
gam.m1 <- gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education,
              data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")
###
summary(gam.m3)
library(broom)
tidy(gam.m3)
glance(gam.m3)

###
preds <- predict(gam.m2, newdata = Wage)
###
gam.lo <- gam(
  wage ~ s(year, df = 4) + lo(age, span = 0.7) + education,
  data = Wage
)

tidy(gam.lo)
glance(gam.lo)

par(mfrow = c(1, 3))
plot(gam.lo, se = TRUE, col = "green")

plotmo(gam.lo)
plotmo(gam.lo,ylim=NA)
plotmo(gam.lo,pt.col=1)
par(mfrow = c(1, 3))
plotmo(gam.lo,pt.col=1,do.par = F)


###
gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education,
                data = Wage)
tidy(gam.lo.i)
glance(gam.lo.i)

###
par(mfrow = c(1, 2))
plot(gam.lo.i)

plotmo(gam.lo.i)
plotmo(gam.lo.i,ylim=NA)
plotmo(gam.lo.i,pt.col=1)

library(akima)
plot(gam.lo.i)

###
gam.lr <- gam(
  I(wage > 250) ~ year + s(age, df = 5) + education,
  family = binomial, data = Wage
)
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")

plotmo(gam.lr)
plotmo(gam.lr,ylim=NA)
plotmo(gam.lr,pt.col=1)

###
table(education, I(wage > 250))
###
gam.lr.s <- gam(
  I(wage > 250) ~ year + s(age, df = 5) + education,
  family = binomial, data = Wage,
  subset = (education != "1. < HS Grad")
)
plot(gam.lr.s, se = T, col = "green")
###
