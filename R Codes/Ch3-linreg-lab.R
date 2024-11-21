
# Lab: Linear Regression -----------------------


## Libraries -----------------------------------

###
library(MASS)
library(ISLR2)

## Simple Linear Regression --------------------

###
head(Boston)
###
lm.fit <- lm(medv ~ lstat)
###
lm.fit <- lm(medv ~ lstat, data = Boston)
attach(Boston)
lm.fit <- lm(medv ~ lstat)
###
lm.fit
summary(lm.fit)
###
names(lm.fit)
coef(lm.fit)
###
confint(lm.fit)
###
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
    interval = "confidence")
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
    interval = "prediction")
###
plot(lstat, medv)
abline(lm.fit)
###
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)
###
par(mfrow = c(2, 2))
plot(lm.fit)
###
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
###
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

### Methods for Simple Linear Regression -------

anova(lm.fit)
library(gvlma)
gvmodel=gvlma(lm.fit)
summary(gvmodel)
library(broom)
tidy(lm.fit)
tidy(lm.fit,conf.int=T)
glance(lm.fit)
augment(lm.fit)
augment(lm.fit,interval = "confidence")
augment(lm.fit,interval = "prediction")
ggstats::ggcoef_model(lm.fit)
GGally::ggcoef_model(lm.fit)
ggstatsplot::ggcoefstats(lm.fit)
library(ggplot2)
ggplot(Boston,aes(x=lstat,y=medv))+
  geom_point()+
  geom_smooth(method = "lm")
library(ggformula)
ggplot(Boston,aes(x=lstat,y=medv))+
  geom_point()+
  geom_lm(interval = "prediction", fill = "skyblue") +
  geom_lm(interval = "confidence") 
library(plotmo)
plotmo(lm.fit)
plotmo(lm.fit,pt.col=1)
plotmo(lm.fit,level=0.95,pt.col=1)
plotres(lm.fit)
library(ggfortify)
autoplot(lm.fit)
library(gglm)
gglm(lm.fit)
library(lindia)
gg_diagnose(lm.fit)
library(ggResidpanel)
resid_panel(lm.fit)

## Multiple Linear Regression -----------------

###
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)
###
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)
###
library(car)
vif(lm.fit)
###
lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)
###
lm.fit1 <- update(lm.fit, ~ . - age)

### Methods for Multiple Linear Regression --------

library(ggheatmap)
ggheatmap(t(scale(Boston)))
library(ggcorrplot)
ggcorrplot(cor(Boston))
ggcorrplot(cor(Boston),hc.order=T)
library(GGally)
ggpairs(Boston)
p=ggpairs(Boston, lower=list(continuous=ggally_smooth_lm))+
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8))
p
library(ggpubr)
p_list=lapply(1:12,function(x){p[13,x]})
ggarrange(plotlist=p_list, nrow=3, ncol=4)
anova(lm.fit)
library(gvlma)
gvmodel=gvlma(lm.fit)
summary(gvmodel)
library(broom)
tidy(lm.fit)
tidy(lm.fit,conf.int=T)
glance(lm.fit)
augment(lm.fit)
augment(lm.fit,interval = "confidence")
augment(lm.fit,interval = "prediction")
ggstats::ggcoef_model(lm.fit)
GGally::ggcoef_model(lm.fit)
ggstatsplot::ggcoefstats(lm.fit)
library(scatterplot3d)
my.lm <- lm(medv ~ lstat + age, data = Boston)
s3d <- scatterplot3d(
  Boston[c("lstat","age","medv")],
  type="h", highlight.3d=TRUE,
  angle=55, scale.y=1, pch=16,
  zlim=range(c(Boston$medv,predict(my.lm)))
)
s3d$plane3d(my.lm, lty.box = "solid")

## Interaction Terms --------------------------

###
summary(lm(medv ~ lstat * age, data = Boston))

### Methods for Interaction Terms -------------

library(plotmo)
library(effects)
lm.fit1=lm(medv ~ lstat + age, data = Boston)
lm.fit=lm(medv ~ lstat * age, data = Boston)
anova(lm.fit1,lm.fit)
models=list(model1=lm.fit1,model2=lm.fit)
GGally::ggcoef_compare(models,type = "faceted")
ggstats::ggcoef_compare(models,type = "faceted")
plotmo(lm.fit)
plotmo(lm.fit,pt.col=1, persp.ticktype="detailed",persp.nticks=3)
plotmo(lm.fit,level=0.95,pt.col=1, type2="image",image.col=heat.colors(12))
plotmo(lm.fit,level=0.95,pt.col=1, type2="contour", contour.col=2, contour.labcex=.4)

plot(effect('lstat:age', lm.fit, xlevels=list(lstat=seq(0, 40, 5))), lines=list(multiline=T))
plot(effect('lstat:age', lm.fit, xlevels=list(age=seq(0, 100, 5))), lines=list(multiline=T))

## Non-linear Transformations of the Predictors ----

###
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)
###
lm.fit <- lm(medv ~ lstat)
anova(lm.fit, lm.fit2)
###
par(mfrow = c(2, 2))
plot(lm.fit2)
###
lm.fit5 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit5)
###
summary(lm(medv ~ log(rm), data = Boston))

### Methods for Non-linear Transformations of the Predictors ----

library(ggplot2)
library(plotmo)
plotmo(lm.fit2,level=0.95,pt.col=1)
ggplot(Boston,aes(x=lstat,y=medv))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~x + I(x^2))
ggstats::ggcoef_model(lm.fit2)
plotmo(lm.fit5,level=0.95,pt.col=1)
ggplot(Boston,aes(x=lstat,y=medv))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~poly(x,5))
ggstats::ggcoef_model(lm.fit5)
lm.fit5new <- lm(medv ~ lstat+I(lstat^2)+I(lstat^3)+I(lstat^4)+I(lstat^5))
models=list(model1=lm.fit,model2=lm.fit2,model5=lm.fit5new)
GGally::ggcoef_compare(models,type = "faceted")
ggstats::ggcoef_compare(models,type = "faceted")
fit.log=lm(medv ~ log(rm), data = Boston)
plotmo(fit.log,level=0.95,pt.col=1)
ggplot(Boston,aes(x=rm,y=medv))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~log(x))

## Qualitative Predictors ---------------------

###
head(Carseats)
###
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, 
    data = Carseats)
summary(lm.fit)
###
attach(Carseats)
contrasts(ShelveLoc)

### Methods for Qualitative Predictors --------

library(GGally)
ggpairs(
  Carseats,
  columns=c(2:6,8:9,1,7,10:11),
  lower=list(continuous=ggally_smooth_lm)
)+
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8))
anova(lm.fit)
library(broom)
tidy(lm.fit)
tidy(anova(lm.fit))
ggstats::ggcoef_model(lm.fit)
GGally::ggcoef_model(lm.fit)
ggstatsplot::ggcoefstats(lm.fit)
library(plotmo)
plotmo(lm.fit)
plotmo(lm.fit,level=0.95,pt.col=1)

## Writing  Functions -------------------------

###
LoadLibraries
LoadLibraries()
###
LoadLibraries <- function() {
 library(ISLR2)
 library(MASS)
 print("The libraries have been loaded.")
}
###
LoadLibraries
###
LoadLibraries()
###
