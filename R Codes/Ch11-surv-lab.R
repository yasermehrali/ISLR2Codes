
# Lab: Survival Analysis ----------


## Brain Cancer Data --------------

###
library(ISLR2)
###
names(BrainCancer)
###
attach(BrainCancer)
table(sex)
table(diagnosis)
table(status)

#### Frequencies ------------------

library(dplyr)
BrainCancer %>% count(sex)
BrainCancer %>% count(diagnosis)
BrainCancer %>% count(status)
BrainCancer %>% count(sex,diagnosis)
BrainCancer %>% count(sex,status)
BrainCancer %>% count(diagnosis,status)
BrainCancer %>% count(sex,diagnosis,status)

###
library(survival)
fit.surv <- survfit(Surv(time, status) ~ 1,data = BrainCancer)
plot(fit.surv, xlab = "Months",
     ylab = "Estimated Probability of Survival")

#### Methods for class survfit without covariate -----

plot(fit.surv, cumhaz = T)

class(fit.surv)
fit.surv
quantile(fit.surv)
median(fit.surv)
residuals(fit.surv,times = c(0,20,40,60,80))
summary(fit.surv)
library(broom)
tidy(fit.surv)
glance(fit.surv)
library(ggfortify)
fortify(fit.surv)
autoplot(fit.surv)
autoplot(fit.surv,conf.int=F)
autoplot(fit.surv,censor =F)
library(GGally)
ggsurv(fit.surv)
ggsurv(fit.surv,CI=F)
library(survminer)
ggsurvplot(fit.surv, data = BrainCancer)
ggsurvplot(fit.surv, data = BrainCancer, conf.int = F)
ggsurvplot(fit.surv, data = BrainCancer, fun="event")
ggsurvplot(fit.surv, data = BrainCancer, fun="cumhaz")
ggsurvplot(fit.surv, data = BrainCancer, fun="pct")
ggsurvplot(fit.surv, data = BrainCancer, risk.table = TRUE)
ggsurvplot(
  fit.surv,                     # survfit object with calculated statistics.
  data = BrainCancer,  # data used to fit survival curves. 
  risk.table = TRUE,       # show risk table.
  # pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  xlim = c(0,90),        # present narrower X axis, but not affect
  # survival estimates.
  break.time.by = 10,     # break X axis in time intervals by 500.
  ggtheme = theme_minimal(), # customize plot and risk table with a theme.
  risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = FALSE # show bars instead of names in text annotations
  # in legend of risk table
)
ggsurvplot(
  fit.surv,                     # survfit object with calculated statistics.
  data = BrainCancer,
  title = "Survival curves", subtitle = "Based on Kaplan-Meier estimates",
  caption = "created with survminer",
  font.title = c(16, "bold", "darkblue"),
  font.subtitle = c(15, "bold.italic", "purple"),
  font.caption = c(14, "plain", "orange"),
  font.x = c(14, "bold.italic", "red"),
  font.y = c(14, "bold.italic", "darkred"),
  font.tickslab = c(12, "plain", "darkgreen")
)
ggsurvplot(
  fit.surv,                     # survfit object with calculated statistics.
  data = BrainCancer, 
  title = "Survival curves", subtitle = "Based on Kaplan-Meier estimates",
  caption = "created with survminer",
  font.title = c(16, "bold", "darkblue"),
  font.subtitle = c(15, "bold.italic", "purple"),
  font.caption = c(14, "plain", "orange"),
  font.x = c(14, "bold.italic", "red"),
  font.y = c(14, "bold.italic", "darkred"),
  font.tickslab = c(12, "plain", "darkgreen"),
  ########## risk table #########,
  risk.table = TRUE,
  risk.table.title = "Note the risk set sizes",
  risk.table.subtitle = "and remember about censoring.",
  risk.table.caption = "source code: website.com",
  risk.table.height = 0.45
)


library(ggsurvfit)
# Default publication ready plot
fit=survfit2(Surv(time, status) ~ 1, data = BrainCancer)
ggsurvfit(fit)
# Changing statistic type
ggsurvfit(fit,type = "cumhaz")
ggsurvfit(fit,type = "risk")
# Configuring KM line type to vary by strata
ggsurvfit(fit,linetype_aes = TRUE)
# Customizing the plot to your needs
ggsurvfit(fit) + 
  add_censor_mark() +
  add_confidence_interval() +
  add_quantile() +
  add_risktable()


###
fit.sex <- survfit(Surv(time, status) ~ sex, data = BrainCancer)
plot(fit.sex, xlab = "Months",
     ylab = "Estimated Probability of Survival", col = c(2,4))
legend("bottomleft", levels(sex), col = c(2,4), lty = 1)

#### Methods for class survfit with covariate -----

plot(fit.sex, cumhaz = T)

class(fit.sex)
fit.sex
dim(fit.sex)
median(fit.sex)
quantile(fit.sex)
residuals(fit.sex,times = c(0,20,40,60,80))
summary(fit.sex)
library(broom)
tidy(fit.sex)
library(ggfortify)
fortify(fit.sex)
autoplot(fit.sex)
autoplot(fit.sex,conf.int=F)
autoplot(fit.sex,censor =F)
autoplot(fit.sex,facets = T)
autoplot(fit.sex,facets = T,ncol=2)
library(GGally)
ggsurv(fit.sex)
ggsurv(fit.sex,CI=T)
library(survminer)
ggsurvplot(fit.sex, data = BrainCancer)
ggsurvplot(fit.sex, data = BrainCancer, fun="event")
ggsurvplot(fit.sex, data = BrainCancer, fun="cumhaz")
ggsurvplot(fit.sex, data = BrainCancer, fun="pct")
ggsurvplot(fit.sex, data = BrainCancer, risk.table = TRUE)
ggsurvplot(fit.sex, data = BrainCancer, risk.table = TRUE, ncensor.plot = TRUE)
ggsurvplot(
  fit.sex,                     # survfit object with calculated statistics.
  data = BrainCancer,  # data used to fit survival curves. 
  risk.table = TRUE,       # show risk table.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  xlim = c(0,90),        # present narrower X axis, but not affect
  # survival estimates.
  break.time.by = 10,     # break X axis in time intervals by 500.
  ggtheme = theme_minimal(), # customize plot and risk table with a theme.
  risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = FALSE # show bars instead of names in text annotations
  # in legend of risk table
)
ggsurvplot(
  fit.sex,                     # survfit object with calculated statistics.
  data = BrainCancer,
  title = "Survival curves", subtitle = "Based on Kaplan-Meier estimates",
  caption = "created with survminer",
  font.title = c(16, "bold", "darkblue"),
  font.subtitle = c(15, "bold.italic", "purple"),
  font.caption = c(14, "plain", "orange"),
  font.x = c(14, "bold.italic", "red"),
  font.y = c(14, "bold.italic", "darkred"),
  font.tickslab = c(12, "plain", "darkgreen")
)
ggsurvplot(
  fit.sex,                     # survfit object with calculated statistics.
  data = BrainCancer, 
  title = "Survival curves", subtitle = "Based on Kaplan-Meier estimates",
  caption = "created with survminer",
  font.title = c(16, "bold", "darkblue"),
  font.subtitle = c(15, "bold.italic", "purple"),
  font.caption = c(14, "plain", "orange"),
  font.x = c(14, "bold.italic", "red"),
  font.y = c(14, "bold.italic", "darkred"),
  font.tickslab = c(12, "plain", "darkgreen"),
  ########## risk table #########,
  risk.table = TRUE,
  risk.table.title = "Note the risk set sizes",
  risk.table.subtitle = "and remember about censoring.",
  risk.table.caption = "source code: website.com",
  risk.table.height = 0.3
)

library(ggsurvfit)
# Default publication ready plot
fit=survfit2(Surv(time, status) ~ sex, data = BrainCancer)
ggsurvfit(fit)
# Changing statistic type
ggsurvfit(fit,type = "cumhaz")
ggsurvfit(fit,type = "risk")
# Configuring KM line type to vary by strata
ggsurvfit(fit,linetype_aes = TRUE)
# Customizing the plot to your needs
ggsurvfit(fit) + 
  add_censor_mark() +
  add_confidence_interval() +
  add_quantile() +
  add_risktable() +
  add_pvalue() +
  add_legend_title("Gender:") +
  add_risktable_strata_symbol()
ggsurvfit(fit) + 
  add_pvalue(location = "annotation")
ggsurvfit(fit) + 
  add_risktable(risktable_group='risktable_stats') +
  add_risktable_strata_symbol()

###
logrank.test <- survdiff(Surv(time, status) ~ sex)
logrank.test

#### Methods for class survdiff -----

class(logrank.test)
library(broom)
tidy(logrank.test)
glance(logrank.test)

library(survminer)
ggsurvplot(fit.sex, data = BrainCancer, pval = TRUE)

###
fit.cox <- coxph(Surv(time, status) ~ sex)
summary(fit.cox)
###
summary(fit.cox)$logtest[1]
summary(fit.cox)$waldtest[1]
summary(fit.cox)$sctest[1]
###
logrank.test$chisq

#### Methods for class coxph with one covariate -----

fit.cox
class(fit.cox)
anova(fit.cox)
concordance(fit.cox)
nobs(fit.cox)
logLik(fit.cox)
extractAIC(fit.cox)
vcov(fit.cox)
model.frame(fit.cox)
model.matrix(fit.cox)
fitted(fit.cox)
predict(fit.cox)
residuals(fit.cox)
library(broom)
tidy(fit.cox)
tidy(fit.cox,exponentiate = T)
tidy(fit.cox,conf.int = T)
glance(fit.cox)
augment(fit.cox,data = BrainCancer)

library(ggeffects)
ggpredict(fit.cox,type = "survival")
ggpredict(fit.cox,type = "cumhaz")

library(survminer)
ggforest(fit.cox, data = BrainCancer)
library(GGally)
ggcoef(fit.cox)
library(ggstats)
ggcoef_model(fit.cox)
library(ggstatsplot)
ggcoefstats(fit.cox)

###
fit.all <- coxph(
  Surv(time, status) ~ sex + diagnosis + loc + ki + gtv +
    stereo)
fit.all

#### Methods for class coxph with more covariates -----

class(fit.all)
summary(fit.all)
anova(fit.all)
concordance(fit.all)
nobs(fit.all)
logLik(fit.all)
extractAIC(fit.all)
vcov(fit.all)
model.frame(fit.all)
model.matrix(fit.all)
fitted(fit.all)
predict(fit.all)
predict(fit.all,type="lp")
predict(fit.all,type="risk")
predict(fit.all,type="expected")
predict(fit.all,type="terms")
predict(fit.all,type="survival")
residuals(fit.all)
library(broom)
tidy(fit.all)
tidy(fit.all,exponentiate = T)
tidy(fit.all,conf.int = T)
glance(fit.all)
augment(fit.all,data = BrainCancer)
augment(fit.all,data = BrainCancer, type.predict = "risk")
augment(fit.all,data = BrainCancer, type.predict = "risk")
augment(fit.all,data = BrainCancer, type.predict = "expected")
augment(fit.all,data = BrainCancer, type.predict = "terms")
augment(fit.all,data = BrainCancer, type.predict = "survival")

library(ggeffects)
ggpredict(fit.all,type = "survival")
ggpredict(fit.all,type = "cumhaz")

library(survminer)
ggforest(fit.all, data = BrainCancer)
library(GGally)
ggcoef(fit.all)
library(ggstats)
ggcoef_model(fit.all)
library(ggstatsplot)
ggcoefstats(fit.all)

###
modaldata <- data.frame(
  diagnosis = levels(diagnosis),
  sex = rep("Female", 4),
  loc = rep("Supratentorial", 4),
  ki = rep(mean(ki), 4),
  gtv = rep(mean(gtv), 4),
  stereo = rep("SRT", 4)
)
survplots <- survfit(fit.all, newdata = modaldata)
plot(survplots, xlab = "Months",
     ylab = "Survival Probability", col = 2:5)
legend("bottomleft", levels(diagnosis), col = 2:5, lty = 1)

#### Methods for class survfitcox -----

class(survplots)
survplots
summary(survplots)
dim(survplots)
quantile(survplots)
median(survplots)
library(broom)
tidy(survplots)
library(ggfortify)
fortify(survplots)

## Publication Data ---------------

###
fit.posres <- survfit(
  Surv(time, status) ~ posres, data = Publication
)
plot(fit.posres, xlab = "Months",
     ylab = "Probability of Not Being Published", col = 3:4)
legend("topright", c("Negative Result", "Positive Result"),
       col = 3:4, lty = 1)
###
fit.pub <- coxph(Surv(time, status) ~ posres,
                 data = Publication)
fit.pub
###
logrank.test <- survdiff(Surv(time, status) ~ posres,
                         data = Publication)
logrank.test
###
fit.pub2 <- coxph(Surv(time, status) ~ . - mech,
                  data = Publication)
fit.pub2

## Call Center Data ---------------

###
set.seed(4)
N <- 2000
Operators <- sample(5:15, N, replace = T)
Center <- sample(c("A", "B", "C"), N, replace = T)
Time <- sample(c("Morn.", "After.", "Even."), N, replace = T)
X <- model.matrix( ~ Operators + Center + Time)[, -1]
###
X[1:5, ]
###
true.beta <- c(0.04, -0.3, 0, 0.2, -0.2)
h.fn <- function(x) return(0.00001 * x)
###
library(coxed)
queuing <- sim.survdata(N = N, T = 1000, X = X,
                        beta = true.beta, hazard.fun = h.fn)
names(queuing)
###
head(queuing$data)
mean(queuing$data$failed)
###
par(mfrow = c(1, 2))
fit.Center <- survfit(Surv(y, failed) ~ Center,
                      data = queuing$data)
plot(fit.Center, xlab = "Seconds",
     ylab = "Probability of Still Being on Hold",
     col = c(2, 4, 5))
legend("topright",
       c("Call Center A", "Call Center B", "Call Center C"),
       col = c(2, 4, 5), lty = 1)
###
fit.Time <- survfit(Surv(y, failed) ~ Time,
                    data = queuing$data)
plot(fit.Time, xlab = "Seconds",
     ylab = "Probability of Still Being on Hold",
     col = c(2, 4, 5))
legend("topright", c("Morning", "Afternoon", "Evening"),
       col = c(5, 2, 4), lty = 1)
###
survdiff(Surv(y, failed) ~ Center, data = queuing$data)
survdiff(Surv(y, failed) ~ Time, data = queuing$data)
###
fit.queuing <- coxph(Surv(y, failed) ~ .,
                     data = queuing$data)
fit.queuing
###
