### Code skeleton for Series 8, task 3
library(gss)
## read in data
data(ozone, package = "gss")

###################################################
### TASK a)
###################################################

ozone$logupo3 <- log(ozone$upo3)
d.ozone <- subset(ozone, select = -upo3)
pairs(d.ozone, pch = ".", gap = 0.1)

## delete outlier
out <- which.max(d.ozone[, "wdsp"])
d.ozone.e <- d.ozone[-out,]

###################################################
### TASK b)
###################################################

## package for wrapFormula()
require(sfsmisc)

## Linear models
## fit 1 (polynomial of degree 1)
form1 <- as.formula("logupo3 ~ .")
fit1 <- lm(form1, data = d.ozone.e)

## fits of degree 2 to 5
form2 <- wrapFormula(f = form1, data = d.ozone.e, wrapString = "poly(*, degree = 2)")
fit2 <- lm(form2, data = d.ozone.e)

form3 <- wrapFormula(f = form1, data = d.ozone.e, wrapString = "poly(*, degree = 3)")
fit3 <- lm(form3, data = d.ozone.e)

form4 <- wrapFormula(f = form1, data = d.ozone.e, wrapString = "poly(*, degree = 4)")
fit4 <- lm(form4, data = d.ozone.e)

form5 <- wrapFormula(f = form1, data = d.ozone.e, wrapString = "poly(*, degree = 5)")
fit5 <- lm(form5, data = d.ozone.e)

## GAM
require(mgcv)
gamForm <- wrapFormula(f = logupo3 ~ ., data = d.ozone.e) # by default = smoothing splines
g1 <- gam(formula = gamForm, data = d.ozone.e)

summary(g1)

###################################################
### TASK c)
###################################################

## plot the fits

## for the linear models
termplot(fit1, partial.resid=TRUE, rug=FALSE, se=TRUE, col.res='#C0C0C050', pch=19)
termplot(fit2, partial.resid=TRUE, rug=FALSE, se=TRUE, col.res='#C0C0C050', pch=19)
termplot(fit3, partial.resid=TRUE, rug=FALSE, se=TRUE, col.res='#C0C0C050', pch=19)
termplot(fit4, partial.resid=TRUE, rug=FALSE, se=TRUE, col.res='#C0C0C050', pch=19)
termplot(fit5, partial.resid=TRUE, rug=FALSE, se=TRUE, col.res='#C0C0C050', pch=19)

mult.fig(nr.plots = 9)
plot(g1, shade = TRUE)

## for the additive model
mult.fig(nr.plots = 9, main = "gam(gamForm, data = d.ozone.e)")
plot(g1, shade = TRUE)

###################################################
### TASK d)
###################################################

## Mallows Cp function

Cp <- function(object,sigma){
  res <- residuals(object)
  n <- length(res)
  p <- n - object$df.residual # object$df.residual outputs degrees of freedom
  # p = no of predictors
  SSE <- sum(res^2)
  SSE / sigma^2 - n + 2 * p
}

## set sigma (use estimated sigma from fit5)
sigma <- summary(fit5)$sigma # use sigma_hat of the full model

## Calculate Mallows's Cp statistic for all 6 models
Cp(fit1, sigma)
Cp(fit2, sigma)
Cp(fit3, sigma)
Cp(fit4, sigma)
Cp(fit5, sigma)
Cp(g1, sigma)
