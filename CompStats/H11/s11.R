library("ISLR")

head(Wage)
str(Wage)
summary(Wage)
unique(Wage$region) # all "Middle Atlantic"

# remove region
d.Wage <- subset(Wage, select = -region )

# we have 2 versions of 'Y': wage or logwage
(fm.lm  <- lm(wage ~ . - logwage, data = d.Wage))
(fm.lmL <- lm(logwage ~ . -wage,  data = d.Wage))

# define formula: polynomial only for the numeric predictors
require(sfsmisc) # for wrapFormula()

(isNum <- sapply(d.Wage, is.numeric))
isN.x <- isNum &  !(names(d.Wage) %in% c("wage","logwage")) # numeric and *not* Y

str(d.Wage.nx <- d.Wage[, isN.x])

# polynomial only for the *numeric* predictors
(fpoly <- wrapFormula(logwage ~ ., data=d.Wage[, isN.x], wrapString="poly(*,degree=3)"))

# wage ~ poly(year, degree = 3) + poly(age, degree = 3)
fpoly[[3]] # == RHS (fpoly)
# to eliminate 'wage' completely
(fFac <- formula(terms(logwage ~ . -wage, data=d.Wage, simplify=TRUE)))
(ff <- formula(paste(deparse1(fFac), "+", deparse1(fpoly[[3]]))))
## logwage ~ year + age + maritl + race + education + jobclass +
##     health + health_ins + poly(year, degree = 3) + poly(age,
##     degree = 3)

ff <- update(ff, wage ~ .^3)
mf <- model.frame(ff, d.Wage)
str( mm <- model.matrix  (mf, data=d.Wage) )## 3000 x 1375
str( y  <- model.response(mf ) ) ## 1:3000

#Exercise 2a)
require(glmnet)
f.ridge <- glmnet(mm, y, alpha=0)
f.lasso <- glmnet(mm, y, alpha=1)
op <- par(mfrow=c(1,2)) # op: save previous settings to revert
plot(f.ridge, xvar="lambda", main="Ridge Regression")
plot(f.lasso, xvar="lambda", main="Lasso Regression")
par(op) # revert to previous

#Exercise 2b)
sFile <- "cv-elastn.rds"
if(!file.exists(sFile)) {
  set.seed(1)
  print(system.time(
    cv.eln <- cv.glmnet(mm, y, alpha=0.5, nfolds=10)
  ))
  ## save it in the "safe-file"
  saveRDS(cv.eln, sFile)
} else { ## read pre-computed result:
  cv.eln <- readRDS(sFile)
}

plot(cv.eln) 
cv.eln$lambda.1se
cv.eln$lambda.min
