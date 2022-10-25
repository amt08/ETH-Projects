# 1.

# we study multivariate adaptive regression splines (MARS) and NNs
# we work with the ozone dataset and we log the response

rm(list = ls())
set.seed(123)
data(ozone, package = "gss")

chgNames <- function(dfr, oldNms, newNms) {
  colnames(dfr)[colnames(dfr) %in% oldNms] <- newNms
  dfr
}

d.ozone <- chgNames(ozone, oldNms = c("upo3", "wdsp", "hmdt", "sbtp"),
                      newNms = c("O3", "wind", "humidity", "temp"))

d.ozone <- subset(transform(d.ozone, "logO3" = log(O3)), select = -O3)
d.ozone.e <- d.ozone[-which.max(d.ozone[,"wind"]),]
d.ozone.es <- d.ozone.e
# centering and scaling of predictors
d.ozone.es[,-10] <- scale(d.ozone.e[,-10]) 

# b)

cv_r2 <- function(fitfn, formula = logO3 ~ . , data = d.ozone.es, ..., trace = TRUE)
{
  modFrame <- model.frame(formula, data = data)
  n <- nrow(data)
  ssr <- 0
  if(trace) cat("j = ")
  # LOOCV
  for(j in 1:n) {
    if(trace) cat(if(j %% (n %/% 10) == 1) paste(j, "") else ".")
    ## Fit without 'j' :
    fit <- fitfn(formula=formula, data = data[-j ,], ..., trace = FALSE)
    ## Evaluate at 'j' :
    ssr <- ssr + (model.response(modFrame)[j] - predict(fit, modFrame[j,]))^2
  }
  
  # compute SST
  fit <- fitfn(formula=formula, data = data, ...)
  # SST = sum((y - mean(y))^2)
  sst <- sum((model.response(modFrame)-mean(model.response(modFrame)))^2)
  if(trace) cat("\n")
  1-ssr/sst
}

# write another function r2 which computes the R2 without cross-validation
r2 <- function(fitfn, formula = logO3 ~ ., data = d.ozone.es, ...)
{
  modFrame <- model.frame(formula, data = data)
  Y <- model.response(modFrame)
  
  # fit the model
  fit <- fitfn(formula=formula, data = data, ...)
  # calculate sum of squares residuals
  ssr <- sum((Y - predict(fit, modFrame))^2)
  # compute sst
  sst <- sum((Y-mean(Y))^2)
  1-ssr/sst
}


# 2. MARS

# a)

# How well can a MARS explain the data? Calculate the R2 and the CV R2 for 
# the MARS model using the earth() function with degrees 1, 2, 3

# which model would you choose based on this info?

# degree will give you whether you want to have interaction or not between your predictors

require(earth)

# degree = Maximum degree of interaction 
cv_r2(earth, degree = 1)
r2(earth, degree = 1)

cv_r2(earth, degree = 2)
r2(earth, degree = 2)

cv_r2(earth, degree = 3)
r2(earth, degree = 3)

# b)

fit <- earth(formula = logO3 ~ ., data = d.ozone.es, degree = 2)
summary(fit)
plotmo(fit, degree2 = FALSE, caption = "Main effects")
plotmo(fit, degree1 = FALSE, caption = "Interactions")

# 3. NNs

# a) how well can a NN explain the data?

require(nnet)

set.seed(0)
# linout = identity activation
r2(nnet, formula = logO3 ~ ., data = d.ozone.es, size = 10, linout = TRUE, trace = FALSE)
set.seed(0)
cv_r2(nnet, size = 10, linout = TRUE, trace = FALSE)

set.seed(12)
r2(nnet, formula = logO3 ~ ., data = d.ozone.es, size = 3, linout = TRUE, trace = FALSE)
set.seed(12)
cv_r2(nnet, size = 3, linout = TRUE, trace = FALSE)

set.seed(12)
r2(nnet, formula = logO3 ~ ., data = d.ozone.es, size = 15, linout = TRUE, trace = FALSE)
set.seed(12)
cv_r2(nnet, size = 15, linout = TRUE, trace = FALSE)

# cross validate R2 is much worse than the in-sample R2, clearly!

# b)

set.seed(0)
# size = 2, does the day of the year affect the ozone level?
# no linout => BCE
fit_nn <- nnet(subset(d.ozone.es, select = - logO3), d.ozone.es[, "logO3"], size = 2)
summary(fit_nn)


set.seed(0)
fit <- nnet(logO3 ~ . , data = d.ozone.es, size = 2)
summary(fit)

# c)

# calculate cv R2 and R2 with and without weight decay

# without weight decay
set.seed(1)
cv_r2(nnet, formula = logO3 ~ . , data = d.ozone.es, size = 3,
     trace = FALSE, maxit = 500)

r2(nnet, formula = logO3 ~ . , data = d.ozone.es, size = 3,
  trace = FALSE, maxit = 500)

cv_r2(nnet, size = 3)
r2(nnet, size = 3)

# with weight decay

cv_r2(nnet, size = 3, decay = 1)
r2(nnet, size = 3, decay = 1)

cv_r2(nnet, formula = logO3 ~ . , data = d.ozone.es, size =3, decay = 1,
      trace = FALSE, maxit = 500)

set.seed(1)
r2(nnet, formula = logO3 ~ . , data = d.ozone.es, size =3, decay = 1,
     trace = FALSE, maxit = 500)

