# 1.

# y = log concentration of a serum
# x = age in months

# Goal: want to know if a complicated nonparametric regression gives us
# valuable information and which one is the best

# The generalization error of the following fits should be compared:
# 1. ksmooth
# 2. loess
# 3. smooth.spline with a fixed degree of freedom
# 4. smooth.spline with the smoothing parameter selected via cross-val
# 5. a constant fit by overall mean of Yi, ignoring Xi values

# a)

diabetes <- read.table('http://stat.ethz.ch/Teaching/Datasets/diabetes2.dat', header = TRUE)
reg <- diabetes[, c("Age", "C.Peptide")]
names(reg) <- c("x", "y")

# for easier dealing with the hat matrix we sort the observations along x
reg <- reg[sort.list(reg$x), ]
attach(reg)

plot(x, y, type = 'l')

# choose h by eye first 
# calculate the LOOCV score of your fit

# leave one out CV
loocv <- rep(0, length(x))

for(i in 1:length(x)){
  test_x <- x[i]
  test_y <- y[i]
  
  training_x <- x[-i]
  training_y <- y[-i]
  
  fitted_ksmooth <- ksmooth(training_x, training_y,
                          kernel = "normal", bandwidth = 4,
                          x.points = test_x)$y
  loocv[i] <- (test_y - fitted_ksmooth) ^ 2
}

loocv_score <- mean(loocv)
loocv_score

## calculate the degrees of freedom that correspond to the fit
n <- nrow(reg)
Id <- diag(n)
snw <- matrix(0, nrow = n, ncol = n)

for(j in 1:n){
  snw[, j] <- ksmooth(reg$x, Id[, j], x.points = reg$x, bandwidth = 4, kernel = "normal")$y
}

df_nw <- sum(diag(snw))

# we can also calculate the CV value
# with the shortcut, by looking at the hat matrix
y_fitted <- ksmooth(reg$x, reg$y,
                    kernel = "normal", bandwidth = 4,
                    x.points = reg$x)$y
cv_shortcut <- mean(((reg$y - y_fitted) / (1 - diag(snw))) ^ 2)
cv_shortcut

# cv_shortcut = mean( (fitted - true_y / 1 - diag(shat)) ^ 2)

# b)
# perform a non-parametric regression on the diabetes dataset
# using loess
# use the degrees of freedom from task a)

loocv_lp <- rep(0, length(x))

for(i in 1:length(x)){
  test_x <- x[i]
  test_y <- y[i]
  
  training_x <- x[-i]
  training_y <- y[-i]
  
  fitted_lp <- predict(loess(training_y ~ training_x, enp.target = df_nw,
                               surface = "direct"), newdata = test_x)
  loocv_lp[i] <- (test_y - fitted_lp) ^ 2
}

loocvlp_score <- mean(loocv_lp)
loocvlp_score

# we do again the shortcut with the hat matrix to calculate CV
n <- nrow(reg)
Id <- diag(n)
slp <- matrix(0, n, n)

for(j in 1:n){
  slp[, j] <- predict(loess(Id[, j] ~ reg$x, enp.target = df_nw,
                            surface = "direct"), newdata = reg$x)
}

y_fitted_lp <- predict(loess(reg$y ~ reg$x, enp.target = df_nw, surface = "direct"),
                       newdata = reg$x)

cv_shortcut_lp <- mean(((y_fitted_lp - reg$y) / (1 - diag(slp))) ^ 2)

cv_shortcut_lp

# c) perform a non-parametric regression on the dataset diabetes
# using a smoothing splines fit

est_ss <- smooth.spline(reg$x, reg$y, cv = TRUE, df = df_nw)
est_ss$cv.crit

# compare this value obtained with the one you calculate
loocv_ss <- rep(0, length(x))

for(i in 1:length(x)){
  test_x <- x[i]
  test_y <- y[i]
  
  training_x <- x[-i]
  training_y <- y[-i]
  
  fitted_ss <- predict(smooth.spline(training_x, training_y, spar = est_ss$spar), x = test_x)$y
  loocv_ss[i] <- (test_y - fitted_ss) ^ 2
}

loocvss_score <- mean(loocv_ss)
loocvss_score

# calculating the cv score using the hat matrix
n <- nrow(reg)
Id <- diag(n)
sss <- matrix(0, n, n)

for(j in 1:n){
  sss[, j] <- predict(smooth.spline(reg$x, Id[, j],
                                    spar = est_ss$spar), x = reg$x)$y
}

y_fitted_ss <- predict(smooth.spline(reg$x, reg$y, spar = est_ss$spar), x = reg$x)$y
cv_shortcut_ss <- mean(((reg$y - y_fitted_ss) / (1 - diag(sss)))^ 2)
cv_shortcut_ss

# d) 

# perform a non-parametric regression on the dataset diabetes, using
# a smoothing splines fit from smooth.spline
# selecting automatically the degrees of freedom
# report the CV value that is calculated internally

# not specifing anything but cv = TRUE
ss_fit <- smooth.spline(reg$x, reg$y, cv = TRUE)
ss_fit$cv.crit

# e) 

# perform a constant fit of the data, ie. neglect the x-values and calculate the mean 
# of the y-values

# calculate the corresponding CV-value for that estimator
loocv_const <- rep(0, length(x))

for(i in 1:length(x)){
  test_y <- y[i]
  training_y <- y[-i]
  
  fitted_const <- mean(training_y)
  loocv_const[i] <- (test_y - fitted_const) ^ 2
}

loocv_const <- mean(loocv_const)
loocv_const

# f)

# which of the 5 estimators has the lowest generalization error?

loocv_const
loocv_score # NW
loocvlp_score
loocvss_score
ss_fit$cv.crit

# the method which includes an optimal choice of a parameter by optimization of the
# CV score is no longer ok to consider in the comparison
# because the outcome depends on all cross-validations and it is no longer
# independent of the point left out at that moment

# 2.

# we want to construct different boostrap CIs
# we want to check the empirical coverage for the different sample sizes

# empirical coverage: % of CIs that contain the true value of all CIs

# a)

# we want to estimate the trimmed mean theta of the gamma distribution
# the 10% largest and smallest observations are trimmed

# approximate theta based on a very large sample

# a) done in the skeleton

set.seed(3)
(true.par <- mean(rgamma(100000000, shape = 2, rate = 2), trim = 0.1))

# b)

# construct a sample of size 40 from the given Gamma distribution and estimate
# theta using the sample trimmed mean theta_hat

set.seed(1)
# 40 obsv sampled from rgamma
sample40 <- rgamma(40, shape = 2, rate = 2)
theta_hat <- mean(sample40, trim = 0.1)

# c) construct 3 95%-boostrap CIs for the trimmed mean theta
# based on the sample created in task b)
library(boot)

# creating the statistic function
# trimmed mean = tm
tm <- function(x, ind){mean(x[ind], trim = 0.1)}

# calculating the bootstrap estimates
res_boot <- boot(data = sample40, statistic = tm, R = 1000, sim = "ordinary")

# calculating the boostrap intervals

# first argument has to be the result 
# of a boostrap calculation
# perc = the quantile boostrap
# norm = the normal approximation
# basic = reversed quantile
intervals <- boot.ci(boot.out = res_boot, conf = 0.95, type = c("basic", "norm", "perc"))

# d)

# to investigate the performance of the different CIs
# we do a small simulation study
# we want to simulate 200 new datasets of 40 obsv each
# we construct boostrap CIs on 500 boostrap replicates
# for each type of CI, compute the % of CIs that do not contain theta
# compute the % of times theta < CI[l]
# compute the % of times theta > CI[u]

bootstrap_interval_basic <- matrix(0, 200, 2)
bootstrap_interval_norm <- matrix(0, 200, 2)
bootstrap_interval_perc <- matrix(0, 200, 2)

for(i in 1:200){
  data40 <- rgamma(40, shape = 2, rate = 2)
  res_boot <- boot(data = data40, statistic = tm, sim = "ordinary", R = 500)
  intervals <- boot.ci(res_boot, conf = 0.95, type = c("basic", "norm", "perc"))
  bootstrap_interval_basic[i, 1] <- intervals$basic[2]
  bootstrap_interval_basic[i, 2] <- intervals$basic[3]
  bootstrap_interval_norm[i, 1] <- intervals$norm[2]
  bootstrap_interval_norm[i, 2] <- intervals$norm[3]
  bootstrap_interval_perc[i, 1] <- intervals$perc[2]
  bootstrap_interval_perc[i, 2] <- intervals$perc[3]
}

true.par > bootstrap_interval_basic[1]
