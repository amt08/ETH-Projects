### Code skeleton for Series 6, Exercise 1
### Please replace all triple question marks by some
### code. 

set.seed(3)
# true theta (the trimmed mean)
(true.par <- mean(rgamma(100000000, shape = 2, rate = 2), trim = 0.1))

set.seed(1)
# the training data
sample40 <- rgamma(n = 40, shape = 2, rate = 2)
# theta hat
mean(sample40, trim = 0.1)


###################################################
### TASK a)
###################################################
require("boot")
tm <- function(x, ind) {mean(x[ind], trim = 0.1)}

# nominal levels at which we want to evaluate coverage (1-alpha)
# we vary alpha
levels <- c(round(seq(0.7, 0.99, 0.01), 2), round(seq(0.992, 0.996, 0.002), 3))
nl <- length(levels) # 33 (1-alpha)s
# desired coverage
conf <- 0.9

# outer layer
M <- 50
# inner layer and final estimate
B <- 500

# function called by the outer bootstrap
# estimates confidence intervals on second layer of bootstrap
boot.outer.fnn <- function(data, ind, B){
  # inner level bootstrap, ie. second level
  # B = 500 bootstrap replicates for the inner level bootstrap
  # boot.outer.fnn is the statistic that is called when we do bootstrapping on the outer level
  # tm is the statistic for the inner level bootstrap
  res.boot.inner <- boot(data = data[ind], statistic = tm, R = B, 
                         sim = "ordinary")
  # for multiple 1-alphas -> conf = levels that we sequenced
  bci <- boot.ci(res.boot.inner, conf = levels, type = c("basic")) 
  ci <- bci[["basic"]][,4:5] # extracting the reversed quantile CI
  
  # res.boot.inner$t0 returns theta_hat*, then we get all lower bounds and all upper bounds
  out <- c(res.boot.inner$t0, ci[,1], ci[,2]) 
  names(out) <- c("tm", paste("lower", levels, sep="_"), paste("upper", levels, sep="_"))
  out
}

# function to find 1 - alpha'
get.level.prime <- function(res.boot.outer, conf){
  # thh is theta_hat
  thh <- res.boot.outer$t0[1] # extract theta_hat
  
  lower <- res.boot.outer$t[, 2:(nl+1)]
  upper <- res.boot.outer$t[, (nl+2):(2*nl+1)]
  
  # 0 / 1 result if theta_hat is included or not in the inner level bootstrap interval
  included <- lower <= thh & thh <= upper
  # change name of included to match each simulated nominal level 1-alpha
  colnames(included) <- levels 
  
  cover <- apply(included, 2, mean)
  
  # if the coverage >= the desired coverage => 1-alpha' = min(of all 1-alphas where the coverage >= desired coverage)
  # 1-alpha' is the smallest value which gives the smallest coverage larger than the desired coverage
  if(max(cover) >= conf){
    # get the smallest level whose coverage is greater than the desired coverage
    level.prime <- min(levels[cover >= conf])
  } else {
    # if we are unable to receive the desired coverage
    level.prime <- NA
  }
  level.prime
}

# get the confidence interval on the nominal levels 1-alpha and 1-alpha' based on B samples
# this is the final step in the double boostrap process -> check the seminar
get.ci <- function(res.boot.outer, conf, level.prime) {
  if(!is.na(level.prime)) {
    # 2 nominal levels: 1- alpha si 1-alpha'
    level.indices <- c(which(levels == conf), which(levels == level.prime))
  } else {
    # if we don't find any applicable level, we cheat and use the highest available
    level.indices <- c(which(levels == conf), nl)
  }
  
  # LB for the CI, where res.boot.outer$t0[1] is theta_hat
  lower0 <- res.boot.outer$t0[2:(nl + 1)]
  upper0 <- res.boot.outer$t0[(nl + 2):(2*nl + 1)]
  
  # we have a basic CI built with 1-alpha
  # and a double CI built with 1-alpha'
  matrix(cbind(lower0[level.indices], upper0[level.indices]), nrow = 2,
         dimnames =  list(c("basic", "double"), c("lower", "upper")))
}

# R => The number of bootstrap replicates for the outer level, M times we extract samples for outer level
# B => In the end, we calculate our bootstrap confidence interval based on B = 500 samples
# the statistic is the boot.outer.fnn function
res.boot.outer <-  boot(sample40, statistic = boot.outer.fnn, R = M, B = B)
(level.prime <- get.level.prime(res.boot.outer, conf)) # conf is the desired coverage
get.ci(res.boot.outer, conf, level.prime)


###################################################
### TASK b, c)
###################################################

##' Checks if a confidence interval contains the true parameter (separately 
##' for the lower and the upper end)
##'
##' @param ci: Output of the function boot.ci which contains CIs
##' @param ty: Type of confidence interval
##' @param true.par: True parameter
##'                    
##' @return Vector with two elements where first one corresponds to the lower
##'         end and the second to the upper end of the confidence interval. 
##'         If the CI is [CI_l, CI_u], the first element is 1 if theta < CI_l
##'         and 0 otherwise. The second element is 1 if theta > CI_u and 0
##'         otherwise.
check_ci <- function(ci, ty, true.par) {
  # Get confidence interval of type ty from object ci
  lower.upper <- ci[ty,]
  
  res <- if (true.par < lower.upper[1]) {
    c(1, 0)
  } else if (true.par > lower.upper[2]) {
    c(0, 1)
  } else {
    c(0, 0)
  }
  names(res) <- c("lower", "upper")
  
  return(res)
}

##' Runs one simulation run, i.e. creates new data set, calculates bootstrap
##' CIs, and checks if true parameter is contained.
##'
##' @param n: Size of sample
##' @param true.par: True parameter
##' @param B: Number of bootstrap replicates in inner bootstrap
##' @param M: Number of bootstrap replicates in outer bootstrap
##' @param conf: desired coverage
##' @param type: Type of bootstrap CIs, only "basic" or "double"
##'                    
##' @return A vector containing the result of the function check_ci for each 
##'         of the confidence intervals and the estimated 1-alpha'
##'         
##'   # here we only do 1 simulation
do_sim <- function(n, true.par, B = 500, M = 50, conf = 0.9,
                   type = c("basic", "double")) {
  # Generate the data
  x <- rgamma(n = n, shape = 2, rate = 2)
  # Evaluate the double bootstrap
  # M = # of iterations, B = nr de bootstrap replicates
  res.boot.outer <-  boot(x, statistic = boot.outer.fnn, R = M, B = B)
  # find 1 - alpha'
  level.prime <- get.level.prime(res.boot.outer, conf)
  # find CIs at level 1-alpha and 1-alpha'
  res.ci <- get.ci(res.boot.outer, conf, level.prime)
  
  # Check if CIs contain true.par
  res <- vector(mode = "integer", length = 0)
  for (ty in type) {
    res <- c(res, check_ci(ci = res.ci, ty = ty, true.par = true.par))
    names(res)[(length(res) - 1):length(res)] <- 
      paste(c(ty, ty), c("lower", "upper"), sep = "_") #add names in the format 
    #'type_lower' and 'type_upper'
  }
  # store 1 - alpha' as well
  res <- c(res, level.prime)
  names(res)[length(res)] <- "level.prime"
  
  return(res)
}

##########################
### Run simulation     ###
##########################
set.seed(22)
require("boot")
sample.size <- c(10, 40, 160, 640)
n.sim <- 200
type <- c("basic", "double")

# The object RES stores the results, i.e. each row corresponds
# to the non-coverage rate for the lower and upper ends of the 
# confidence intervals, i.e. the percentage of times that theta < CI_l 
# and the percentage of times that theta > CI_u, if the CI is 
# denoted by (CI_l, CI_u). Further we add one column for the median of the estimated 1 - alpha',
# one for the fraction of times we could not find 1 - alpha' and one for n. 
RES <- matrix(NA, nrow = length(sample.size), ncol = length(type) * 2 + 3)
colnames(RES) <- c(paste(rep(type, each = 2), 
                         rep(c("lower", "upper"), times = length(type)), 
                         sep = "_"), "median_level_prime", "NA_fraction", "n")

for (j in 1:length(sample.size)) {
  n <- sample.size[j]
  cat("n =",  n ,"\n")
  
  # The object res.sim stores the results, i.e. each row corresponds
  # to the output of the function do_sim. This means that each row contains 0
  # and 1 encoding whether the true parameter was inside the CI or outside
  # as well as 1 - alpha'
  res.sim <- matrix(NA, nrow = n.sim, ncol = length(type) * 2 + 1)
  for (i in 1:n.sim) {
    cat("."); if(i %% 20 == 0) cat(i ,"\n")
    # Simulate one data-set
    res.sim[i, ] <- do_sim(n = n, true.par = true.par, type = type)
  }
  
  # Compute the upper and lower non-coverage rate, the median of 1 - alpha' 
  # and the fraction of simulation runs, where we did not find 1 - alpha'
  
  # the non-coverage rate for each sample size and the median of 1-alpha's from the [sample size] simulations
  RES[j, ] <- c(apply(res.sim[,1:(length(type) * 2)], 2, mean),
                median(res.sim[,(length(type) * 2) + 1 ], na.rm = TRUE),
                # fraction of times we could not find 1-alpha', ie. when col of res.sim[,6] is na
                mean(is.na(res.sim[,(length(type) * 2) + 1 ])), n)
}

# getting the max non-coverage from the matrix
y.lim <- max(RES[, 1:(length(type) * 2)])

# Plot of lower non-coverage
plot(basic_lower ~ n, data = RES, col = 1, pch = 1, ylim = c(0, y.lim), 
     log = "x", ylab = "One-sided non-coverage", 
     main = "Non-coverage of the lower end of the CIs.") 
points(double_lower ~ n, data = RES, col = 2, pch = 2, xlog = TRUE)
lines(basic_lower ~ n, data = RES, col = 1, lty = 1, xlog = TRUE)
lines(double_lower ~ n, data = RES, col = 2, lty = 2, xlog = TRUE)
abline(h = 0.05, lty = 5)
legend("topright", legend = c("reversed", "double bootstrap"), 
       pch = 1:2, lty = 1:2, col = 1:2)

# do it analogously for the upper non-coverage
# Plot of upper non-coverage
plot(basic_upper ~ n, data = RES, col = 1, pch = 1, ylim = c(0, y.lim), 
     log = "x", ylab = "One-sided non-coverage", 
     main = "Non-coverage of the upper end of the CIs.") 
points(double_upper ~ n, data = RES, col = 2, pch = 2, xlog = TRUE)
lines(basic_upper ~ n, data = RES, col = 1, lty = 1, xlog = TRUE)
lines(double_upper ~ n, data = RES, col = 2, lty = 2, xlog = TRUE)
abline(h = 0.05, lty = 5)
legend("topright", legend = c("reversed", "double bootstrap"), 
       pch = 1:2, lty = 1:2, col = 1:2)
