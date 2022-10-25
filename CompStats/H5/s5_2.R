### Code skeleton for Series 5, Exercise 2
### Please replace all triple question marks by some
### code. 

###################################################
### TASK a)
###################################################
set.seed(3)
(true.par <- mean(rgamma(100000000, shape = 2, rate = 2), trim = 0.1))

###################################################
### TASK b)
###################################################
set.seed(1)
sample40 <- rgamma(n = 40, shape = 2, rate = 2)
mean(sample40, trim = 0.1)
 
###################################################
### TASK c)
###################################################
require("boot")
tm <- function(x, ind) {mean(x[ind], trim = 0.1)}

res.boot <- boot(data = sample40, statistic = tm, R = 10000, 
                 sim = "ordinary")
result <- boot.ci(res.boot, conf = 0.95,
        type = c("basic", "norm", "perc"))

###################################################
### TASK d)
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
  lower.upper <- switch(ty,
                        # if norm extract 2:3 from CI[["normal"]]
                         "norm" = ci[["normal"]][2:3],
                         "perc" = ci[["percent"]][4:5], 
                         "basic" = ci[["basic"]][4:5]
  )
  
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
##' @param R: Number of bootstrap replicates
##' @param type: Type of bootstrap CIs, see function boot.ci
##'                    
##' @return A vector containing the result of the function check_ci for each 
##'         of the confidence intervals
do_sim <- function(n, true.par, R = 500, 
                   type = c("basic", "norm", "perc")) {
  # Generate the data
  x <- rgamma(n = n, shape = 2, rate = 2)
  # Construct the CIs for the trimmed mean
  res.boot <- boot(data = x, statistic = tm, R = R, sim = "ordinary")
  res.ci <- boot.ci(res.boot, conf = 0.95, type = type)
  
  # Check if CIs contain true.par
  res <- vector(mode = "integer", length = 0)
  
  # for each interval type
  for (ty in type) {
    # apply check_ci which returns a binary vector that is concatenated with res
    # for each interval type it returns a 0/1 vector
    res <- c(res, check_ci(ci = res.ci, ty = ty, true.par = true.par)) # check.ci returns c(0,1) or c(0,0) or c(1,0)
    # rename cols of res
    names(res)[(length(res) - 1):length(res)] <- 
      paste(c(ty, ty), c("lower", "upper"), sep = "_") #add names in the format 
                                                       #'type_lower' and 'type_upper'
  }

  return(res)
}

##########################
### Run simulation     ###
##########################
set.seed(22)
require("boot")
sample.size <- c(10, 40, 160, 640)
n.sim <- 200
type <- c("basic", "norm", "perc")

# The object RES stores the results, i.e. each row corresponds
# to the non-coverage rate for the lower and upper ends of the 
# confidence intervals, i.e. the percentage of times that theta < CI_l 
# and the percentage of times that theta > CI_u, if the CI is 
# denoted by (CI_l, CI_u). The last column of RES corresponds to 
# the number of observations. 

RES <- matrix(NA, nrow = length(sample.size), ncol = length(type) * 2 + 1)
colnames(RES) <- c(paste(rep(type, each = 2), 
                         rep(c("lower", "upper"), times = length(type)), 
                         sep = "_"), "n")

for (j in 1:length(sample.size)) {
  # for each sample size in the sample size vector
  n <- sample.size[j]
  # The object res.sim stores the results, i.e. each row corresponds
  # to the output of the function do_sim. This means that each row contains 0
  # and 1 encoding whether the true parameter was inside the CI or outside. 
  # Also see the function check_ci.
  
  # res.sim stores the binary vectors for all the 200 simulations for each sample size
  res.sim <- matrix(NA, nrow = n.sim, ncol = length(type) * 2)
  for (i in 1:nrow(res.sim)) {
    # Compute CIs and check if true.par is contained in them
    res.sim[i, ] <- do_sim(n = n, true.par = true.par, type = type)
  }
  # Compute the upper and lower non-coverage rate
  RES[j, ] <- c(apply(res.sim, 2, mean), n)
}


###################################################
### TASK e)
###################################################
# maximum of the non-coverage rates
y.lim <- max(RES[, -ncol(RES)]) # excludes the last column which just gives the sample size

# Plot of lower non-coverage
plot(basic_lower ~ n, data = RES, col = 1, pch = 1, ylim = c(0, y.lim), 
     log = "x", ylab = "One-sided non-coverage", 
     main = "Non-coverage of the lower end of the CIs.") 
points(norm_lower ~ n, data = RES, col = 2, pch = 2, xlog = TRUE)
points(perc_lower ~ n, data = RES, col = 3, pch = 3, xlog = TRUE)
lines(basic_lower ~ n, data = RES, col = 1, lty = 1, xlog = TRUE)
lines(norm_lower ~ n, data = RES, col = 2, lty = 2, xlog = TRUE)
lines(perc_lower ~ n, data = RES, col = 3, lty = 3, xlog = TRUE)
# plot the lower part left from the 95% CI, ie. 2.5%
abline(h = 0.025, lty = 5)
legend("topright", legend = c("reversed", "normal", "quantile"), 
       pch = 1:3, lty = 1:3, col = 1:3)

# do it analogously for the upper non-coverage
plot(basic_upper ~ n, data = RES, col = 1, pch = 1, ylim = c(0, y.lim), 
      log = "x", ylab = "One-sided non-coverage", 
      main = "Non-coverage of the upper end of the CIs.") 
points(norm_upper ~ n, data = RES, col = 2, pch = 2, xlog = TRUE)
points(perc_upper ~ n, data = RES, col = 3, pch = 3, xlog = TRUE)
lines(basic_upper ~ n, data = RES, col = 1, lty = 1, xlog = TRUE)
lines(norm_upper ~ n, data = RES, col = 2, lty = 2, xlog = TRUE)
lines(perc_upper ~ n, data = RES, col = 3, lty = 3, xlog = TRUE)
# plot the upper part left from the 95% CI, ie. 2.5%
abline(h = 0.025, lty = 5)
legend("topright", legend = c("reversed", "normal", "quantile"), 
       pch = 1:3, lty = 1:3, col = 1:3)
