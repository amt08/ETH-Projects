### 1.

# we apply double bootstrapping
# we re-create the setup
set.seed(3)
# this is like the true theta because we base our estimation on a very large sample size
(true.par <- mean(rgamma(100000000, shape = 2, rate = 2), trim = 0.1))

set.seed(1)
# estimated theta
sample40 <- rgamma(40, shape = 2, rate = 2)
theta_hat <- mean(sample40, trim = 0.1)

# a) 

# we want to construct 90% boostrap CIs for the trimmed mean theta based on the sample created

# CIs considered:
# 1. reversed quantile
# 2. reversed quantile with corrected level alpha', where alpha' is an ADJUSTED level such that
# we have 90% coverage of theta_hat in CIs calculated on a second layer bootstrap
# because in the 2nd level boostrap we care about theta_hat being in the correct interval

# we use M = 50 samples for the outer boostrap and B = 500 for every second layer boostrap
# in the end, we calculate our bootstrap CI based on B = 500 samples with nominal level alpha 
# (for the reversed quantile) and alpha' (for the interval corrected by the double bootstrap).


### 2.

# apply parametric bootstrap by hand and compare it with the output of the package boot
# theta = the 75% percentile of the variable boogg

boogg <- c(17, 26, 12, 6, 12, 18, 10, 12, 26, 13, 13, 11, 12, 35, 7, 21, 44, 10, 21)

# a)

# 1D scatterplots
stripchart(boogg, method = "stack")

# the MLE for shape and rate if we fit a Gamma distribution to the data:
require(MASS)

?fitdistr

fit.gamma <- fitdistr(boogg, "gamma")
fit.gamma$estimate

# b)

# plot a histogram of the variable boogg and add the density curve of the Gamma distr
# with the estimated shape and rate (ie. the MLE)

hist(boogg, freq = FALSE, breaks = 50)
lines(x = seq(from = 0, to = max(boogg), by = 0.4),
      y = dgamma(x = seq(from = 0, to = max(boogg), by = 0.4), 
                 shape = fit.gamma$estimate["shape"],
                 rate = fit.gamma$estimate["rate"]))

# c)

# generate 1000 bootstrap samples using parametric bootstrap
# by hand and compute theta_hat*_1, ... theta_hat*_1000
set.seed(987)
theta_hat <- quantile(boogg, 0.75)
theta_hat_star <- rep(0, 1000)

for( i in 1:1000){
  # generate data with gamma distribution and MLE
  sample <- rgamma(length(boogg), shape = fit.gamma$estimate["shape"], rate = fit.gamma$estimate["rate"])
  theta_hat_star[i] <- quantile(sample, 0.75)
}

hist(theta_hat_star, breaks = 20)
abline(v = quantile(boogg, probs = 0.75), col = 2, lwd = 2)

# d)

# construct the following bootstrap CIs for theta by hand based on the generated bootstrap samples

# quantile CI
alpha <- 0.05
CI_quantile <- c(quantile(theta_hat_star, alpha/2), quantile(theta_hat_star, 1 - alpha/2))
CI_quantile

# OR
CI.q1 <- quantile(theta_hat_star, probs = c(0.025, 0.975))
CI.q1

# normal approximation
theta_hat_star_mean <- mean(theta_hat_star)
stddev <- sqrt(var(theta_hat_star - theta_hat_star_mean))
2 * theta_hat - theta_hat_star_mean + c(-qnorm(1 - alpha/2),
                                        qnorm(1 - alpha/2)) * stddev

# OR
(CI.n1 <- c(2*theta_hat - mean(theta_hat_star) - qnorm(0.975) * sd(theta_hat_star),
            2*theta_hat - mean(theta_hat_star) + qnorm(0.975) * sd(theta_hat_star)))


# reversed quantile
c(theta_hat - quantile(theta_hat_star - theta_hat, 1 - alpha/2),
  theta_hat - quantile(theta_hat_star - theta_hat, alpha/2))

# OR:
CI.r1 <- theta_hat - quantile(theta_hat_star - theta_hat, probs = c(0.975, 0.025))
CI.r1

# e)

# conduct the same types of CIs using the package boot and compare them to the CIs computed by hand

require(boot)
set.seed(2020)

# statistic for the parametric bootstrap
perc75 <- function(data){
  quantile(data, probs = 0.75)
}

#generation function for parametric bootstrap
perc75.gen <- function(data, mle){
  sample <- rgamma(data, shape = mle[1], rate = mle[2])
  return(sample)
}

boot_res <- boot(boogg, statistic = perc75, sim = "parametric", ran.gen = perc75.gen,
     mle = c(fit.gamma$estimate["shape"], fit.gamma$estimate["rate"]), R = 1000)

boot.ci(boot_res, type = c("basic", "norm", "perc"))

# plotting theta_hat*
hist(boot_res$t, breaks = 20)
abline(v = theta_hat, col = 2, lwd = 2)

# f)

set.seed(2020)

# compare the parametric bootstrap CIs to the CIs using the non-parametric bootstrap calculated by hand
theta_hat_star_nonp <- rep(1000,0)
for(i in 1:1000){
  sample <- sample(boogg, length(boogg), replace=TRUE) # sample with replacement from boogg
  theta_hat_star_nonp[i] <- quantile(sample, probs = 0.75)
}

R <- 1000

for (i in 1:R){
  ind <- sample(1:length(boogg), length(boogg), replace=TRUE)
  theta_hat_star_nonp[i] <- quantile(boogg[ind], probs = 0.75) 
}

# CI's

# quantile
quantile(theta_hat_star_nonp, probs = c(0.025, 0.975))

# normal approximation
2 * theta_hat - mean(theta_hat_star_nonp) +
  c(-qnorm(0.975), qnorm(0.975)) * sd(theta_hat_star_nonp - mean(theta_hat_star_nonp))

# reversed quantile
theta_hat - quantile(theta_hat_star_nonp - theta_hat, probs = c(0.975, 0.025))

