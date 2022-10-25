## 1

# a)

x <- seq(-1, 1, length = 101)
nrep <- 1000

set.seed(79)

# y_hat / m_hat is a matrix with 101 rows and 1000 cols

estnw <- estlp <- estss <- matrix(0, nrow = 101, ncol = nrep)

# the standard error is a matrix for each x and for each simulation
senw <- selp <- sess <- matrix(0, nrow = 101, ncol = nrep)

# NW -> h = 0.2
# span = 0.2971339 for LP estimator (loess)
# spar = 0.623396 for SS (smooth.spline)

m <- function(x) return(x + 4 * cos(7 * x))

for(i in 1:nrep){
  # errors have the same length as y
  y <- m(x) + rnorm(length(x))
  
  # get estimates for the mean function m(x) in the points
  # given by the vector x
  
  # E[Y/x] = m_hat(x)
  
  estnw[, i] <- ksmooth(x, y, kernel = "normal", bandwidth = 0.2, x.points = x)$y
  estlp[, i] <- predict(loess(y ~ x, span = 0.2971339), newdata = x)
  estss[, i] <- predict(smooth.spline(x, y = y, spar = 0.623396), x = x)$y
 
  # calculate the estimated standard errors
  # sigma_e = RSS / n - df
  # RSS = sum((y - y_hat)^2)
  
  # sigma epsilon scalar
  sigma2nw <- sum((y - estnw[, i]) ^ 2) / (length(y) - sum(diag(snw)))
  # the standard errors for each simulation, for each x
  senw[, i] <- sqrt(sigma2nw * diag(snw %*% t(snw)))
  
  sigma2lp <- sum((y - estlp[, i]) ^ 2) / (length(y) - sum(diag(slp)))
  selp[, i] <- sqrt(sigma2lp * diag(slp %*% t(slp)))
  
  sigma2ss <- sum((y - estss[, i]) ^ 2) / length(y) - sum(diag(sss))
  sess[, i] <- sqrt(sigma2ss * diag(sss %*% t(sss)))
  
}

true_m <- m(x)

mean_nw <- apply(estnw, 1, mean )
mean_lp <- apply(estlp, 1, mean)
mean_ss <- apply(estss, 1, mean)

# empirical bias squared
bias_nw <- (mean_nw - true_m)^2
bias_lp <- (mean_lp - true_m)^2
bias_ss <- (mean_ss - true_m)^2

bias <- matrix(c(bias_nw, bias_lp, bias_ss), nrow=101, ncol = 3)
matplot(x, bias, type = 'l', lty = c(1:3), col = c(1:3))
rug(x)
legend('topright', lty = c(1:3), c('nw', 'lp', 'ss'), col = c(1:3))


# variance of m_hat(xi)
var_nw <- apply(estnw, 1, var)
var_lp <- apply(estlp, 1, var)
var_ss <- apply(estss, 1, var)

var <- matrix(c(var_nw, var_lp, var_ss), nrow = 101, ncol = 3)
matplot(var, type = 'l')

# mse of m_hat(xi)
mse_nw <- bias_nw + var_nw
mse_lp <- bias_lp + var_lp
mse_ss <- bias_ss + var_ss

mse <- matrix(c(mse_nw, mse_lp, mse_ss), nrow = 101, ncol = 3)

matplot(mse, type = 'l')

# plotting the true function
matplot(x, true_m, type = 'l')

matplot(x, estnw, type = "l", col = "grey")


# b)

# calculate the estimated standard error for each simulation run, x-value and estimator

# hat matrix for the NW estimator (nxn)
snw <- matrix(0, nrow = 101, ncol = 101)
identity_matrix <- diag(101)
for(j in 1:101){
  # one col from the identity matrix where everything is 0 and the jth position is 1
  y <- identity_matrix[, j]
  snw[, j] <- ksmooth(x, y, kernel = "normal", bandwidth = 0.2, x.points = x)$y
}

# S hat matrix for local polynomial
slp <- matrix(0, nrow = 101, ncol = 101)
identity_matrix <- diag(101)

for(j in 1:length(y)){
  y <- identity_matrix[, j]
  slp[, j] <- predict(loess(y ~ x, span = 0.2971339), newdata = x)
}

# S hat matrix for smoothing splines
sss <- matrix(0, nrow = 101, ncol = 101)
identity_matrix <- diag(101)
for(j in 1:length(y)){
  y <- identity_matrix[, j]
  sss[, j] <- predict(smooth.spline(x = x, y = y, spar =  0.623396), x = x)$y
}

# Var_hat(m_hat(xi)) = sigma_epsilon_hat^2 * (SStranspose)ii

# how many times out of 1000 simulations, does the pointwise CI at x = 0.5
# contain the true value m(0.5), namely what is the coverage rate?

true_pointwise <- m(0.5)
true_pointwise

# take the index of x at 0.5
x_index <- which(x == 0.5)

estnw[x_index,] # all m_hats la 0.5

coverage <- function(estimator, index, se){
  res <- rep(NA, length(x))
  for(i in 1:1000){
    lb <- estimator[index, i] - se[index, i] * 1.96
    ub <- estimator[index, i] + se[index, i] * 1.96
    res[i] <- (lb <= true_pointwise) * (ub >= true_pointwise)
  }
  return(res)
}

sum(coverage(estnw, x_index, senw))
sum(coverage(estlp, x_index, selp))
sum(coverage(estss, x_index, sess))

# how often does the confidence BAND for ALL points xi simultaneously contain all true values
all_true <- m(x)

simultaneous <- function(est, se){
  result <- rep(NA, 1000)
  for(i in 1:1000){
    lb <- est[, i] - se[, i] * 1.96
    ub <- est[, i] + se[, i] * 1.96
    result[i] <- all((lb <= all_true) == TRUE) * all((ub >= all_true) == TRUE)
  }
  return(result)
}

sum(simultaneous(estnw, senw))
sum(simultaneous(estlp, selp))
sum(simultaneous(estss, sess))