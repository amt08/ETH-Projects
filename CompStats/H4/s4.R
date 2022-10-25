# 1

## a)

bmwr <- scan("http://stat.ethz.ch/Teaching/Datasets/bmw.dat")

bmwpt <- cumsum(c(40, bmwr))

# log returns
bmwlr <- log(bmwpt[-1] / bmwpt[-length(bmwpt)])

# bmwr = p[t] - p[t-1]
# bmwpt = p[t] / p[t-1]
# bmlr = log(bmwpt)

par(mfrow=c(3,1))
plot.ts(bmwr, main = 'BMW returns')
plot.ts(bmwpt, main = 'BMW price')
plot.ts(bmwlr, main = 'BMW log-returns')

# b)

par(mfrow=c(2, 1))
acf(bmwlr)
acf(bmwlr ^ 2)

# c)

# fit the data using the nonparametric regression methods NW, local polynomial
# and smoothing splines for the regression function v

# degree of smoothness
# loess => span = number between 0 and 1 which looks at the fraction of data
# to include in the support (x) of the kernel, we use 0.75

# smoothing splines
# smooth.spline, we can define the smoothness via the degrees of freedom
# df is the parameter in the function
# dfs can be computed via the trace of the hat matrix
# use the same degrees of freedom as in loess
# which can be recovered using fit$trace.hat (fit is the loess fitted model)

# for the ksmooth use h = 3.54

# ksmooth internally reorders its x input in increasing order
# so you will lose the time ordering. To recover it, do:

# ox <- order(x)
# fit <- ksmooth(x, y)
# fit$x <- fit$x[order(ox)]
# fit$y <- fit$y[order(ox)]

y <- bmwlr[-1] ^ 2 # X_t ^ 2
x <- bmwlr[-length(bmwlr)] # v(X_t-1)

# loess
lp <- loess(y ~ x, span = 0.75)
df_lp <- lp$trace.hat

# smooth.spline
ss <- smooth.spline(x=x, y=y, df=df_lp)

# ksmooth
# keep track of how the values of x are ordered
ox <- order(x)
nwfit <- ksmooth(x=x, y=y, kernel = "normal", bandwidth = 0.16,
                 x.points = x)
# here we reverse to the original time ordering
nwfit$x <- nwfit$x[order(ox)]
nwfit$y <- nwfit$y[order(ox)]

### checking model assumptions
lp_fitted <- fitted(lp)
lp_resid <- resid(lp)
ss_fitted <- fitted(ss)

# we need to obtain the fitted values for NW
# we want an equidistant grid of design points
# we evaluate NW at x_out
x_out <- seq(min(x), max(x), length = length(x))
nw_fitted <- ksmooth(x, y, kernel = 'normal', bandwidth = 0.16,
                     x.points = x_out)

# Plot 1, we look at the full range of x
# Look at the estimated volatility function as a function of Xt
par(mfrow = c(2, 1))
plot(x, y, main = "Fitting results (whole range of data)",
     cex = 0.6, col = "gray")
lines(x_out, nw_fitted$y, col = 1) # plotting the ksmooth
lines(x_out, predict(lp, newdata = x_out), lty = 3, col=3, 
      cex = 0.5) # plotting loess
lines(x_out, predict(ss, x = x_out)$y, lty = 5, col = 4)

legend('topright', legend = c('NW', 'LP', 'SS'),
       lty = c(1, 3, 5), col = c(1, 3, 4), cex = 0.3)

# Plot 2, we look only at the central region of the data
x_window <- quantile(x, c(0.2, 0.8))
y_window <- c(0, quantile(y, 0.95))

plot(x, y, main = 'Fitting results (central region of the data',
     cex = 0.6, col = "gray", xlim = x_window, ylim = y_window)
lines(x_out, nw_fitted$y, col = 1)
lines(x_out, predict(lp, newdata = x_out),
      lty = 3, col = 3, cex = 0.5)
lines(x_out, predict(ss, x = x_out)$y, lty = 4, col = 4)

# Now we plot time vs residual plots for the 3 smoothers
# we're not using x_out here
par(mfrow=c(3,1))
plot.ts(y-nwfit$y, main = "Residuals: ksmooth")
plot.ts(y-fitted(lp), main = "Residuals: local polynomial")
plot.ts(y-predict(ss, x=x)$y, main = "Residuals: smoothing splines")

# Last task: Look at the estimated implied volatility as a function of time
# we look at the estimated implied volatility from the model we have fitted

# this can be obtained by plotting a time series of the square root
# of the fitted value (volatility is the standard deviation, not the
# variance) e = sigma, sigma = sqrt(v(X_t-1))

par(mfrow=c(3, 1))
plot.ts(bmwlr, main = 'log-returns')
plot.ts(sqrt(fitted(lp)), main = 'estimated implied volatility (loess)')
plot.ts(sqrt(predict(ss, x = x)$y), main = 'estimated implied volatility smoothing splines')

test_fitted <- fitted(lp)
test_fitted_2 <- predict(lp, newdata = x)
test_fitted == test_fitted_2

# d)
# fit the data using the functions: 
# glkerns = kernel regression with global optimal bandwidth
# lokerns = kernel regression with local optimal bandwidth

# compare the fits

# plot the local bandwidths from lokerns and compare them
# to the global bandwidth of glkerns

# how does the bandwidth relate to the density of the data?

library(lokern)
?glkerns

# is.rand = TRUE means that X is not fixed, it's random
# hetero = TRUE means that we have heteroscedastic errors

# we fit the model and we predict on the input data
glkernsfit <- glkerns(x, y, hetero = TRUE, is.rand = TRUE, x.out = x)
lokernsfit <- lokerns(x, y, x.out = x, hetero = TRUE, is.rand = TRUE)

# fit the model and predict on the x_out grid
glkerns_grid <- glkerns(x, y, x.out = x_out, hetero = TRUE, is.rand = TRUE)
lkerns_grid <- lokerns(x, y, x.out = x_out, hetero = TRUE, is.rand = TRUE)

# Plot 1: whole range of data first
par(mfrow=c(2,1))
plot(x, y, main = "Fitting results whole range of data first", cex = 0.6, col = "gray")
lines(x_out, lkerns_grid$est, col = 1)
lines(x_out, glkerns_grid$est, col = "blue", lty = 2)

# Plot 2: looking at the fits for the central region of the data
plot(x, y, main = "Fitting results (central region of data",
     cex = 0.6, col = "gray", xlim = x_window, ylim = y_window)
lines(x_out, lkerns_grid$est, col = 1)
lines(x_out, glkerns_grid$est, col = "blue", lty = 2)

# now we look at the residual plots vs time for the smoothers
par(mfrow=c(2,1))
plot.ts(y - lokernsfit$est[order(ox)], main = "Lokerns residuals vs time")
plot.ts(y - glkernsfit$est[order(ox)], main = "Glkerns residuals vs time")

# plotting the local bandwidth from lokerns to compare them 
# with the global bandwidth from glkerns
# now we plot the bandwidths as a function of x for both lokerns and glkerns
par(mfrow=c(1,1))
plot(x_out, lkerns_grid$bandwidth, type = "l", main = "Local bandwidths for lokerns", ylab = "Bandwidth")
abline(h=glkerns_grid$bandwidth, col = 2, lty = 2)
legend('topright', legend = c("Lokerns", "Glkerns"), lty = c(1, 2), col = c(1,2), cex = 0.6)
rug(x, col = "grey")

# However at the border of the data the bandwidth is chosen smaller, which is not
# what you would expect and indicate a problem with the automatic bandwidth choice.