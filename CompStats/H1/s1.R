## ex 4

### a)

## simple linear regression
set.seed(21)

# generates equidistant x-values
x <- seq(1,40,1)
y <- 2*x+1+5*rnorm(length(x))

# fit of the linear regression
reg <- lm(y~x) 
summary(reg)
plot(x,y)

# draws regression line
abline(reg)

###########

set.seed(21)

n <- 100
x <- seq(1,40,1)
coefs <- rep(NA, n)

for (i in 1:n){
  y <- 2*x+1+5*rnorm(length(x))
  reg <- lm(y~x)
  coefs[i] <- coef(reg)[2]
}

## b)

#add the normal density of the theoretically true distribution of the slopes to the histogram
# B_hat ~ N(B, sigma2 * (XtX)-1)

new_x <- matrix(c(rep(1, length(x)), x), nrow = length(x), ncol = 2)
xtx_minus1 <- solve(t(new_x) %*% new_x)
hist(coefs, freq = FALSE)
lines(seq(1.8,2.3,by=0.01),dnorm(seq(1.8,2.3,by=0.01),mean=2,sd=sqrt(5^2 * xtx_minus1[2,2])))

### c)

mean_coefs <- mean(coefs)
emp_stdev <- sqrt(var(coefs))

## or:

summary(coefs)
sd(coefs)

### d)

set.seed(21)

n <- 100
x <- seq(1,40,1)
coefs_2 <- rep(NA, n)

for (i in 1:n){
  # here the errors are not normally distributed
  y <- 2*x+1+5*(1-rchisq(length(x), df=1))/sqrt(2)
  reg <- lm(y~x)
  coefs_2[i] <- coef(reg)[2]
}

hist(coefs_2, freq=FALSE)
lines(seq(1.7,2.3,by=0.01),dnorm(seq(1.7,2.3,by=0.01),mean=2,sd=sqrt(5^2 * xtx_minus1[2,2])))

#### ex 5

# a)

airline <- scan("http://stat.ethz.ch/Teaching/Datasets/airline.dat")
airline

par(mfrow = c(1, 2))
t <- ts(airline, start = c(1949, 1), frequency = 12)
plot(c(time(t)), airline, type = 'l', xlab= "Time")

# data has trend and seasonality

# b)

plot(log(airline), type = "l")

# c)

f1 <- 1:144
f2 <- rep(c(1, rep(0, 11)), 12)
f3 <- rep(c(0, 1, rep(0, 10)), 12)
f3 <- rep(c(0,1,rep(0,10)),12) # effect February
f4 <- rep(c(0,0,1,rep(0,9)), 12)
f5 <- rep(c(0,0,0,1,rep(0,8)),12)
f6 <- rep(c(0,0,0,0,1,rep(0,7)),12)
f7 <- rep(c(0,0,0,0,0,1,rep(0,6)),12)
f8 <- rep(c(0,0,0,0,0,0,1,rep(0,5)),12)
f9 <- rep(c(0,0,0,0,0,0,0,1,rep(0,4)),12)
f10 <- rep(c(rep(0,8),1,rep(0,3)),12)
f11 <- rep(c(rep(0,9),1,rep(0,2)),12)
f12 <- rep(c(rep(0,10),1,rep(0,1)),12)
f13 <- rep(c(rep(0,11),1),12)

# d)
reg1 <- lm(log(airline) ~ f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9 + f10 + f11 + f12 + f13 - 1)
summary(reg1)
par(mfrow = c(1, 1))

time <- 1:144
month <- as.factor(rep(month.name, 12))
reg2 <- lm(log(airline) ~ time + month - 1)
summary(reg2)


# plot the fitted values and residuals against time
par(mfrow = c(2,1))
plot(time, residuals(reg2), type = 'l')
plot(time, fitted(reg2), type = 'l')