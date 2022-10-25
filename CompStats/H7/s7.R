# 2.

# we want to use a bootstrap with LDA and QDA on the iris data
# by just using petal information:

Iris <- iris[,c("Petal.Length","Petal.Width","Species")]

require(MASS)
set.seed(16)

# 3.

heart <- read.table("http://stat.ethz.ch/Teaching/Datasets/heart.dat", header = TRUE)

# a) solved on paper

# b)

# write an R function that calculates the negative log-likelihood derived in a)
neg.ll <- function(beta, data){
  return(- sum(log(choose(data$m, data$N)) + data$N * (beta[1] + beta[2] * data$age) - data$m * log(1 + exp(beta[1] + beta[2] * data$age))))
}

# make a contour plot of the NLL
beta0.grid <- seq(-10, 10, length = 101)
beta1.grid <- seq(-1, 1, length = 101)

neg.ll.values <- matrix(0, nrow = length(beta0.grid), ncol = length(beta1.grid))

for(i in 1:length(beta0.grid)){
  for(j in 1:length(beta1.grid)){
    neg.ll.values[i, j] <- neg.ll(c(beta0.grid[i], beta1.grid[j]), heart)
  }
}

contour(beta0.grid, beta1.grid, neg.ll.values,
        levels=c(seq(50,200,50),seq(300,2300,200)))

# c)

# estimate the parameters beta0 and beta1 of the model function using the function glm in R

# binomial responses in GLM are recorded as a two-column matrix: number of successses and number of failures

fit <- glm(cbind(N, m-N) ~ age, family = "binomial", data = heart)
summary(fit)

# compare the estimates from glm with estimates you get when minimizing the NLL function implemented in b)
mle <- optim(c(0, 0), neg.ll, data = heart)

mle$par # beta0_mle and beta1_mle

fit$coefficients

# d)

# plot the probability estimate against age

# at what age would you expect 10%, 20%,..., 90% of people to have symptoms of heart disease?
new.age <- 0:100

predicted <- predict(fit, newdata = data.frame(age = new.age), type = "response")

# x = age, y = prop of people having symptoms of all people in the age category
plot(heart$age, heart$N / heart$m, xlim = c(0, 100),
     ylim = c(0, 1), xlab = "Age",
     ylab = "Fraction of people with heart disease")
lines(new.age, predicted)

perc <- (1:9)/10 # 10%, 20%, ..., 90%
x.age <- (log(perc/(1-perc)) - coef(fit)[1])/coef(fit)[2] # as per formula
names(x.age) <- perc

for(n in 1:9)
  lines(c(-4, x.age[n], x.age[n]), c(perc[n], perc[n], -0.04), lty = 2)
