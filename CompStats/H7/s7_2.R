### Code skeleton for Series 7, task 2

## Read in dataset, set seed, load package
Iris <- iris[,c("Petal.Length", "Petal.Width", "Species")]
grIris <- as.integer(Iris[, "Species"])
set.seed(16)
library(MASS)

## Read n
n <- nrow(Iris)

## Utility function for plotting boundaries
predplot <- function(object, x, gr = grIris, main = "", lines.only = FALSE,
                     len = 42, colcont = "black", ...)
{
  ##  gr : the true grouping/class vector
  stopifnot(length(gr) == nrow(x))
  xp <- seq(min(x[, 1]), max(x[, 1]), length = len)
  yp <- seq(min(x[, 2]), max(x[, 2]), length = len)
  grid <- expand.grid(xp, yp)
  colnames(grid) <- colnames(x)[-3]
  Z <- predict(object, grid, ...)
  zp <- as.numeric(Z$class)
  zp <- Z$post[, 3] - pmax(Z$post[, 2], Z$post[, 1])
  if(!lines.only)
    plot(x[,1], x[,2], col = gr, pch = gr,
         main = main, xlab = colnames(x)[1], ylab = colnames(x)[2])
  contour(xp, yp, matrix(zp, len),
          add = TRUE, levels = 0, drawlabels = FALSE, col = colcont)
  zp <- Z$post[, 1] - pmax(Z$post[, 2], Z$post[, 3])
  contour(xp, yp, matrix(zp, len),
          add = TRUE, levels = 0, drawlabels = FALSE, col = colcont)
}

## Bootstrap size
B <- 1000

###################################################
### TASK a)
###################################################

## Use function lda to fit data
class_lda <- lda(x = Iris[, c("Petal.Length","Petal.Width")], grouping = grIris)

## Use function predplot to plot the boundaries
predplot(class_lda, Iris, main = "Classification with LDA")

## Use function qda to fit data
class_qda <- qda(x = Iris[, c("Petal.Length","Petal.Width")], grouping = grIris)

## Use function predplot to plot the boundaries
predplot(class_qda, Iris, main = "Classification with QDA")

###################################################
### TASKS b)
###################################################

## Create a random index matrix with either functions sample or sample.int to generate bootstrap
## Each column corresponds to the n indices of one bootstrap sample
index <- matrix(sample(x = n, size = n*B, replace = TRUE), nrow=n, ncol=B)

## Initialise the list for LDA and QDA fits
fit_lda <- vector("list", B) # 1000 lda fits
fit_qda <- vector("list", B) # 1000 qda fits


## Use both methods on the bootstrap samples
for(i in 1:B) {
  ind <- index[, i]
  fit_lda[[i]] <- lda(Iris[ind, c("Petal.Length","Petal.Width")], grouping=Iris[ind , "Species"])
  fit_qda[[i]] <- qda(Iris[ind, c("Petal.Length","Petal.Width")], grouping=Iris[ind , "Species"])
}

# Initialise the mu_hat bootstrap estimates for LDA
# B cols and 2 rows: Petal.Length and Petal.Width
# 3 mu_hat for the 3 classes
mu_hat_1 <- mu_hat_2 <- mu_hat_3 <- matrix(0, ncol = B,nrow = 2)

## Determine the mu_hat bootstrap estimates
for(i in 1:B){
  mu_hat_all <- fit_lda[[i]]$means
  mu_hat_1[, i] <- mu_hat_all[1,] # 2 means for class 1 of the Petal.Length and Petal.Width predictors
  mu_hat_2[, i] <- mu_hat_all[2,] # 2 means for class 2 of the Petal.Length and Petal.Width predictors
  mu_hat_3[, i] <- mu_hat_all[3,] # 2 means for class 3 of the Petal.Length and Petal.Width predictors
}

## Plot the bootstrapped estimators
## define a suitable plotting range
xmin <- min(Iris[, "Petal.Length"])
xmax <- max(Iris[, "Petal.Length"])
ymin <- min(Iris[, "Petal.Width"])
ymax <- max(Iris[, "Petal.Width"])
## plot the estimated mean of each group
plot(mu_hat_1[1, ], mu_hat_1[2,], xlim = c(xmin, xmax), ylim = c(ymin, ymax),
     xlab = colnames(Iris)[1], ylab = colnames(Iris)[2], pch = 4,
     main = "Bootstrap samples")
points(mu_hat_2[1, ], mu_hat_2[2, ], col = 2, pch = 4)
points(mu_hat_3[1, ], mu_hat_3[2, ], col = 3, pch = 4)


###################################################
### TASK c)
###################################################

## Plot the bootstrapped boundaries estimates with LDA
predplot(class_lda, Iris,
         main = "Bootstrapped boundaries estimates with LDA")
for(i in 1:B){
  fit <- fit_lda[[i]]
  predplot(fit, Iris, lines.only = TRUE, colcont = adjustcolor("gray", 0.25))
}

## Plot the bootstrapped boundaries estimates with QDA
predplot(class_qda, Iris,
         main= "Bootstrapped boundaries estimates with QDA")
for(i in 1:B){
  fit <- fit_qda[[i]]
  predplot(fit, Iris, lines.only = TRUE, colcont = adjustcolor("gray", 0.25))
}

###################################################
### TASK d)
###################################################
# calculate the bootstrap estimate of the generalisation error for both LDA and QDA

## Initialise the error vectors
error_lda <- rep(0, B)
error_qda <- rep(0, B)

for(i in 1:B){
  fit_L <- fit_lda[[i]]
  error_lda[i] <- mean(predict(fit_L, newdata = Iris[, c("Petal.Length", "Petal.Width")])$class != Iris[, "Species"])
  fit_Q <- fit_qda[[i]]
  error_qda[i] <- mean(predict(fit_Q, newdata = Iris[, c("Petal.Length", "Petal.Width")])$class != Iris[, "Species"])
}

## Print the error
cat("Generalized error for LDA:", format(mean(error_lda), digits=4))
cat("Generalized error for QDA:", format(mean(error_qda), digits=4))


## Plot the boxplot of the errors
boxplot(cbind(error_lda, error_qda), notch = TRUE, col = "blue",
        main = "Boxplot for boostrap estimate  of generalization error")

###################################################
### TASK e)
###################################################

# calculate the out-of-bootstrap (OOB) estimate of the generalization error for both methods, using the same loss function
## Initialize the errors
error_lda_OOB <- rep(0, B)
error_qda_OOB <- rep(0, B)

for(i in 1:B){
  ind <- index[,i]
  fit_L <- fit_lda[[i]]
  # predict on everything that hasn't been selected in the bootstrap
  error_lda_OOB[i] <- mean(predict(fit_L, newdata = Iris[-ind, c("Petal.Length", "Petal.Width")])$class !=  Iris[-ind, "Species"])
  fit_Q<- fit_qda[[i]]
  error_qda_OOB[i] <- mean(predict(fit_Q, newdata =  Iris[-ind, c("Petal.Length", "Petal.Width")])$class !=  Iris[-ind, "Species"])
}

## Print the error
cat("Generalized OOB error for LDA:", format(mean(error_lda_OOB), digits=4))
cat("Generalized OOB error for QDA:", format(mean(error_qda_OOB), digits=4))

## Plot the boxplot of the errors
boxplot(cbind(error_lda_OOB, error_qda_OOB), notch = TRUE, col = "blue",
        main = "Boxplot for boostrap estimate of generalization error")
