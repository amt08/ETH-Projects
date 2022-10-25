## Code Skeleton for Series 8, task 1

## Read in dataset, load package
Iris <- iris[,c("Petal.Length", "Petal.Width", "Species")]
grIris <- as.integer(Iris[, "Species"])

library(MASS) # lda(), qda()
library(nnet) # multinom()

predplot <- function(object, x, gr = grIris, main = "", lines.only = FALSE,
                     len = 42, colcont = "black", method, ...)
{
  ##  gr : the true grouping/class vector
  all.methods <- c("LDA", "QDA", "multinom", "dummy")
  if (!(method %in% all.methods)) stop("Illegal method: ", method)
  stopifnot(length(gr) == nrow(x))
  xp <- seq(min(x[, 1]), max(x[, 1]), length = len)
  yp <- seq(min(x[, 2]), max(x[, 2]), length = len)
  grid <- expand.grid(xp, yp)
  colnames(grid) <- colnames(x)[-3]
  if(!lines.only) # for all methods:
      plot(x[,1], x[,2], col = gr, pch = gr,
           main = main, xlab = colnames(x)[1], ylab = colnames(x)[2])
  if (method %in% c("LDA", "QDA")) {
    Z <- predict(object, grid, ...)
    ZP <- Z$post
  } else if (method == "multinom") {
    ZP <- predict(object, newdata = grid, type = "probs")
  } else { ## method  "dummy" :
    ## object is list with 3 elements [[1]] .. [[3]
    ## take these fits as *columns* of matrix ZP[ , ] :
    ZP <- cbind(
        fit.1 = predict(object[[1]], newdata = grid, type = "response"),
        fit.2 = predict(object[[2]], newdata = grid, type = "response"),
        fit.3 = predict(object[[3]], newdata = grid, type = "response"))
  }
  ## for all methods:
  zp <- ZP[, 3] - pmax(ZP[, 2], ZP[, 1])
  contour(xp, yp, matrix(zp, len),
          add = TRUE, levels = 0, drawlabels = FALSE, col = colcont)
  zp <- ZP[, 1] - pmax(ZP[, 2], ZP[, 3])
  contour(xp, yp, matrix(zp, len),
          add = TRUE, levels = 0, drawlabels = FALSE, col = colcont)
}

par(mfrow = c(2, 2), ## the following are not needed, but "nice":
    mar = .1 + c(3,3,2,1), mgp = c(1.5, 0.6, 0))

## Use function lda() to fit data
class_lda <- lda(x = Iris[, c("Petal.Length", "Petal.Width")], grouping = Iris[, "Species"])

## Use function predplot() to plot the boundaries
predplot(class_lda, Iris, main = "Classification with LDA", method ="LDA")

## Use function qda() to fit data
class_qda <- qda(x = Iris[ ,c("Petal.Length", "Petal.Width")], grouping = Iris[, "Species"])

## Use function predplot() to plot the boundaries
predplot(class_qda, Iris, main = "Classification with QDA", method = "QDA")

## task a)

## Use function multinom to fit data => multionmial logistic regression
class_multinom <- multinom(formula = Species ~ Petal.Length + Petal.Width, data = Iris)

## Use function predplot to plot the boundaries
predplot(class_multinom, x = Iris, main = "Classification with multinomial regression", method = "multinom")

## task b)

## define first dummy encoding
Iris1 <- Iris
levels(Iris1$Species) <- c("setosa", "not", "not")
Iris1$Species <- relevel(Iris1$Species, ref = "not")
## fit model
fit.1 <- glm(formula = Species ~ ., data = Iris1, family = "binomial")

## define second dummy encoding
Iris2 <- Iris
levels(Iris2$Species) <- c("not", "versicolor", "not")
Iris2$Species <- relevel(Iris2$Species, ref = "not")
## fit model
fit.2 <- glm(formula = Species ~ ., data = Iris2, family = "binomial")

## define third dummy encoding
Iris3 <- Iris
levels(Iris3$Species) <- c("not", "not", "virginica")
Iris3$Species <- relevel(Iris3$Species, ref = "not")
## fit model
fit.3 <- glm(formula = Species ~ ., data = Iris3, family = "binomial")

## Use function predplot() to plot the boundaries
## check the function definition to find the right arguments
predplot(list(fit.1, fit.2, fit.3), x = Iris,
         main = "Classification with dummy encoding", method = "dummy")
