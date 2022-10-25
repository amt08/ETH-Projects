## Code Skeleton for Series 8, task 2

require(ROCR)

d.baby <- read.table("http://stat.ethz.ch/Teaching/Datasets/baby.dat", header=TRUE)

## task a)

fit <- glm(Survival ~ ., data = d.baby, family = "binomial")

pred <- prediction(predictions = fit$fitted.values, labels = d.baby$Survival)
perf <- performance(pred, "tpr", "fpr" )
title <- "logist.regr: glm(Survival ~ . , d.baby)  [in-sample]" # this is the in-sample prediction
plot(perf, main = paste("ROC: ", title))

# measurement = "cost"
perf.cost <- performance(pred, "cost")
plot(perf.cost, main = title)

## task b)

n <- nrow(d.baby)
K <- 10
all.y.true <- all.y.pred <- vector("list", length = K)
set.seed(101)
## define folds
folds <- sample(cut(seq(1, n), breaks = K, labels = FALSE), replace = FALSE) # replace = FALSE pt ca facem un permutation dupa ce am facut cut
for (i in 1:K){
  test.ind <- which(folds == i)
  df.train<- d.baby[-test.ind,]
  df.test <- d.baby[test.ind,]
  fit.glm <- glm(formula = Survival ~ ., data = df.train, family = "binomial")
  y.true <- df.test$Survival
  y.pred <- predict(fit.glm, newdata = df.test, type = "response")
  all.y.true[[i]] <- y.true
  all.y.pred[[i]] <- y.pred
}

pred.cv <- prediction(predictions = all.y.pred, labels = all.y.true)

perf.cv <- performance(pred.cv, "tpr", "fpr" )
title <- "logist.regr: glm(Survival ~ . , d.baby)  [cross-validated]"
plot(perf.cv, avg = "threshold", main = paste("ROC:  ", title))
## add the in-sample curve
plot(perf, col = 2, add = TRUE)

# here we plot the misclassification rate
perf.cv.cost <- performance(pred.cv, "cost")
plot(perf.cv.cost, avg = "vertical", main = title)
## add the in-sample curve
plot(perf.cost, col = 2, add = TRUE)

## task c)
c1 <- seq(0, 2, 0.2)
for (j in 1:length(c1)) {
  ## cost for given cost function
  perf.cv.cost.pen <- performance(pred.cv, "cost", cost.fp = c1[j], cost.fn = 2 - c1[j])
  plot(perf.cv.cost.pen, avg = "vertical", col = j - 6 * (j > 6), lty = 1 + 1 * (j > 6), add = (j > 1),
       # mean(d.baby$Survival == 1) =  fraction of true positives
       # 1 - mean(d.baby$Survival == 1) =  fraction of true negatives
       ylim = c(0, 2 * max(mean(d.baby$Survival == 1), 1 - mean(d.baby$Survival == 1))))
}
legend("topleft", legend = paste("c1 =", c1), col = c(rep(1:6, 2)),
       lty =  c(rep(1, 6), rep(2, 5)), ncol = 3)
