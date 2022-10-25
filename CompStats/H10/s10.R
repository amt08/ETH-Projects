### Code skeleton for Series 10

## b)

## load the data:
vehicle <- read.table("http://stat.ethz.ch/Teaching/Datasets/NDK/vehicle.dat",
                   header = TRUE)

## train a full grown tree:
require(rpart)
set.seed(10)
tree <- rpart(Class ~ ., data = vehicle,
              control = rpart.control(cp = 0.0, minsplit = 30))

tree <- rpart(Class ~ ., data = vehicle, control = rpart.control(minsplit = 30, cp = 0))
min_cv <- which.min(tree$cptable[,4])
thresh <- tree$cptable[min_cv, "xstd"] + tree$cptable[min_cv, "xerror"]
tree$cptable[tree$cptable[, "xerror"] < thresh, "CP"][1]

## package for plotting trees:
require(rpart.plot)
prp(tree, extra=1, type=1, 
    box.col=c('pink', 'palegreen3', 
              'lightsteelblue 2','lightgoldenrod 1')[tree$frame$yval])


## d)
plotcp(tree)
printcp(tree)

cp_choose <- printcp(tree)

tree <- prune.rpart(tree, cp = 0.008757962)

## f)

# misclassification error
misclass.sample <- function(data, ind.training, ind.test)
{
  tree <- rpart(Class ~ ., data = data[ind.training, ],
                # initially we have the tree pruned with these parameters
                control = rpart.control(cp = 0.0, minsplit = 30))
  
  ## choose optimal cp according to 1-std-error rule:
  min.ind <- which.min(tree$cptable[,"xerror"]) # find the min cross validation error
  # add the std deviation of the min value to the min value CV
  min.lim <- tree$cptable[min.ind, "xerror"] + tree$cptable[min.ind, "xstd"]
  # get the cp of the first smallest CV value than the limit we accept
  cp.opt <- tree$cptable[(tree$cptable[,"xerror"] < min.lim),"CP"][1] # select first cp value
  
  ## prune the tree:
  tree.sample <- prune.rpart(tree, cp=cp.opt)
  
  ## return missclassifcation error:
  mean(data$Class[ind.test] != predict(tree.sample, newdata = data[ind.test, ], type = "class"))
}

# Misclassification error
mean(residuals(tree))

## CV-error:
cv.err <- function(data, ind){
  misclass.sample(data, ind.training = -ind, ind)
}

data <- vehicle
n <- nrow(data)
cv.samples <- sapply(1:n, cv.err, data = data)
errcv <- mean(cv.samples)

## Bootstrap error:
B <- 1000
n <- nrow(data)
boot.err <- function(data, ind) {
  misclass.sample(data, ind, ind)
}

boot.samples <- replicate(B, boot.err(data, sample(1:n, B, replace = TRUE)))
errboot <- mean(boot.samples)

## Out-of-bootstrap-sample generalization error
oobs.sample <- function(data, ind){
  misclass.sample(data, ind, -ind)
}

obs.samples <- replicate(B, oobs.sample(data, sample(1:n, B, replace=TRUE)))
erroobs <- mean(obs.samples)



