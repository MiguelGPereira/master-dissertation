# CORE SCRIPT

# dependencies
require("carenR")
source("LRAR/caren.R")
source("LRAR/predictLRAR.R")
source("edira/edira.R")
source("miner.R")

# configurations
isPairwise <- 1

Kfolds <- 10
randomSeed <- 1234
maxPairs <- 2
minSupport <- 1
minConfidence <- 90
minLift <- 0
minImprovment <- 0.01

#load("german2005.RDATA")
#load("german2009.RDATA")
load("portugal2013.RDATA")
#load("german2005_2009.RDATA")

# invoke miner
if(is2Years){
  mineRules(X, Y, is2Years, xs = xs, ys = ys)
} else {
  # KFold Cross Validation
  # Randomly shuffle the data
  set.seed(randomSeed)
  samplerows <- sample(nrow(X))
  X <- X[samplerows, ]
  Y <- Y[samplerows, ]
  
  # Create K equally size folds
  folds <- cut(seq(1, nrow(X)), breaks = Kfolds, labels = FALSE)
  
  # Perform K fold cross validation
  mineRules(X, Y, folds = folds, Kfolds = Kfolds)
}







