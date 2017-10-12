# CORE SCRIPT

# dependencies
require("carenR")
library(arules)
source("LRAR/caren.R")
source("LRAR/predictLRAR.R")
source("edira/edira.R")
source("miner.R")

# configurations
isPairwise <- 1

Kfolds <- 10
randomSeed <- 1234
maxPairs <- 2
minSupport <- 20
minConfidence <- 90
confThreshold <- 0.05
minLift <- 0
minImprovment <- 0.01

#load("german2005.RDATA")
#load("german2009.RDATA")
#load("german2005_2009.RDATA")
#load("portugal2009.RDATA")
#load("portugal2013.RDATA")
#load("portugal2009_2013.RDATA")
#load("portugal2013_2017.RDATA")
load("wisconsin.RDATA")
is2Years <- 0
X <- x
Y <- y

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







