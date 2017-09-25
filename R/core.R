# CORE SCRIPT
# configurations
german2005Data <- "../data/out_german_2005.csv"
german2009Data <- "../data/out_german.csv"
Kfolds <- 3
randomSeed <- 1234
is2Years <- 0
data <- german2009Data
dataSecondYear <- german2009Data
isPairwise <- 1
minSupport <- 1
minConfidence <- 90
minLift <- 0
minImprovment <- 0.01

# load dependencies
require("carenR")
source("LRAR/caren.R")
source("LRAR/predictLRAR.R")
source("edira/edira.R")
source("miner.R")

# read DATA
out <- read.csv(data, sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")
if(is2Years){
  out_2 <- read.csv("../data/out_german.csv", sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")
} 

# transform DATA
# select rows of relevant parties
rankings <- out[,c(33:37)]
if(is2Years){
  rankings_2 <- out_2[,c(33:37)]
}

# split in independent variables (x) and votes (y)
Y <- t(apply(rankings, 1, rank))
X <- out[,c(4:32)]
if(is2Years){
  ys <- t(apply(rankings_2, 1, rank))
  xs <- out_2[,c(4:32)]
}

# transform dataframes in matrixes
X <- as.matrix(X)
Y <- as.matrix(Y)
if(is2Years){
  xs <- as.matrix(xs)
  ys <- as.matrix(ys)
}

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







