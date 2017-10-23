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
minSupport <<- 51
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

datasets <- list("housing", "wisconsin", "iris")

for(dataset in datasets) {
  load(paste("label ranking data/",dataset,".RDATA", sep = ""))
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
    #mineRules(X, Y, folds = folds, Kfolds = Kfolds)
    repeat{
      exception <- FALSE
      result = tryCatch({
        mineRules(X, Y, folds = folds, Kfolds = Kfolds)
      }, error = function(e) {
        #if(e$message == "subscript out of bounds" &&  e$call == "FUN(X[[i]], ...)"){
        if(e$message == "no rules"){
          if(minSupport == 1) {
            print("#no rules found!")
            exception <- TRUE
          } else {
            print(paste("#no rules found - decreasing support from ",minSupport," to ",minSupport - 5))
            minSupport <<- minSupport - 5
            e
          }
        } else if(e$message == "few rules"){
          print(paste("#less than 5000 rules - def.rank can be optimized - decreasing support from ",minSupport," to ",minSupport - 5))
          minSupport <<- minSupport - 5
          e
        } else {
          print("#error")
          print(e)  
          exception <- TRUE
        }
      }, finally = {})
      
      if( !inherits(result, "error") || (inherits(result, "error") && exception) ){
        break()
      }
    }
    
  } 
}






