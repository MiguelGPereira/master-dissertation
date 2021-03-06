  # CORE SCRIPT
  
  isGrid <- F
  args = commandArgs(trailingOnly=TRUE)
  if (length(args)==1) {
    if (args[1] == "grid"){
      isGrid <- T
    }
  }
  
  # dependencies
  if (isGrid) {
    library("carenR",lib.loc="/homes/up201402671/data/R/lib")
    library(arules, lib.loc="/homes/up201402671/data/R/lib")
    source("caren_integrated.R")
  } else {
    require("carenR")
    library(arules)
  }
  source("LRAR/caren.R")
  source("LRAR/predictLRAR.R")
  source("edira/edira.R")
  source("miner.R")
  
  # configurations
  isPairwise <- 1
  
  Kfolds <- 10
  randomSeed <- 1234
  minConfidence <- 70
  baseMinSupport <- 1
  confThreshold <- 0.05
  minLift <- 0
  minImprovment <- 0
  theta <- 1
  
  # dataset <- "german2005"
  # dataset <- "german2009"
  # dataset <- "german2005_2009"
  # dataset <- "portugal2009"
  # dataset <- "portugal2013"
  # dataset <- "portugal2009_2013"
  # dataset <- "portugal2013_2017_incomplete"
  # dataset <- "portugal2013_2017"
  
  load(paste0(dataset,".RDATA"))
  
  # datasets <- list("authorship","bodyfat","calhousing","cpu-small","elevators","fried","glass","housing","iris","pendigits","segment","stock","vehicle","vowel","wine","wisconsin")
  datasets <- list("iris")
  
  ## FOR DATASETS
  for(dataset in datasets) {
    if (isGrid) {
      load(paste("/homes/up201402671/data/master-dissertation/R/label_ranking_data/",dataset,".RData", sep = ""))
    } else {
      load(paste("label_ranking_data/",dataset,".RDATA", sep = ""))
    }

    is2Years <- 0
    X <- x
    Y <- y

    # Randomly shuffle the data
    set.seed(randomSeed)
    samplerows <- sample(nrow(X))
    X <- X[samplerows, ]
    Y <- Y[samplerows, ]

    pairsLimit <- if(ncol(Y) < 4) ncol(Y) else 4
    for(mpairs in 2:2){

      maxPairs <<- mpairs
      minSupport <<- baseMinSupport

      # invoke miner
      if(is2Years){
        mineRules(X, Y, is2Years, xs = xs, ys = ys)
      } else {
        # KFold Cross Validation

        # Create K equally size folds
        folds <- cut(seq(1, nrow(X)), breaks = Kfolds, labels = FALSE)

        # Perform K fold cross validation
        #mineRules(X, Y, folds = folds, Kfolds = Kfolds)
        mineRules(X, Y, folds = folds, Kfolds = Kfolds)
        repeat{
          exception <- FALSE
          result = tryCatch({
            #mineRulesTest(X, Y, folds = folds, Kfolds = Kfolds)
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
            } else if(e$message == "non zero def.rank"){
              print(paste("#def.rank can be optimized - decreasing support from ",minSupport," to ",minSupport - 5))
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
  }
  
  ### FOR ELECTIONS
  # for(mpairs in 5:5){
  #   is2Years <- 1
  #   maxPairs <<- mpairs
  #   minSupport <<- 1
  #   # invoke miner
  #   if(is2Years){
  #     mineRules(X, Y, is2Years, xs = xs, ys = ys)
  #   } else {
  #     # KFold Cross Validation
  #     # Randomly shuffle the data
  #     set.seed(randomSeed)
  #     samplerows <- sample(nrow(X))
  #     X <- X[samplerows, ]
  #     Y <- Y[samplerows, ]
  # 
  #     # Create K equally size folds
  #     folds <- cut(seq(1, nrow(X)), breaks = Kfolds, labels = FALSE)
  # 
  #     # Perform K fold cross validation
  #     mineRules(X, Y, folds = folds, Kfolds = Kfolds)
  # 
  #   }
  # }
  
  
  
  
  
  
