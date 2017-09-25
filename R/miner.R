# load dependencies
require("carenR")
source("LRAR/caren.R")
source("LRAR/predictLRAR.R")
source("edira/edira.R")

mineRules<-function(X, Y, is2Years = 0, folds = NULL, Kfolds = 1, xs = NULL, ys = NULL){
  for(i in 1:Kfolds){
    
    if(is2Years){
      x <- X
      y <- Y
    } else {
      # Segement your data by fold
      testIndexes <- which(folds==i)
      train_ind <- -testIndexes
      
      x <- X[train_ind, ]
      xs <- X[-train_ind, ]
      y <- Y[train_ind, ]
      ys <- Y[-train_ind, ]
    }
    
    # discretize TRAIN
    DISC <- mdlp.rank(x, y, method = "kendall")
    xd = DISC$Disc.data
    
    #prune columns with no partitions
    idx <- apply(DISC$Disc.data, 2, function(dc) any(dc==2, na.rm=TRUE))
    DISC$Disc.data <- DISC$Disc.data[,idx, drop = FALSE]
    
    if (ncol(DISC$Disc.data) == 0)
    {
      print("Not discretized")
      xs <- NULL
    } else {
      xs <- sapply(which(idx), function(j){
        findInterval(xs[,j], c(-Inf, DISC$cutp[[j]], Inf) )
      })
    }
    
    # transform in unique attributes
    unique.attributes <- function(train.matrix, class, test.matrix, npart)
    {
      mx <- c(0, cumsum(apply(train.matrix, 2, max))[-ncol(train.matrix)])
      
      list( cbind( t(t(train.matrix) + as.vector(mx)), class ), t(t(test.matrix) + mx) , npart = npart)
    }
    
    if(!is.null(xs)){
      npart <- mean(apply(DISC$Disc.data, 2, function(c) length(unique(c))))
      res <- unique.attributes(DISC$Disc.data, class, xs, npart)
      
    } else {
      res <- NULL
    }
    xs <-res[[2]] 
    xd <-res[[1]]
    
    # mine LR rules
    if(isPairwise) {
      rulz <- aflrC7Pairwise(xd, y, msup = minSupport, mconf = minConfidence, mlift = minLift, mimp = minImprovment, theta = 0, Xmx = "2000M")
    } else {
      rulz <- aflrC7(xd, y, msup = minSupport, mconf = minConfidence, mlift = minLift, mimp = minImprovment, theta = 0, Xmx = "2000M")
    }
    
    # predict LRARs
    if(isPairwise){
      yp <- crankPairwise(rulz, xs, ys, std, m2, 1)  
    } else {
      yp <- crank(rulz, xs, ys, std, m2, 1)
    }
    
    if(!isPairwise){
      # evaluate predictions
      tau <- mean(sapply(1:nrow(ys), function(j) cor(ys[j,], yp[,j], method="kendall")))
      print(paste("fold", i, ": tau=", tau)) 
    } else {
      print(paste("fold", i, ": success")) 
    }
  }
}