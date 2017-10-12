# load dependencies
require("carenR")
source("LRAR/caren.R")
source("LRAR/predictLRAR.R")
source("edira/edira.R")

#default rank
def.rank <- function(ys)
{
  ready <- FALSE
  while (ready == FALSE)
  {
    r <- rank(colMeans(ys))
    if (length(unique(r))==1 )
    {
      ys <- ys[-1,]
    } else {
      ready <- TRUE
    }
  }
  r
}

goodmank <- function(x,y)
{
  a <- outer(x,x,function(u,v) sign(v-u))
  b <- outer(y,y,function(u,v) sign(v-u))
  #a[a==0] <- NA
  #b[b==0] <- NA
  comp <- a==b
  diag(comp) <- NA
  return((sum(comp, na.rm = TRUE)-sum(!comp, na.rm = TRUE))/sum((comp)>=0, na.rm = T))
}

mineRules<-function(X, Y, is2Years = 0, folds = NULL, Kfolds = 1, xs = NULL, ys = NULL){
  tauList = list()
  baselineList = list()
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
    indexesDiscretized <- apply(DISC$Disc.data, 2, function(dc) any(dc==2, na.rm=TRUE))
    
    # equal frequency
    # indexesDiscretized <- apply(DISC$Disc.data, 2, function(dc) any(dc==2, na.rm=TRUE))
    # discretizedList <- 1:nrow(DISC$Disc.data)
    # chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
    # splits <- chunk2(discretizedList, 4)
    # for(t in 1:4){
    #   discretizedList <<- discretizedList
    #   discretizedList[splits[[t]] ] <- t
    # }
    # 
    # noDisc <- x[,!indexesDiscretized]
    # noDisc <- apply(noDisc, 2, function(col){
    #   orderedInd <- order(col)
    #   for(o in 1:length(orderedInd)){
    #     col[orderedInd[o]] <- discretizedList[o]
    #   }
    #   col
    # })
    # 
    # DISC$Disc.data[,!indexesDiscretized] <- noDisc

    # equal width
    if(sum(!indexesDiscretized) > 0){
      D <- list()
      nbins <- 4
      xnd <- as.matrix(x[,!indexesDiscretized])
      #browser()
      D$Disc.data <- apply(xnd, 2, discretize, "interval", nbins, 1:nbins)
      #browser()
      D$Disc.data <- apply(D$Disc.data, 2, as.integer)
      D$cutp <- lapply(1:ncol(xnd), function(i){
        discretize(xnd[,i], "interval", nbins, 1:nbins, onlycuts=TRUE)[-c(1,(nbins+1))]
      })

      DISC$Disc.data[,!indexesDiscretized] <- D$Disc.data
      DISC$cutp[!indexesDiscretized] <- D$cutp
      indexesDiscretized <- apply(DISC$Disc.data, 2, function(dc) any(dc==2, na.rm=TRUE))
    }
    #browser()
    #prune columns with no partitions
    DISC$Disc.data <- DISC$Disc.data[,indexesDiscretized, drop = FALSE]
    #browser()
    if (ncol(DISC$Disc.data) == 0)
    {
      print("Not discretized")
      xs <- NULL
    } else {
      xs <- sapply(which(indexesDiscretized), function(j){
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
    
    #browser()
    
    # mine LR rules
    if(isPairwise) {
      rulz <- aflrC7Pairwise(xd, y, msup = minSupport, mconf = minConfidence, mlift = minLift, mimp = minImprovment, 
                             theta = 0, Xmx = "2000M", confThreshold = confThreshold)
    } else {
      rulz <- aflrC7(xd, y, msup = minSupport, mconf = minConfidence, mlift = minLift, mimp = minImprovment, theta = 0, Xmx = "2000M")
    }
    #browser()
    # predict LRARs
    std <- def.rank(y)
    mt <- 0
    if(isPairwise){
      yp <- crankPairwise(rulz, xs, ys, std, m2, mt)  
      # print(t(yp))
      # print(unique(t(yp)))
      # browser()
    } else {
      #browser()
      yp <- crank(rulz, xs, ys, std, m2, mt)
      # print(unique(t(yp)))
    }
    
    # evaluate rules
    if(ys == 0){
      yp <- t(yp)
      colnames(yp) <- colnames(y)
      saveRDS(yp, "predictions.rds")
      saveRDS(y, "results.rds")
      print("2017 prediction saved")
      #browser()
      print("baseline w/ last year's results")
      #print(mean(sapply(1:nrow(ys), function(j) cor(ys[j,], baseline, method="kendall"))))
    } else if(isPairwise){
      #..
      #browser()
      yp <- t(yp)
      colnames(yp) <- colnames(y)
      saveRDS(yp, "predictions2013.rds")
      saveRDS(ys, "results2013.rds")
      
      gamma <- mean(sapply(1:nrow(ys), function(j) {
        # print("ys[j,]")
        # print(ys[j,])
        # print("yp[j,]")
        # print(yp[j,])
        gm <- goodmank(ys[j,], yp[j,])
        # print("gm")
        # print(gm)
        gm
      }))
      print(table(sapply(1:nrow(ys), function(j) goodmank(ys[j,], yp[j,]))))
      print("unique(yp)")
      print(unique(yp))
      #browser()
      tauList[[i]] <- gamma
      print(paste("fold", i, ": gamma=", gamma))
      baseline <- rank(colMeans(y))
      baselineTau <- mean(sapply(1:nrow(ys), function(j) cor(ys[j,], baseline, method="kendall")))
      baselineList[[i]] <- baselineTau
      print(paste("baseline tau=", baselineTau))
      print(paste("smart baseline",mean(sapply(1:nrow(ys), function(j) cor(y[j,], ys[j,], method="kendall")))))
      if(i == Kfolds){
        print(paste("final gamma=", sum(as.numeric(tauList))/Kfolds))
        print(paste("final baseline gamma=", sum(as.numeric(baselineList))/Kfolds))
      }
    } else {
      tau <- mean(sapply(1:nrow(ys), function(j) cor(ys[j,], yp[,j], method="kendall")))
      tauList[[i]] <- tau
      print(paste("fold", i, ": tau=", tau))
      baseline <- rank(colMeans(y))
      baselineTau <- mean(sapply(1:nrow(ys), function(j) cor(ys[j,], baseline, method="kendall")))
      baselineList[[i]] <- baselineTau
      print(paste("baseline tau=", baselineTau))
      print(paste("smart baseline",mean(sapply(1:nrow(ys), function(j) cor(y[j,], ys[j,], method="kendall")))))
      
      if(i == Kfolds){
        print(paste("final tau=", sum(as.numeric(tauList))/Kfolds))
        print(paste("final baseline tau=", sum(as.numeric(baselineList))/Kfolds))
      }
    }
    
  }
}