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

# _goodmank <- function(x,y)
# {
#   a <- outer(x,x,function(u,v) sign(v-u))
#   b <- outer(y,y,function(u,v) sign(v-u))
#   b[b==0] <- NA
#   comp <- a==b
#   diag(comp) <- NA
#   return((sum(comp, na.rm = TRUE)-sum(!comp, na.rm = TRUE))/sum((comp)>=0, na.rm = T))
# }

goodmank <- function(x,b)
{
  a <- outer(x,x,function(u,v) sign(v-u))
  comp <- a==b
  diag(comp) <- NA
  return((sum(comp, na.rm = TRUE)-sum(!comp, na.rm = TRUE))/sum((comp)>=0, na.rm = T))
}

# _accuracy <- function(x,y)
# {
#   a <- outer(x,x,function(u,v) sign(v-u))
#   b <- outer(y,y,function(u,v) sign(v-u))
#   b[b==0] <- NA
#   comp <- a==b
#   diag(comp) <- NA
#   return(sum(!comp, na.rm = TRUE) == 0)
# }

accuracy <- function(x,b)
{
  a <- outer(x,x,function(u,v) sign(v-u))
  comp <- a==b
  diag(comp) <- NA
  return(sum(!comp, na.rm = TRUE) == 0)
}

equalFrequencyDisc <- function(x, indexesDiscretized, DISC)
{
    discretizedList <- 1:nrow(DISC$Disc.data)
    chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
    splits <- chunk2(discretizedList, 4)
    for(t in 1:4){
      discretizedList <<- discretizedList
      discretizedList[splits[[t]] ] <- t
    }

    noDisc <- x[,!indexesDiscretized]
    noDisc <- apply(noDisc, 2, function(col){
      orderedInd <- order(col)
      for(o in 1:length(orderedInd)){
        col[orderedInd[o]] <- discretizedList[o]
      }
      col
    })

    DISC$Disc.data[,!indexesDiscretized] <- noDisc
    DISC
}

equalWidthDisc <- function(x, indexesDiscretized, DISC)
{
    D <- list()
    nbins <- 4
    xnd <- as.matrix(x[,!indexesDiscretized])
    D$Disc.data <- apply(xnd, 2, discretize, "interval", nbins, 1:nbins)
    D$Disc.data <- apply(D$Disc.data, 2, as.integer)
    D$cutp <- lapply(1:ncol(xnd), function(i){
      discretize(xnd[,i], "interval", nbins, 1:nbins, onlycuts=TRUE)[-c(1,(nbins+1))]
    })

    DISC$Disc.data[,!indexesDiscretized] <- D$Disc.data
    DISC$cutp[!indexesDiscretized] <- D$cutp
    DISC
}

mineRules<-function(X, Y, is2Years = 0, folds = NULL, Kfolds = 1, xs = NULL, ys = NULL){
  print("minerules enter")
  tauList <<- list()
  accuracyList <<- list()
  baselineList <<- list()
  completenessList <<- list()
  defRankUsageList <<- list()
  rulzList <<- list()
  for(i in 1:Kfolds){
    print("minerules fold")
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
    
    if(sum(!indexesDiscretized) > 0){
      # equal frequency
      # indexesDiscretized <- apply(DISC$Disc.data, 2, function(dc) any(dc==2, na.rm=TRUE))
      # DISC <- equalFrequencyDisc(x, indexesDiscretized, DISC)

      # equal width
      DISC <- equalWidthDisc(x, indexesDiscretized, DISC)
      indexesDiscretized <- apply(DISC$Disc.data, 2, function(dc) any(dc==2, na.rm=TRUE))
    }
    
    
    #prune columns with no partitions
    DISC$Disc.data <- DISC$Disc.data[,indexesDiscretized, drop = FALSE]
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

    # mine LR rules
    if(isPairwise) {
      rulz <- aflrC7Pairwise(xd, y, msup = minSupport, mconf = minConfidence, mlift = minLift, mimp = minImprovment, 
                             theta = theta, Xmx = "2000M", confThreshold = confThreshold)
      if(is.na(rulz)) {
        stop("no rules")
      } 
      # else if (minSupport != 1 && length(rulz) < 5000) {
      #   stop("few rules")
      # }
    } else {
      rulz <- aflrC7(xd, y, msup = minSupport, mconf = minConfidence, mlift = minLift, mimp = minImprovment, theta = 0, Xmx = "2000M")
    }

    # predict LRARs
    std <- def.rank(y)
    mt <- 0
    if(isPairwise){
      yp <- crankPairwise(rulz, xs, ys, std, m2, mt, kfold = i)  
    } else {
      yp <- crank(rulz, xs, ys, std, m2, mt)
    }
    
    # evaluate rules
    if(is.null(ys)){
      # 2017 predictions
      yp <- t(yp)
      colnames(yp) <- colnames(y)
      saveRDS(yp, "predictions.rds")
      saveRDS(y, "results.rds")
      print("2017 prediction saved")
      print("baseline w/ last year's results")
      print(mean(sapply(1:nrow(ys), function(j) cor(ys[j,], baseline, method="kendall"))))
    } else if(isPairwise){
      #browser()
      # yp <- t(yp)
      # colnames(yp) <- colnames(y)
      # saveRDS(yp, "predictions2013_17.rds")
      # saveRDS(y, "results2013_17.rds")
      # print("2013_17 prediction saved")
      # browser()
      gamma <- mean(sapply(1:nrow(ys), function(j) {
        g <- goodmank(ys[j,], yp[[j]])
        if (is.nan(g)){
          g <- 0
        }
        g
      } ))
      acc <- mean(sapply(1:nrow(ys), function(j) accuracy(ys[j,], yp[[j]]) ))
      
      tauList[[i]] <- gamma
      accuracyList[[i]] <- acc
      #rulzList[[i]] <- length(rulz)
      if (defRankUsageList[[i]] != 0 && minSupport > 1) {
        stop("non zero def.rank")
      }
      print(paste("fold", i, ": gamma=", gamma," | completeness=", completenessList[[i]]," | accuracy=", acc," | %defrank=", defRankUsageList[[i]]))
      baseline <- rank(colMeans(y))
      baselineTau <- mean(sapply(1:nrow(ys), function(j) cor(ys[j,], baseline, method="kendall")))
      baselineList[[i]] <- baselineTau
      print(paste("baseline tau=", baselineTau))
      print(paste("smart baseline",mean(sapply(1:nrow(ys), function(j) cor(y[j,], ys[j,], method="kendall")))))
      if(i == Kfolds){
        if(!is.null(dataset)){
          title <- paste("############### ",dataset," ###############")  
        } else {
          title <- paste("############### output ###############")  
        }
        cat("\n")
        cat(title,"\n")
        cat("# confidence:",minConfidence/100,"\n")
        cat("# max pairs:",maxPairs,"\n")
        cat("# calculated support:",minSupport/100,"\n")
        cat("#\n")
        gamma <- round(sum(as.numeric(tauList))/Kfolds, 2)
        cat("# gamma:", gamma,"\n")
        completeness <- round(sum(as.numeric(completenessList))/Kfolds, 2)
        cat("# completeness:", completeness,"\n")
        accuracy <- round(sum(as.numeric(accuracyList))/Kfolds, 2)
        cat("# accuracy:", accuracy,"\n")
        defrank <- round(sum(as.numeric(defRankUsageList))/Kfolds, 2)
        cat("# def.rank:", defrank,"\n")
        rules <- round(sum(as.numeric(rulzList))/Kfolds)
        cat("# rules:", rules,"\n")
        cat("#\n")
        baseline <- round(sum(as.numeric(baselineList))/Kfolds, 2)
        cat("# baseline (gamma):", baseline,"\n")
        cat("%RFORMAT%",dataset,"%",minConfidence/100,"%",maxPairs,"%",minSupport/100,"%",gamma,"%",completeness,"%",accuracy,"%",defrank,"%",rules,"%",baseline,"%",minImprovment,"\n", sep = "")
        cat(paste(replicate(nchar(title), "#"), collapse = ""),"\n")
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
        tau <- round(sum(as.numeric(tauList))/Kfolds,2)
        baseline <- round(sum(as.numeric(baselineList))/Kfolds,2)
        print(paste("final tau=", tau))
        print(paste("final baseline tau=", baseline))
        
        cat("%RFORMAT%",dataset,"%",minConfidence/100,"%","NA","%",minSupport/100,"%",tau,"%","NA","%","NA","%","NA","%","NA","%",baseline,"%",minImprovment,"\n", sep = "")
      }
    }
    
  }
}

mineRulesTest<-function(X, Y, is2Years = 0, folds = NULL, Kfolds = 1, xs = NULL, ys = NULL){
  tauList <<- list()
  accuracyList <<- list()
  baselineList <<- list()
  completenessList <<- list()
  defRankUsageList <<- list()
  rulzList <<- list()
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
    if(sum(!indexesDiscretized) > 0){
      # equal frequency
      # indexesDiscretized <- apply(DISC$Disc.data, 2, function(dc) any(dc==2, na.rm=TRUE))
      # DISC <- equalFrequencyDisc(x, indexesDiscretized, DISC)
      
      # equal width
      DISC <- equalWidthDisc(x, indexesDiscretized, DISC)
      indexesDiscretized <- apply(DISC$Disc.data, 2, function(dc) any(dc==2, na.rm=TRUE))
    }
    
    #prune columns with no partitions
    DISC$Disc.data <- DISC$Disc.data[,indexesDiscretized, drop = FALSE]
    if (ncol(DISC$Disc.data) == 0)
    {
      print("Not discretized")
      xs <- NULL
    } else {
      xs <- sapply(which(indexesDiscretized), function(j){
        findInterval(xs[,j], c(-Inf, DISC$cutp[[j]], Inf) )
      })
    }
    
    cat("\n","pair",maxPairs,"fold",i,"pass","\n")
    
  }
}