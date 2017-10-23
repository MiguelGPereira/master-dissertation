crank <- function(rulz, xs, ys, std, m2, mt)
{
  #Order the rules by "confidence" and "support"
  rulz <- rulz[order(-sapply(rulz, "[[" ,"sup" ))]
  rulz <- rulz[order(-sapply(rulz, "[[" ,"conf" ))]
  
  if (mt==0)
  { 
    i <- TRUE
  } else
  {
    i <- 1:mt
  }
  
  ##Match rules whith examples
  rs <- apply(xs, 1, function(row)
  {
    a <- sapply(rulz, function(l) { all(l$a %in% row) })
    
    if (any(a))
    {
      if (sum(a)<mt) {
        ii <- 1:sum(a)
      } else {
        ii <- i
      }
      #browser()
      #if is pt
      if(colnames(xs)[1] == "Residents.Total"){
        fRul <- sapply(rulz[a][ii], "[[", "c")
        if(class(fRul)!="list"){
          rt <- rank( rowMeans( fRul ) )  
        } else {
          rt <- rank( rowMeans( t(do.call(rbind,fRul)) ) ) 
        }
      } else {
        rt <- rank( rowMeans( sapply(rulz[a][ii], "[[", "c") ) )  
      }
      
      if (length(unique(rt))==1 )
      {
        #The method uses the baseline ranking the first rule if the avg ranking is constant
        rt <- -rank(-std)
      }
    } else
    {
      #NOTE: -rank(-c(1,2,3)) is equivalent to c(1,2,3) 
      rt <- -rank(-std)
    }
    rt
  })
  
  rs
}

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

goodmank <- function(x,y)
{
  a <- outer(x,x,function(u,v) sign(v-u))
  b <- outer(y,y,function(u,v) sign(v-u))
  a[a==0] <- NA
  b[b==0] <- NA
  comp <- a==b
  diag(comp) <- NA
  return((sum(comp, na.rm = TRUE)-sum(!comp, na.rm = TRUE))/sum((comp)>=0, na.rm = T))
}

crankPairwise <- function(rulz, xs, ys, std, m2, mt, kfold = 0)
{
  #Order the rules by "confidence" and "support"
  rulz <- rulz[order(-sapply(rulz, "[[" ,"sup" ))]
  rulz <- rulz[order(-sapply(rulz, "[[" ,"conf" ))]
  browser()
  if (mt==0)
  { 
    i <- TRUE
  } else
  {
    i <- 1:mt
  }
  
  # TO DELETE
  # xs <- xs[1:25,]
  # ys <- ys[1:25,]
  # print(xs)
  # print(ys)
  ##Match rules whith examples
  compList <<- list()
  compIndex <<- 1
  defRankCount <<- 0
  rs <- apply(xs, 1, function(row)
  {
    a <- sapply(rulz, function(l) { all(l$a %in% row) })
    if (any(a))
    {
      # fetch the ii rows of the the mt best rules
      if (sum(a)<mt) {
        ii <- 1:sum(a)
      } else {
        ii <- i
      }
      #browser()
      appliedRules <- sapply(rulz[a][1], function(line){
        pairsMatrix <- line$c
        pairsRanking <- apply(pairsMatrix, 1, function(line){ sum(line, na.rm = TRUE) })
      })
      appliedRules <- t(appliedRules)
      
      compList[[compIndex]] <<- rulz[a][1][[1]]$completeness
      compIndex <<- compIndex + 1
      #browser()
      
      normalizer <- colSums(abs(appliedRules))
      aggregatedRules <- apply(appliedRules, 2, function(column){ sum(column, na.rm = TRUE) })
      normalizedRules <- aggregatedRules/normalizer
      aggregatedRules <- normalizedRules
      
      aggregatedRules[is.nan(aggregatedRules)] <- NA
      aggregatedRules <- -aggregatedRules
      rt <- aggregatedRules
      
      # ranking <- rank(-aggregatedRules, na.last = TRUE)
      # rt <- ranking
      # print(rulz[a][1])
      # print(rt)
      # 
      # browser()
      
      if (length(unique(rt))==1 )
      {
        #The method uses the baseline ranking the first rule if the avg ranking is constant
        rt <- -rank(-std)
        defRankCount <<- defRankCount + 1
      }
    } else
    {
      #NOTE: -rank(-c(1,2,3)) is equivalent to c(1,2,3) 
      rt <- -rank(-std)
      defRankCount <<- defRankCount + 1
    }
    rt
  })
  meanCompleteness <- mean(as.numeric(compList))
  print(paste("completeness",meanCompleteness))
  completenessList[[kfold]] <<- meanCompleteness
  
  percentDefRank <- defRankCount * 1 / nrow(xs)
  print(paste("def rank % in fold ",percentDefRank))
  defRankUsageList[[kfold]] <<- percentDefRank
  
  rs
}
