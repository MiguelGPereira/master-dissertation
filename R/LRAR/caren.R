require("carenR")
#source("caren_integrated.R")

#Apriori for Label Ranking - C (CAREN incorporated)
aflrC7 <- function(disc.x, y, msup = 1, mconf = 70, mlift = 0, mimp = 0, theta = 0, Xmx = "2000M")
{
	colnames(y) <- paste("L", 1:ncol(y), sep = "")
	
	if (mlift == 0 ) mlift = NULL  else mlift = mlift
	
	#
	#Insert here the options for the java exec
	#
	opt = paste(c("-RANK", theta, ",",1, " ", "-nodef -RS4"), collapse="")
	
  
	if (file.exists("RulesTemp.csv")) file.remove("RulesTemp.csv")
	if (file.exists("caren_temp.bas")) file.remove("caren_temp.bas")
	# Calls caren 
	r <- caren( cbind(disc.x, y), min.sup=msup/100, min.conf=mconf/100, imp = mimp,lift=mlift, class="L1", Xmx = Xmx, Options = opt)
	
	if(nrow(r)>0){
		#set new order
		r <- r[order(sapply(r[,"Ant"], length)),]
		
		res <-  list()
		
		apply(r, 1, function(rr)
				{
					if (length(rr$Ant)>0 && !is.na(rr$Ant)) {
						a <- as.integer( unlist( strsplit( rr$Ant , "=") )[c(F,T)] )
						names(a) <- unlist( strsplit( rr$Ant , "=") )[c(T,F)]
						
						if (length(grep("null", as.character(rr$Cons)))==0){
							c <- as.integer( unlist(strsplit( unlist( strsplit( as.character(rr$Cons) , ">") ), "L"))[c(F,T)] )
							c <- order(c)
							
							sup <- round(as.numeric(rr$Sup)*100, 2)
							conf <- round(as.numeric(rr$Conf)*100, 2)
							lift <- round(as.numeric(rr$Lift)*100, 2)
							
							res[[length(res)+1]] <<- list(
									a=a,
									c=c,
									sup= sup,
									conf= conf,
									lift= lift 
							)
						}
					}
				})
		
		if (length(res)>0){
			res
		} else {
			NA
		}
	} else {
		NA
	}
} #End function

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

#Pairwise 
aflrC7Pairwise <- function(disc.x, y, msup = 1, mconf = 70, mlift = 0, mimp = 0, theta = 0, 
                           maxpairs = maxPairs, Xmx = "2000M", confThreshold = 0.05)
{
  colnames(y) <- paste("L", 1:ncol(y), sep = "")
  
  if (mlift == 0 ) mlift = NULL  else mlift = mlift
  
  #
  #Insert here the options for the java exec
  #
  opt = paste(c("-RANK", theta, ",",1, " ","-fisher -Pair", maxpairs, " ", "-nodefault -nodef -RS4"), collapse="")
  #opt = paste(c("-Pair", maxpairs, " ", "-nodef -RS4"), collapse="")
  
  if (file.exists("RulesTemp.csv")) file.remove("RulesTemp.csv")
  if (file.exists("caren_temp.bas")) file.remove("caren_temp.bas")
  # Calls caren 
  r <- caren( cbind(disc.x, y), min.sup=msup/100, min.conf=mconf/100, imp = mimp,lift=mlift, class="L1", Xmx = Xmx, Options = opt)

  if(nrow(r)>0){
    #set new order
    r <- r[order(sapply(r[,"Ant"], length)),]
    
    # clean lines with more than 1 pair
    # ind <- sapply(r$Cons, function(pairs){ !grepl(",", pairs) })
    # r <- r[ind,] 
    
    # r <- r[order(r[,"Cons"]),]
    # idx <- c(1:nrow(r))
    # idx <- sapply(idx, function(t) TRUE)
    # # browse each rule
    # for(i in 1:nrow(r)){
    #   print(paste("%",formatC((i/nrow(r))*100,digits=2, format="f"), "(",i,"/",nrow(r),")"))
    #   antecedent <- r[i,]$Ant
    #   consequent <- unlist( strsplit(as.character(r[i,]$Cons), ","))
    #   confidence <- r[i,]$Conf
    #   # browse each previous rule we are trying to dismiss (previous pruning)
    #   # check if conficence is lower on inside the threshold
    #   # check if the antecedent is the same
    #   # check if the consequent has the length to be a possible specialization of the test
    #   # check if it actually is specialization
    #   prev <- which(idx[1:i])
    #   id <- which( (as.character(r[prev,]$Ant) == as.character(antecedent)) & (r[prev,]$Conf<confidence | (r$Conf-confidence<confThreshold)))
    #   filter <- sapply(r[prev,][id,]$Cons, function(testConsequent){
    #     testConsequent <- unlist(strsplit(as.character(testConsequent), ","))
    #     (length(testConsequent)<length(consequent)) & (length(which(consequent %in% testConsequent)) == length(testConsequent))
    #   })
    #   idx[prev][id] <- !filter
    # }
    # r <- r[idx,]
    
    res <-  list()
    
    n <- ncol(y)
    k <- 2
    maxpairs <- factorial(n)/(factorial(k)*factorial(n-k))
    
    apply(r, 1, function(rr)
    {
      if (length(rr$Ant)>0 && !is.na(rr$Ant)) {
        a <- as.integer( unlist( strsplit( rr$Ant , "=") )[c(F,T)] )
        names(a) <- unlist( strsplit( rr$Ant , "=") )[c(T,F)]
        
        if (length(grep("null", as.character(rr$Cons)))==0){
          
          pairsMatrix <- matrix(data=NA,nrow=ncol(y),ncol=ncol(y))
          pairs <- rr$Cons
          pairs <- as.character(pairs)
          pairs <- unlist( strsplit(pairs, ","))
          sapply(pairs, function(pair){
            pair <- unlist( strsplit(pair, ">"))
            pair <- unlist( strsplit(pair, "L"))
            pair <- pair[c(F,T)]
            pair <- as.integer(pair)
            
            first <- pair[1]
            second <- pair[2]
            # following 1 & -1 inverted because of caren
            pairsMatrix[first, second] <- -1
            pairsMatrix[second, first] <- 1
            pairsMatrix <<- pairsMatrix
          })
          pairsRanking <- apply(pairsMatrix, 1, function(line){ sum(line, na.rm = TRUE) })
          c <- pairsMatrix
          #browser()
          sup <- round(as.numeric(rr$Sup)*100, 2)
          conf <- round(as.numeric(rr$Conf)*100, 2)
          lift <- round(as.numeric(rr$Lift)*100, 2)

          completeness <- length(pairs)/maxpairs
            
          res[[length(res)+1]] <<- list(
            a=a,
            c=c,
            sup= sup,
            conf= conf,
            lift= lift,
            completeness= completeness
          )
        }
      }
    })
    
    if (length(res)>0){
      res
    } else {
      NA
    }
  } else {
    NA
  }
} #End function
