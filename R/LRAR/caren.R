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
