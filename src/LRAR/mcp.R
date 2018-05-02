# TODO: Add comment
# 
# Author: claudio
###############################################################################


#(WD <- getwd())
#if (!is.null(WD)) setwd("./apriori")

source("aflrC7.R")
source("pred.aux.R")

#n.data=name of the dataset
#part=Vector of partitions (if not a vector, all partitions will have the same size)
#disc=discretization method ("ef"-equal freq.; "ew"- equal width)
#m1=Coeficient (kendall,spearman, etc) used for learning the rankings
#m2=Coeficient (kendall,spearman, etc) used to evaluate the performance (default is 2 - Kendall tau)
#r=method to choose rules (0 - average of all rules, n - average of first n-th rules )
#wrt=write to file (TRUE or FALSE)
#k=Number of tests in the cross validation (0 = Leave-one-out)
#Yseed=randomize the target rankings (0=NULL)
#labelsRand - labels=1,2,... randomizes the relative positions of the labels in a ranking (0 = NULL)
#rankingsRand = TRUE - generates random rankings
#method=c("caren", "knn", "pluce", "nbayes", "lrt")
pred <- function(	n.data, disc = "me", msup = 1, mconf = 100, mlift = 0, 
					mimp = 0.001, mimp2 = -100, m1 = 2, m2 = 2, r = 0, wrt = FALSE,
					k = 1:10,	part = 3, seed = 10, theta = 0.5, dtheta = 0, Xmx = "6000M", disc.sim = "kendall",
					gama = 0.98, splitCrit = "CS", force.disc = FALSE, Yseed = 0, labelsRand = NULL, rankingsRand = FALSE,
					method=c("caren", "pluce"), kn = 5, exp_name = "random", opt.mconf = TRUE, print.rules = FALSE)
{
  
  if (is.list(n.data)){
    x <- n.data$x
    y <- n.data$y
    
  } else {
    load(file.path("..", "Data", "LR", paste(n.data,".RData", sep="")))
  }
  
  #Sets the seed for the generation of random numbers
  set.seed(seed)
  
	frst <- sapply(function(i) {
    MCONF <- mconf
	  rst <- NULL
	  frst <- NULL
	  flag <- TRUE
		mconf <- MCONF
		
		s <- S[i,]
		
		
		while (flag) {
				##Function (Apriori for Label Ranking C)
				rulz <- aflrC7(d, y[-s,], msup, mconf, mlift, mimp, mimp2, theta, Xmx, dirt)
				if (print.rules)
				{
					print(rulz)
				}
				
				if ( is.list(rulz) )
				{
					print("OK")
					
					#Standard Ranking
					std <- def.rank(y[-s,])
					
					#Consensus Ranking
					otp <- c(crank(rulz, xs, y[s,,drop=FALSE], std, m2,r), npart=npart)
					print(otp)
					if(otp["pred"] > 0.95)
					{
						rst <- c(otp, mconf=mconf, U=getU(y))
						flag <- FALSE
					} else if(mconf > 0 & opt.mconf)
					{
					  mconf <- mconf - 5
						if (otp["pred"]<0.1 & mconf > 10) mconf <- mconf - 5
						cat("min Confidence decreased to:", mconf, "\n")
					} else {
					  rst <- c(otp, mconf=mconf, U=getU(y))
						flag <- FALSE
					}
					
				} else
				{
					print("X")
					
					if(mconf > 0 & opt.mconf)
					{
						mconf <- mconf - 10
						cat("min Confidence decreased to:", mconf, "\n")
					} else {
						rst <- c(rep(NA,4),1, mconf=NA, U=getU(y))
						flag <- FALSE
					}
				}
			}
		
		
		rst
	})

  print(rowMeans(frst  , na.rm = T))
  mean.rst <- round(rowMeans(frst	, na.rm = T),3)
	sd.rst <- round(apply(frst,	1, sd, na.rm = T),3)
	
	cat(fill=TRUE)
	if (is.list(n.data)) { n.data <- "test" }
	cat(n.data, mean.rst, c(paste("disc=", disc, "sup=", msup), paste("conf=", mconf)),fill=TRUE)
	frst
}
#pred("iris",disc="m11",mconf=90, mimp=0.01, Xmx = "1000M")

