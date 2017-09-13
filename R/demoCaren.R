# load dependencies
require("carenR")
source("LRAR/caren.R")
source("LRAR/predictLRAR.R")
source("edira/edira.R")


# read DATA
## PT
out <- read.csv("../data/out_german_based_charfix.csv", sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")
## GET
#out <- read.csv("../data/out_german.csv", sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")

# transform DATA
## select rows of relevant parties
### PT
rankings <- out[,c(3,4,11,15,29)]
### GER
#rankings <- out[,c(33:37)]
## split in independent variables (x) and votes (y)
### PT
y <- t(apply(-rankings, 1, rank))
x <- out[,-c(0:33)]
### GER
#y <- t(apply(rankings, 1, rank))
#x <- out[,c(4:32)]
## transform dataframes in matrixes
x <- as.matrix(x)
y <- as.matrix(y)
## sample size percentage for TRAIN
smp_size <- floor(0.75 * nrow(x))
## set the seed to make the partition reproductible
set.seed(123)
## split
train_ind <- sample(seq_len(nrow(x)), size = smp_size)
x <- x[train_ind, ]
xs <- x[-train_ind, ]
y <- y[train_ind, ]
ys <- y[-train_ind, ]

# discretize TRAIN
DISC <- mdlp.rank(x, y, method = "kendall")
xd = DISC$Disc.data

# mine LR rules
rulz <- aflrC7(xd, y, msup = 1, mconf = 70, mlift = 0, mimp = 0, theta = 0, Xmx = "2000M")

getNonZeroIndexes <- function(cutp) {
  out <- c()
  outIndex <- 1
  zeroRow <- list(numeric(0))
  for (i in 1:length(cutp)) {
    l <- cutp[i]
    if (!identical(l, zeroRow)) {
      out[outIndex] <- i
      outIndex <- outIndex + 1
    }
  }
  return(out)
}

validIndexes <- getNonZeroIndexes(DISC$cutp)
#xs <- xs[, validIndexes]
#DISC$cutp = DISC$cutp[validIndexes]

# discretize TEST 
xsd <- sapply(1:ncol(xs), function(j){
  findInterval(xs[,j], c(-Inf, DISC$cutp[[j]], Inf))
})

# predict LRARs
yp <- crank(rulz, xsd, ys, std, m2, 1)#mt)

# evaluate predictions
tau <- mean(sapply(1:nrow(ys), function(j) cor(ys[j,], yp[,j], method="kendall")))
tau
