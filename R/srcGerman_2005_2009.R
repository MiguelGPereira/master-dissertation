# load dependencies
require("carenR")
source("LRAR/caren.R")
source("LRAR/predictLRAR.R")
source("edira/edira.R")

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

# read DATA
## GET
outTrain <- read.csv("../data/out_german_2005.csv", sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")
outTest <- read.csv("../data/out_german.csv", sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")

# transform DATA (TRAIN)
## select rows of relevant parties
### GER
rankings <- outTrain[,c(33:37)]
## split in independent variables (x) and votes (y)
### GER
y <- t(apply(rankings, 1, rank))
x <- outTrain[,c(4:32)]
## transform dataframes in matrixes
x <- as.matrix(x)
y <- as.matrix(y)

# transform DATA (TEST)
## select rows of relevant parties
### GER
rankings <- outTest[,c(33:37)]
## split in independent variables (x) and votes (y)
### GER
ys <- t(apply(rankings, 1, rank))
xs <- outTest[,c(4:32)]
## transform dataframes in matrixes
xs <- as.matrix(xs)
ys <- as.matrix(ys)

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
rulz <- aflrC7(xd, y, msup = 1, mconf = 90, mlift = 0, mimp = 0.01, theta = 0, Xmx = "2000M")

# predict LRARs
std <- def.rank(y)
mt <- 0
yp <- crank(rulz, xs, ys, std, m2, mt)

# evaluate predictions
tau <- mean(sapply(1:nrow(ys), function(j) cor(ys[j,], yp[,j], method="kendall")))
tau
