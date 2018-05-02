# load dependencies
require("carenR")
source("LRAR/caren.R")
source("LRAR/predictLRAR.R")
source("edira/edira.R")

#settings
M_PAIRS <- 2

# read DATA
## GET
out <- read.csv("../data/out_german.csv", sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")

# transform DATA
## select rows of relevant parties
### GER
rankings <- out[,c(33:37)]
## split in independent variables (x) and votes (y)
### GER
y <- t(apply(rankings, 1, rank))
x <- out[,c(4:32)]
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

# mine LR Pairwise rules
rulz <- aflrC7Pairwise(xd, y, msup = 20, mconf = 90, mlift = 1, mimp = 0, theta = 0, maxpairs = M_PAIRS, Xmx = "2000M")

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

# discretize TEST 
# xsd <- sapply(1:ncol(xs), function(j){
#   findInterval(xs[,j], c(-Inf, DISC$cutp[[j]], Inf))
# })

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
str(res)

# predict LRARs
yp <- crankPair(rulz, xs, ys, std, m2, 1)#mt)

# evaluate predictions
tau <- mean(sapply(1:nrow(ys), function(j) cor(ys[j,], yp[,j], method="kendall")))
tau
