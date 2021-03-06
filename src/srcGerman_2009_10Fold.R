# load dependencies
require("carenR")
source("LRAR/caren.R")
source("LRAR/predictLRAR.R")
source("edira/edira.R")


# read DATA
out <- read.csv("../data/out_german.csv", sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")

# transform DATA
# select rows of relevant parties
rankings <- out[,c(33:37)]

# split in independent variables (x) and votes (y)
Y <- t(apply(rankings, 1, rank))
X <- out[,c(4:32)]

# transform dataframes in matrixes
X <- as.matrix(X)
Y <- as.matrix(Y)

# KFold Cross Validation
K <- 10

# Randomly shuffle the data
set.seed(1234)
samplerows <- sample(nrow(X))
X <- X[samplerows, ]
Y <- Y[samplerows, ]

# Create K equally size folds
folds <- cut(seq(1, nrow(X)), breaks=K, labels=FALSE)

# Perform K fold cross validation
test.function<-function(){
  for(i in 1:K){
    # Segement your data by fold
    testIndexes <- which(folds==i)
    train_ind <- -testIndexes
    
    x <- X[train_ind, ]
    xs <- X[-train_ind, ]
    y <- Y[train_ind, ]
    ys <- Y[-train_ind, ]
    
    # Use the test and train data partitions
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
    #print(std)
    yp <- crank(rulz, xs, ys, std, m2, 1)#mt)

    # evaluate predictions
    tau <- mean(sapply(1:nrow(ys), function(j) cor(ys[j,], yp[,j], method="kendall")))

    print(paste("fold", i, ": tau=", tau))
  }
}
test.function()
