# ----- German 2005 ------------------------------------------------------------
data <- "../data/out_german_2005.csv"
atributes = c(4:32)
rankings = c(33:37)
is2Years <- F

out <- read.csv(data, sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")

Y <- out[,rankings]
Y <- t(apply(Y, 1, rank))
Y <- as.matrix(Y)

X <- out[,atributes]
X <- as.matrix(X)

save(X, Y, is2Years, file = "german2005.RDATA")


# German 2009 ------------------------------------------------------------------
data <- "../data/out_german.csv"
atributes = c(4:32)
rankings = c(33:37)
is2Years <- F

out <- read.csv(data, sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")

Y <- out[,rankings]
Y <- t(apply(Y, 1, rank))
Y <- as.matrix(Y)

X <- out[,atributes]
X <- as.matrix(X)

save(X, Y, is2Years, file = "german2009.RDATA")

# ----- German 2005_2009 ------------------------------------------------------
dataTrain <- "../data/out_german_2005.csv"
dataTest <- "../data/out_german.csv"
atributes = c(4:32)
rankings = c(33:37)
is2Years <- T

outTrain <- read.csv(dataTrain, sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")
outTest <- read.csv(dataTest, sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")

Y <- outTrain[,rankings]
Y <- t(apply(Y, 1, rank))
Y <- as.matrix(Y)

ys <- outTest[,rankings]
ys <- t(apply(ys, 1, rank))
ys <- as.matrix(ys)

X <- outTrain[,atributes]
X <- as.matrix(X)

xs <- outTest[,atributes]
xs <- as.matrix(xs)

save(X, Y, xs, ys, is2Years, file = "german2005_2009.RDATA")

# ----- Portugal 2013 ------------------------------------------------------------
data <- "../data/out_german_based_charfix.csv"
atributes = -c(0:33)
rankings = c(3,4,11,15,29)
is2Years <- F

out <- read.csv(data, sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")

Y <- out[,rankings]
Y <- t(apply(-Y, 1, rank))
Y <- as.matrix(Y)

X <- out[,atributes]
X <- as.matrix(X)
save(X, Y, is2Years, file = "portugal2013.RDATA")
