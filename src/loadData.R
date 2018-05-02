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
data <- "../data/out_portugal_2013_nc.csv"
atributes = c(34,36,38,40,42,44:47,49,51,53,55,57,59,61,63:65,68)

rankings = c(3,4,11,15,29)
is2Years <- F

out <- read.csv(data, sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")
outColNames <- colnames(out)
out <- apply(out, 1, function(line){
  cds <- c(4:7, 16:20)
  line[4] <- max(as.numeric(line[cds]))
  psd <- c(15:24)
  line[15] <- max(as.numeric(line[psd]))
  as.numeric(line)
})
out <- t(out)
colnames(out) <- outColNames

Y <- out[,rankings]
Y <- t(apply(-Y, 1, rank, ties.method = "min"))
Y <- as.matrix(Y)

X <- out[,atributes]
X <- as.matrix(X)
X[X == "#DIV/0!"] <- 0
X <- apply(X, 2, function(l) as.numeric(l))
save(X, Y, is2Years, file = "portugal2013.RDATA")

# ----- Portugal 2009 ------------------------------------------------------------
data <- "../data/out_portugal_2009_nc.csv"
atributes = c(34,36,38,40,42,44:47,49,51,53,55,57,59,61,63:65,68)
rankings = c(3,4,11,15,29)
is2Years <- F

out <- read.csv(data, sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")
outColNames <- colnames(out)
out <- apply(out, 1, function(line){
  cds <- c(4:7, 16:20)
  line[4] <- max(as.numeric(line[cds]))
  psd <- c(15:24)
  line[15] <- max(as.numeric(line[psd]))
  as.numeric(line)
})
out <- t(out)
colnames(out) <- outColNames

Y <- out[,rankings]
Y <- t(apply(-Y, 1, rank, ties.method = "min"))
Y <- as.matrix(Y)

X <- out[,atributes]
X <- as.matrix(X)
X[X == "#DIV/0!"] <- 0
X <- apply(X, 2, function(l) as.numeric(l))

residentsTotal <- out[,"Residents.Total"]
employeesTotal <- out[,"Employees.per.Sector.Total"]
unemploymentTotal <- out[,"Unemployment.Total"]
X[,"X.Residents.0.14"] <- out[,"Residents.0.14"]/residentsTotal 
X[,"X.Residents.15.64"] <- out[,"Residents.15.64"]/residentsTotal 
X[,"X.Residents.65."] <- out[,"Residents.65."]/residentsTotal 
X[,"X.Population.without.Education"] <- out[,"Population.without.Education"]/residentsTotal 
X[,"X.Population.with.High.School"] <- out[,"Population.with.High.School"]/residentsTotal 
X[,"X.Employees.per.Sector.Agriculture"] <- out[,"Employees.per.Sector.Agriculture"]/employeesTotal 
X[,"X.Employees.per.Sector.Extraction.Industries"] <- out[,"Employees.per.Sector.Extraction.Industries"]/employeesTotal 
X[,"X.Employees.per.Sector.Transformation.Industries"] <- out[,"Employees.per.Sector.Transformation.Industries"]/employeesTotal 
X[,"X.Employees.per.Sector.Construction"] <- out[,"Employees.per.Sector.Construction"]/employeesTotal 
X[,"X.Employees.per.Sector.Gross.and.Retails.Markets"] <- out[,"Employees.per.Sector.Gross.and.Retails.Markets"]/employeesTotal 
X[,"X.Employees.per.Sector.Administration.and.Services"] <- out[,"Employees.per.Sector.Administration.and.Services"]/employeesTotal 
X[,"X.Employees.per.Economic.Sector.Banks"] <- out[,"Employees.per.Economic.Sector.Banks"]/employeesTotal 
X[,"X.Employees.per.Economic.Sector.Others"] <- out[,"Employees.per.Economic.Sector.Others"]/employeesTotal 
X[,"X.Unemployment..25"] <- out[,"Unemployment..25"]/unemploymentTotal 

save(X, Y, is2Years, file = "portugal2009.RDATA")

# ----- Portugal 2009_2013 ------------------------------------------------------
dataTrain <- "../data/out_portugal_2009_nc.csv"
dataTest <- "../data/out_portugal_2013_nc.csv"
atributes = c(34,36,38,40,42,44:47,49,51,53,55,57,59,61,63:65,68)
rankings = c(3,4,11,15,29)
is2Years <- T

outTrain <- read.csv(dataTrain, sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")
outColNames <- colnames(outTrain)
outTrain <- apply(outTrain, 1, function(line){
  cds <- c(4:7, 16:20)
  line[4] <- max(as.numeric(line[cds]))
  psd <- c(15:24)
  line[15] <- max(as.numeric(line[psd]))
  as.numeric(line)
})
outTrain <- t(outTrain)
colnames(outTrain) <- outColNames
outTest <- read.csv(dataTest, sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")
outColNames <- colnames(outTest)
outTest <- apply(outTest, 1, function(line){
  cds <- c(4:7, 16:20)
  line[4] <- max(as.numeric(line[cds]))
  psd <- c(15:24)
  line[15] <- max(as.numeric(line[psd]))
  as.numeric(line)
})
outTest <- t(outTest)
colnames(outTest) <- outColNames

Y <- outTrain[,rankings]
Y <- t(apply(-Y, 1, rank, ties.method = "min"))
Y <- as.matrix(Y)

ys <- outTest[,rankings]
ys <- t(apply(-ys, 1, rank, ties.method = "min"))
ys <- as.matrix(ys)

X <- outTrain[,atributes]
X <- as.matrix(X)
X[X == "#DIV/0!"] <- 0
X <- apply(X, 2, function(l) as.numeric(l))
residentsTotal <- outTrain[,"Residents.Total"]
employeesTotal <- outTrain[,"Employees.per.Sector.Total"]
unemploymentTotal <- outTrain[,"Unemployment.Total"]
X[,"X.Residents.0.14"] <- outTrain[,"Residents.0.14"]/residentsTotal 
X[,"X.Residents.15.64"] <- outTrain[,"Residents.15.64"]/residentsTotal 
X[,"X.Residents.65."] <- outTrain[,"Residents.65."]/residentsTotal 
X[,"X.Population.without.Education"] <- outTrain[,"Population.without.Education"]/residentsTotal 
X[,"X.Population.with.High.School"] <- outTrain[,"Population.with.High.School"]/residentsTotal 
X[,"X.Employees.per.Sector.Agriculture"] <- outTrain[,"Employees.per.Sector.Agriculture"]/employeesTotal 
X[,"X.Employees.per.Sector.Extraction.Industries"] <- outTrain[,"Employees.per.Sector.Extraction.Industries"]/employeesTotal 
X[,"X.Employees.per.Sector.Transformation.Industries"] <- outTrain[,"Employees.per.Sector.Transformation.Industries"]/employeesTotal 
X[,"X.Employees.per.Sector.Construction"] <- outTrain[,"Employees.per.Sector.Construction"]/employeesTotal 
X[,"X.Employees.per.Sector.Gross.and.Retails.Markets"] <- outTrain[,"Employees.per.Sector.Gross.and.Retails.Markets"]/employeesTotal 
X[,"X.Employees.per.Sector.Administration.and.Services"] <- outTrain[,"Employees.per.Sector.Administration.and.Services"]/employeesTotal 
X[,"X.Employees.per.Economic.Sector.Banks"] <- outTrain[,"Employees.per.Economic.Sector.Banks"]/employeesTotal 
X[,"X.Employees.per.Economic.Sector.Others"] <- outTrain[,"Employees.per.Economic.Sector.Others"]/employeesTotal 
X[,"X.Unemployment..25"] <- outTrain[,"Unemployment..25"]/unemploymentTotal

xs <- outTest[,atributes]
xs <- as.matrix(xs)
xs[xs == "#DIV/0!"] <- 0
xs <- apply(xs, 2, function(l) as.numeric(l)) 

save(X, Y, xs, ys, is2Years, file = "portugal2009_2013.RDATA")

# ----- Portugal 2013_2017 ------------------------------------------------------
dataTrain <- "../data/out_portugal_2013_nc.csv"
dataTest <- "../data/out_portugal_2017_nc.csv"
atributes = c(34,36,38,40,42,44:47,49,51,53,55,57,59,61,63:65,68)
rankings = c(3,4,11,15,29)
is2Years <- T

outTrain <- read.csv(dataTrain, sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")
outColNames <- colnames(outTrain)
outTrain <- apply(outTrain, 1, function(line){
  cds <- c(4:7, 16:20)
  line[4] <- max(as.numeric(line[cds]))
  psd <- c(15:24)
  line[15] <- max(as.numeric(line[psd]))
  as.numeric(line)
})
outTrain <- t(outTrain)
colnames(outTrain) <- outColNames
outTest <- read.csv(dataTest, sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")

Y <- outTrain[,rankings]
Y <- t(apply(-Y, 1, rank, ties.method = "min"))
Y <- as.matrix(Y)

ys <- 0

X <- outTrain[,atributes]
X <- as.matrix(X)
X[X == "#DIV/0!"] <- 0
X <- apply(X, 2, function(l) as.numeric(l))

xs <- outTest[,atributes]
xs <- as.matrix(xs)
xs[xs == "#DIV/0!"] <- 0
xs <- apply(xs, 2, function(l) as.numeric(l))
residentsTotal <- outTest[,"Residents.Total"]
employeesTotal <- outTest[,"Employees.per.Sector.Total"]
unemploymentTotal <- outTest[,"Unemployment.Total"]
xs[,"X.Residents.0.14"] <- outTest[,"Residents.0.14"]/residentsTotal 
xs[,"X.Residents.15.64"] <- outTest[,"Residents.15.64"]/residentsTotal 
xs[,"X.Residents.65."] <- outTest[,"Residents.65."]/residentsTotal 
xs[,"X.Population.without.Education"] <- outTest[,"Population.without.Education"]/residentsTotal 
xs[,"X.Population.with.High.School"] <- outTest[,"Population.with.High.School"]/residentsTotal 
xs[,"X.Employees.per.Sector.Agriculture"] <- outTest[,"Employees.per.Sector.Agriculture"]/employeesTotal 
xs[,"X.Employees.per.Sector.Extraction.Industries"] <- outTest[,"Employees.per.Sector.Extraction.Industries"]/employeesTotal 
xs[,"X.Employees.per.Sector.Transformation.Industries"] <- outTest[,"Employees.per.Sector.Transformation.Industries"]/employeesTotal 
xs[,"X.Employees.per.Sector.Construction"] <- outTest[,"Employees.per.Sector.Construction"]/employeesTotal 
xs[,"X.Employees.per.Sector.Gross.and.Retails.Markets"] <- outTest[,"Employees.per.Sector.Gross.and.Retails.Markets"]/employeesTotal 
xs[,"X.Employees.per.Sector.Administration.and.Services"] <- outTest[,"Employees.per.Sector.Administration.and.Services"]/employeesTotal 
xs[,"X.Employees.per.Economic.Sector.Banks"] <- outTest[,"Employees.per.Economic.Sector.Banks"]/employeesTotal 
xs[,"X.Employees.per.Economic.Sector.Others"] <- outTest[,"Employees.per.Economic.Sector.Others"]/employeesTotal 
xs[,"X.Unemployment..25"] <- outTest[,"Unemployment..25"]/unemploymentTotal 

save(X, Y, xs, ys, is2Years, file = "portugal2013_2017_incomplete.RDATA")

# ----- Portugal 2013_2017 COMPLETE ------------------------------------------------------
dataTrain <- "../data/out_portugal_2013_nc.csv"
dataTest <- "../data/out_portugal_2017_complete_nc.csv"
atributes = c(34,36,38,40,42,44:47,49,51,53,55,57,59,61,63:65,68)
rankings = c(3,4,11,15,29)
is2Years <- T

outTrain <- read.csv(dataTrain, sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")
outColNames <- colnames(outTrain)
outTrain <- apply(outTrain, 1, function(line){
  cds <- c(4:7, 16:20)
  line[4] <- max(as.numeric(line[cds]))
  psd <- c(15:24)
  line[15] <- max(as.numeric(line[psd]))
  as.numeric(line)
})
outTrain <- t(outTrain)
colnames(outTrain) <- outColNames
outTest <- read.csv(dataTest, sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")

Y <- outTrain[,rankings]
Y <- t(apply(-Y, 1, rank, ties.method = "min"))
Y <- as.matrix(Y)

ys <- outTest[,rankings]
ys <- t(apply(-ys, 1, rank, ties.method = "min"))
ys <- as.matrix(ys)

X <- outTrain[,atributes]
X <- as.matrix(X)
X[X == "#DIV/0!"] <- 0
X <- apply(X, 2, function(l) as.numeric(l))

xs <- outTest[,atributes]
xs <- as.matrix(xs)
xs[xs == "#DIV/0!"] <- 0
xs <- apply(xs, 2, function(l) as.numeric(l))
residentsTotal <- outTest[,"Residents.Total"]
employeesTotal <- outTest[,"Employees.per.Sector.Total"]
unemploymentTotal <- outTest[,"Unemployment.Total"]
xs[,"X.Residents.0.14"] <- outTest[,"Residents.0.14"]/residentsTotal 
xs[,"X.Residents.15.64"] <- outTest[,"Residents.15.64"]/residentsTotal 
xs[,"X.Residents.65."] <- outTest[,"Residents.65."]/residentsTotal 
xs[,"X.Population.without.Education"] <- outTest[,"Population.without.Education"]/residentsTotal 
xs[,"X.Population.with.High.School"] <- outTest[,"Population.with.High.School"]/residentsTotal 
xs[,"X.Employees.per.Sector.Agriculture"] <- outTest[,"Employees.per.Sector.Agriculture"]/employeesTotal 
xs[,"X.Employees.per.Sector.Extraction.Industries"] <- outTest[,"Employees.per.Sector.Extraction.Industries"]/employeesTotal 
xs[,"X.Employees.per.Sector.Transformation.Industries"] <- outTest[,"Employees.per.Sector.Transformation.Industries"]/employeesTotal 
xs[,"X.Employees.per.Sector.Construction"] <- outTest[,"Employees.per.Sector.Construction"]/employeesTotal 
xs[,"X.Employees.per.Sector.Gross.and.Retails.Markets"] <- outTest[,"Employees.per.Sector.Gross.and.Retails.Markets"]/employeesTotal 
xs[,"X.Employees.per.Sector.Administration.and.Services"] <- outTest[,"Employees.per.Sector.Administration.and.Services"]/employeesTotal 
xs[,"X.Employees.per.Economic.Sector.Banks"] <- outTest[,"Employees.per.Economic.Sector.Banks"]/employeesTotal 
xs[,"X.Employees.per.Economic.Sector.Others"] <- outTest[,"Employees.per.Economic.Sector.Others"]/employeesTotal 
xs[,"X.Unemployment..25"] <- outTest[,"Unemployment..25"]/unemploymentTotal 

save(X, Y, xs, ys, is2Years, file = "portugal2013_2017.RDATA")


