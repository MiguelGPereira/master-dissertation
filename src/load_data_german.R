out <- read.csv("../data/out_german.csv", sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")
colnames(out)
#correr "colnames(out)" e refinar o comando a seguir para os maiores partidos
rankings <- out[,c(33:37)]
head(rankings)#mostar 6 linhas
y <- t(apply(rankings, 1, rank))
#or, since the problem is just putting in matrix format: y <- as.matrix(rankings)
#y <- rankings
head(y)
dados <- out[,c(4:32)]
x <- dados
head(x)
source("labelrankingforests/lrf.R")
pred(x,y)