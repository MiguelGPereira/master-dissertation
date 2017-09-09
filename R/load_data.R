out <- read.csv("../data/out_german_based.csv", sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")
colnames(out)
#correr "colnames(out)" e refinar o comando a seguir para os maiores partidos
rankings <- out[,c(3,4,11,15,29)]
head(rankings)#mostar 6 linhas
y <- t(apply(-rankings, 1, rank))
head(y)
#unique rankings: unique(y)
#number of unique rankings: nrow(unique(y))/nrow(y)
#percentage of unique rankings: nrow(unique(y))/nrow(y)
#histogram of ranking distribution regarding the party BE: hist(rankings$BE)
#number of rankings where BE got more than 10000 votes: which(rankings$BE > 10000)
dados <- out[,-c(0:33)]
x <- dados
head(x)
source("labelrankingforests/lrf.R")
pred(x,y)
