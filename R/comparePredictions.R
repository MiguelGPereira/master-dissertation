# compare predictions
yp <- readRDS("predictions.rds")
y <- readRDS("results.rds")

changes <- list()
for(i in 1:nrow(yp)){
  predict <- yp[i,]
  result <- y[i,]
  valid <- which(!is.na(predict))
  predict <- predict[valid]
  result <- result[valid]
  totalSum <- NULL
  for(k in 1:length(predict)){
    base <- result[k]
    test <- result[predict > predict[k]]
    out <- sum(test[test < base])
    if(out > 0) {
      diffs <- paste(names(test[test < base]), collapse = " ^ ")
     if(is.null(totalSum)) {
       totalSum <<- paste(names(base), ": {", diffs, "}")
     } else {
       totalSum <<- paste(totalSum," ^ ",names(base), ": {", diffs, "}")
     }
    }
  }
  if(is.null(totalSum)){
    changes[[i]] <- "none"
  } else {
    changes[[i]] <- totalSum
  }
}

data <- "../data/out_portugal_2013_nc.csv"
csv <- read.csv(data, sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")

out <- cbind(csv[,1],y,changes,yp)
View(out)

notNone <- which(changes != "none")
write.table(out[notNone,], file = "LRARs2017.csv",row.names=T, na="",col.names=T, sep=";")




