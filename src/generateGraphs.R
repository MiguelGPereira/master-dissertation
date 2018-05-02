library(ggplot2)

dataset <- list()
gamma <- list()
completeness <- list()
accuracy <- list()
rules <- list()

processFile = function(filepath) {
  con = file(filepath, "r")
  while (TRUE) {
    line = readLines(con, n = 1)
    if (length(line) == 0) {
      break
    }
    if (substr(line, 0, 8) == "%RFORMAT") {
      data <- strsplit(line, "%")
      dataset <<- c(dataset,data[[1]][3])
      gamma <<- c(gamma,data[[1]][7])
      #cat(data[[1]][3],data[[1]][7],"\n",sep = "    ")
      completeness <<- c(completeness,data[[1]][8])
      accuracy <<- c(accuracy,data[[1]][9])
      rules <<- c(rules,data[[1]][11])
    }
  }
  
  close(con)
}

processFile("job-all-pairwise-final")

dataset <- sapply( dataset, paste0, collapse="")
gamma <- sapply( gamma, paste0, collapse="")
completeness <- sapply( completeness, paste0, collapse="")
accuracy <- sapply( accuracy, paste0, collapse="")
rules <- sapply( rules, paste0, collapse="")


dataframe <- data.frame(dataset, gamma, completeness, accuracy)

dataframe <- cbind(dataframe, labels = NA)
dataframe[dataframe[,"dataset"]=="iris","labels"] <- 3
dataframe[dataframe[,"dataset"]=="bodyfat","labels"] <- 7
dataframe[dataframe[,"dataset"]=="glass","labels"] <- 6
dataframe[dataframe[,"dataset"]=="housing","labels"] <- 6
dataframe[dataframe[,"dataset"]=="stock","labels"] <- 5
dataframe[dataframe[,"dataset"]=="vehicle","labels"] <- 4
dataframe[dataframe[,"dataset"]=="wine","labels"] <- 3
dataframe[dataframe[,"dataset"]=="cpu-small","labels"] <- 5

dataframe[dataframe[,"dataset"]=="german2005_2009","labels"] <- 5
dataframe[dataframe[,"dataset"]=="portugal2013_2017","labels"] <- 5
dataframe[dataframe[,"dataset"]=="portugal2009_2013","labels"] <- 5

dataframe$gamma=as.numeric(levels(dataframe$gamma))[dataframe$gamma]
dataframe$accuracy=as.numeric(levels(dataframe$accuracy))[dataframe$accuracy]
dataframe$completeness=as.numeric(levels(dataframe$completeness))[dataframe$completeness]

ggplot(dataframe, aes(x=accuracy, y=completeness, group=dataset, color=dataset))+geom_point()+geom_path()+scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1.0), limits = c(0,1))+scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1.0), limits = c(0,1))+coord_fixed()
ggplot(dataframe, aes(x=accuracy, y=completeness, color=dataset))+geom_point()+geom_path()+scale_x_continuous(breaks = c(0,0.5,1.0), limits = c(0,1))+scale_y_continuous(breaks = c(0,0.5,1.0), limits = c(0,1))+coord_fixed()+facet_wrap(~dataset, nrow = 2, ncol = 4)
ggplot(dataframe, aes(x=accuracy, y=completeness, group=dataset, color=labels))+geom_point()+geom_path()+scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1.0), limits = c(0,1))+scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1.0), limits = c(0,1))+coord_fixed()









