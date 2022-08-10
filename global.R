library(cfda)

##Create table with the summary of statistics(jumps, duration timeSpent) for all data set 
tableOfStatsAll<- function(resStatsAll,stats){
  if(stats %in% c("jump","duration")){
    q <- quantile(resStatsAll, seq(0, 1, 0.25))
    d <- data.frame(
      Mean = round(mean(resStatsAll), 2),
      Median = round(q[3], 2),
      Q1 = round(q[2], 2),
      Q3 = round(q[4], 2),
      Min = round(q[1], 2),
      Max = round(q[5], 2),
      Sd = round(sd(resStatsAll), 2)
    )
    row.names(d) <- c("All")
    return(d)
  }else if(stats=="time"){
    d <- as.data.frame(matrix(ncol = 8, nrow = 0))
    colnames(d) <- c("Mean", "Median", "Q1", "Q3", "Min", "Max", "Sd")
    mod = colnames(resStatsAll)
    for (i in mod) {
      time <- resStatsAll[, i]
      q <- quantile(time)
      d <- rbind.data.frame(d, data.frame(
        Mean = round(mean(time), 2),
        Median = round(q[3], 2),
        Q1 = round(q[2], 2),
        Q3 = round(q[4], 2),
        Min = round(q[1], 2),
        Max = round(q[5], 2),
        Sd = round(sd(time), 2)
      )
      )
    }
    row.names(d) <- mod
    return(d)
  }
}

##Create table with the summary of statistics(jumps, duration timeSpent) by group variable 
tableOfStatsByGroup<- function(data,resStatsAll,stats,groupVar,modalites,nomState){
  if(stats %in% c("jump","duration")){
    d <- as.data.frame(matrix(ncol = 9, nrow = 0))
    colnames(d) <- c("Mean", "Median", "Q1", "Q3", "Min", "Max", "Sd", "Number")
    for(i in modalites){
      data2 <- data[data[, groupVar] == i, ]
      if(stats=="jump"){
        res<-compute_number_jumps(data2[, c("id", "time", "state")])
      }else{
        res <- compute_duration(data2[, c("id", "time", "state")])
      }
      q <- quantile(res, seq(0, 1, 0.25))
      d <- rbind.data.frame(
        d,
        data.frame(
          Mean = round(mean(res), 2),
          Median = round(q[3], 2),
          Q1 = round(q[2], 2),
          Q3 = round(q[4], 2),
          Min = round(q[1], 2),
          Max = round(q[5], 2),
          Sd = round(sd(res), 2),
          Number = length(res)
        )
      )
    }
    q <- quantile(resStatsAll, seq(0, 1, 0.25))
    d <-
      rbind.data.frame(
        d,
        data.frame(
          Mean = round(mean(resStatsAll), 2),
          Median = round(q[3], 2),
          Q1 = round(q[2], 2),
          Q3 = round(q[4], 2),
          Min = round(q[1], 2),
          Max = round(q[5], 2),
          Sd = round(sd(resStatsAll), 2),
          Number = length(resStatsAll)
        )
      )
    row.names(d) <- c(modalites,"All")
    d
    return(d)
  }
  else if(stats=="time"){
    d <- as.data.frame(matrix(ncol = 8, nrow = 0))
    for (i in modalites) {
      idToKeep <- unique(data[data[, groupVar] == i, "id"])
      time <- resStatsAll[names(resStatsAll[, nomState]) %in% idToKeep, nomState]
      q <- quantile(time)
      d <- rbind.data.frame(
        d,
        data.frame(
          Mean = round(mean(time), 2),
          Median = round(q[3], 2),
          Q1 = round(q[2], 2),
          Q3 = round(q[4], 2),
          Min = round(q[1], 2),
          Max = round(q[5], 2),
          Sd = round(sd(time), 2),
          Number = length(time)
        )
      )
    }
    time <- resStatsAll[, nomState]
    q <- quantile(time)
    d <- rbind.data.frame(
      d,
      data.frame(
        Mean = round(mean(time), 2),
        Median = round(q[3], 2),
        Q1 = round(q[2], 2),
        Q3 = round(q[4], 2),
        Min = round(q[1], 2),
        Max = round(q[5], 2),
        Sd = round(sd(time), 2),
        Number = length(time)
      )
    )
    row.names(d) <- c(modalites, "All")
    return(d)
  }
  
}

##Create table with the summary of statistics(jumps, duration timeSpent) by cluster
tableOfStatsCluster<-function(data,resStatsAll,stats,class,nomState,nbClust){
  if(stats %in% c("jump","duration")){
    d <- as.data.frame(matrix(ncol = 8, nrow = 0))
    colnames(d) <-c("Mean", "Median", "Q1", "Q3", "Min", "Max", "Sd", "Number")
    for (i in c(1:nbClust)) {
      idToKeep <- names(class[class == i])
      data2 <- data[data$id %in% idToKeep, ]
      if(stats=="jump"){
        res<-compute_number_jumps(data2[, c("id", "time", "state")])
      }else{
        res <- compute_duration(data2[, c("id", "time", "state")])
      }
      q <- quantile(res, seq(0, 1, 0.25))
      d <- rbind.data.frame(
        d,
        data.frame(
          Mean = round(mean(res), 2),
          Median = round(q[3], 2),
          Q1 = round(q[2], 2),
          Q3 = round(q[4], 2),
          Min = round(q[1], 2),
          Max = round(q[5], 2),
          Sd = round(sd(res), 2),
          Number = length(res)
          
        )
      )
    }
    res <- resStatsAll
    q <- quantile(res, seq(0, 1, 0.25))
    d <- rbind.data.frame(
      d,
      data.frame(
        Mean = round(mean(res), 2),
        Median = round(q[3], 2),
        Q1 = round(q[2], 2),
        Q3 = round(q[4], 2),
        Min = round(q[1], 2),
        Max = round(q[5], 2),
        Sd = round(sd(res), 2),
        Number = length(res)
      )
    )
    row.names(d) <- c(paste("Cluster",1:nbClust),"All")
    return(d)
  }
  else if(stats=="time"){
    d <- as.data.frame(matrix(ncol = 8, nrow = 0))
    for (i in c(1:nbClust)) {
      idToKeep <- names(class[class == i])
      time <- resStatsAll[names(resStatsAll[, nomState]) %in% idToKeep, nomState]
      q <- quantile(time)
      d <- rbind.data.frame(
        d,
        data.frame(
          Mean = round(mean(time), 2),
          Median = round(q[3], 2),
          Q1 = round(q[2], 2),
          Q3 = round(q[4], 2),
          Min = round(q[1], 2),
          Max = round(q[5], 2),
          Sd = round(sd(time), 2),
          Number = length(time)
        )
      )
    }
    time <- resStatsAll[, nomState]
    q <- quantile(time)
    d <- rbind.data.frame(
      d,
      data.frame(
        Mean = round(mean(time), 2),
        Median = round(q[3], 2),
        Q1 = round(q[2], 2),
        Q3 = round(q[4], 2),
        Min = round(q[1], 2),
        Max = round(q[5], 2),
        Sd = round(sd(time), 2),
        Number = length(time)
      )
    )
    row.names(d) <- c(paste("Cluster",1:nbClust),"All")
    return(d)
  }
}