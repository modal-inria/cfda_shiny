if (!require("shiny")) {
  install.packages("shiny")
  library("shiny")
}
if (!require("shinydashboard")) {
  install.packages("shinydashboard")
  library("shinydashboard")
}
if (!require("shinyMatrix")) {
  install.packages("shinyMatrix")
  library("shinyMatrix")
}
if (!require("shinyWidgets")) {
  install.packages("shinyWidgets")
  library("shinyWidgets")
}
if (!require("shinycssloaders")) {
  install.packages("shinycssloaders")
  library("shinycssloaders")
}
if (!require("DT")) {
  install.packages("DT")
  library("DT")
}
if (!require("plotly")) {
  install.packages("plotly")
  library("plotly")
}
if (!require("cfda")) {
  install.packages("cfda")
  library("cfda")
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}
if (!require("scales")) {
  install.packages("scales")
  library("scales")
}

#############
# Variables #
#############

# maximum number of modalities for a variable to be considered as a categorical group variable
# (for the plot and the table by group in descriptive statistics and estimation of markov chain parts)
MAXMOD <- 12


#############
# Functions #
#############

# check that the number of modality does neot exceed the limit
checkNumberOfModality <- function(nModality, maxNModality = MAXMOD) {
  validate(
    need(
      nModality <= maxNModality,
      paste0("This variable has too many modalities (", nModality, "), the limit is ", maxNModality, ".")
    )
  )
}

# check that the selected group variable has not more than 1 modality per individual
checkGroupVariable <- function(data, groupVariableName) {
  validate(
    need(
      nrow(unique(data[, c("id", groupVariableName)])) == length(unique(data$id)),
      "This variable can't be used as group variable because some individuals has more than 1 modality for this variable."
    )
  )
}

# get the list of states
getStates <- function(data) {
  if (is.factor(data$state)) {
    states <- levels(data$state)
  } else {
    states <- as.character(sort(unique(data$state)))
  }

  return(states)
}

# For a given variable get a 1-ow data.frame with Mean, Median, Q1, Q3, Min, Max, Sd (and Number)
computeStatsDataFrame <- function(vector, addNumber = FALSE) {
  q <- quantile(vector)

  d <- data.frame(
    Mean = round(mean(vector), 2),
    Median = round(q[3], 2),
    Q1 = round(q[2], 2),
    Q3 = round(q[4], 2),
    Min = round(q[1], 2),
    Max = round(q[5], 2),
    Sd = round(sd(vector), 2)
  )
  if (addNumber) {
    d$Number <- length(vector)
  }

  return(d)
}

## Create table with the summary of statistics(jumps, duration timeSpent) for all data set
tableOfStatsAll <- function(resStatsAll, stats) {
  if (stats %in% c("jump", "duration")) {
    d <- computeStatsDataFrame(resStatsAll, addNumber = FALSE)
    row.names(d) <- c("All")

    return(d)
  } else if (stats == "time") {
    d <- as.data.frame(matrix(ncol = 8, nrow = 0))
    colnames(d) <- c("Mean", "Median", "Q1", "Q3", "Min", "Max", "Sd")
    mod <- colnames(resStatsAll)
    for (i in mod) {
      dNew <- computeStatsDataFrame(resStatsAll[, i], addNumber = FALSE)
      d <- rbind.data.frame(d, dNew)
    }
    row.names(d) <- mod

    return(d)
  }
}

## Create table with the summary of statistics (jumps, duration, timeSpent) by group variable
tableOfStatsByGroup <- function(data, resStatsAll, stats, groupVar, modalites, nomState) {
  if (stats %in% c("jump", "duration")) {
    d <- as.data.frame(matrix(ncol = 9, nrow = 0))
    colnames(d) <- c("Mean", "Median", "Q1", "Q3", "Min", "Max", "Sd", "Number")
    for (i in modalites) {
      data2 <- data[data[, groupVar] == i, ]
      if (stats == "jump") {
        res <- compute_number_jumps(data2[, c("id", "time", "state")])
      } else {
        res <- compute_duration(data2[, c("id", "time", "state")])
      }
      dNew <- computeStatsDataFrame(res, addNumber = TRUE)
      d <- rbind.data.frame(d, dNew)
    }
    dNew <- computeStatsDataFrame(resStatsAll, addNumber = TRUE)
    d <- rbind.data.frame(d, dNew)
    row.names(d) <- c(modalites, "All")

    return(d)
  } else if (stats == "time") {
    d <- as.data.frame(matrix(ncol = 8, nrow = 0))
    for (i in modalites) {
      idToKeep <- unique(data[data[, groupVar] == i, "id"])
      time <- resStatsAll[names(resStatsAll[, nomState]) %in% idToKeep, nomState]
      dNew <- computeStatsDataFrame(time, addNumber = TRUE)
      d <- rbind.data.frame(d, dNew)
    }
    time <- resStatsAll[, nomState]
    dNew <- computeStatsDataFrame(time, addNumber = TRUE)
    d <- rbind.data.frame(d, dNew)
    row.names(d) <- c(modalites, "All")

    return(d)
  }
}

## Create table with the summary of statistics (jumps, duration, timeSpent) by cluster
tableOfStatsCluster <- function(data, resStatsAll, stats, class, nomState, nbClust) {
  if (stats %in% c("jump", "duration")) {
    d <- as.data.frame(matrix(ncol = 8, nrow = 0))
    colnames(d) <- c("Mean", "Median", "Q1", "Q3", "Min", "Max", "Sd", "Number")
    for (i in seq_len(nbClust)) {
      idToKeep <- names(class[class == i])
      data2 <- data[data$id %in% idToKeep, ]
      if (stats == "jump") {
        res <- compute_number_jumps(data2[, c("id", "time", "state")])
      } else {
        res <- compute_duration(data2[, c("id", "time", "state")])
      }
      dNew <- computeStatsDataFrame(res, addNumber = TRUE)
      d <- rbind.data.frame(d, dNew)
    }
    dNew <- computeStatsDataFrame(resStatsAll, addNumber = TRUE)
    d <- rbind.data.frame(d, dNew)
    row.names(d) <- c(paste("Cluster", 1:nbClust), "All")

    return(d)
  } else if (stats == "time") {
    d <- as.data.frame(matrix(ncol = 8, nrow = 0))
    for (i in seq_len(nbClust)) {
      idToKeep <- names(class[class == i])
      time <- resStatsAll[names(resStatsAll[, nomState]) %in% idToKeep, nomState]
      dNew <- computeStatsDataFrame(time, addNumber = TRUE)
      d <- rbind.data.frame(d, dNew)
    }
    time <- resStatsAll[, nomState]
    dNew <- computeStatsDataFrame(time, addNumber = TRUE)
    d <- rbind.data.frame(d, dNew)
    row.names(d) <- c(paste("Cluster", 1:nbClust), "All")

    return(d)
  }
}

# plot extreme individuals on factorial plan
plotExtreme <- function(data, min_pc, max_pc, dimNumber, col) {
  ids <- unique(data$id)
  group <- factor(rep(NA, length(ids)), levels = c("Lowest component values", "Highest component values"))
  group[ids %in% min_pc] <- "Lowest component values"
  group[ids %in% max_pc] <- "Highest component values"

  plotData(data, group = group, addBorder = FALSE, addId = FALSE, col = col) +
    labs(title = paste("Extreme individuals on component", dimNumber))
}
