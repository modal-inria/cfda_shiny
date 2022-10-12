## maximum number of modalities for a variable to be considered as a categorical group variable
## (for the plot and the table by group in descriptive statistics and estimation of markov chain parts)
MAXMOD <- 12


shinyServer(function(input, output, session) {

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

  ###################
  # 1. Import data ##
  ###################

  ## Define color for each state
  colorOfState <- reactive({
    etat <- sort(unique(data_used()$state))
    codeColor <- hue_pal()(length(etat))
    res <- paste(paste0('"', etat, '"'), "=", paste0('"', codeColor, '"'))
    vecColor <- paste("c(", toString(res), ")")
    vecColor
  })

  ## Import a data set
  data_import <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    data <- read.csv(file$datapath, header = TRUE, sep = input$sep, dec = input$dec)
  })

  ## Check if a file has been import
  output$filetest <- reactive({
    return(is.null(input$file1$datapath))
  })

  ## Create a output in UI for filetest
  outputOptions(output, "filetest", suspendWhenHidden = FALSE)

  ## filter data set
  data_used <- reactive({
    if ((sum(colnames(data_import()) %in% "id") != 1) | (sum(colnames(data_import()) %in% "time") != 1) | (
      sum(colnames(data_import()) %in% "state") != 1)) {
      validate("Error! data must have exactly one column named 'id', one column named 'state' and one column name 'time'")
    }
    validate(need(is.numeric(data_import()[, "time"]), "time must be numeric, please choose correct decimal symbol"))
    data <- data_import()
    input$applyMod
    isolate({
      ## Filter by length trajectories
      if (input$filterChoiceLength == "2") {
        minT <- data %>%
          group_by(id) %>%
          filter(time == min(time, na.rm = TRUE))
        maxT <- data %>%
          group_by(id) %>%
          filter(time == max(time, na.rm = TRUE))
        if ((input$upper == "") & (input$lower == "")) {
          idToKeep <- minT$id
        } else if ((input$upper == "") & !is.na(input$lower)) {
          tryCatch(
            {
              lower <- as.double(input$lower)
            },
            error = function(cond) {
              validate(need(FALSE, "Invalid time. Please check format."))
            }
          )
          idToKeep <- minT[minT$time <= lower, "id"]$id
        } else if (!is.na(input$upper) & (input$lower == "")) {
          tryCatch(
            {
              upper <- as.double(input$upper)
            },
            error = function(cond) {
              validate(need(FALSE, "Invalid time. Please check format."))
            }
          )
          idToKeep <- maxT[maxT$time >= upper, "id"]$id
        } else {
          tryCatch(
            {
              upper <- as.double(input$upper)
              lower <- as.double(input$lower)
            },
            error = function(cond) {
              validate(need(FALSE, "Invalid time. Please check format."))
            }
          )
          idToKeep <- intersect(minT[minT$time <= lower, "id"], maxT[maxT$time >= upper, "id"])$id
        }
        data <- data[data$id %in% idToKeep, ]
      }

      ## Filter by number of jumps
      if (input$filterChoiceJump == "2") {
        nJump <- nJump_import()
        if (is.na(input$more) & is.na(input$less)) {
          idToKeep <- unique(data$id)
        } else if (is.na(input$more) & !is.na(input$less)) {
          idToKeep <- names(nJump[nJump <= input$less])
        } else if (!is.na(input$more) & is.na(input$less)) {
          idToKeep <- names(nJump[nJump >= input$more])
        } else {
          idToKeep <- names(nJump[(nJump >= input$more) & (nJump <= input$less)])
        }
        data <- data[data$id %in% idToKeep, ]
      }

      ## Filter by percentage
      if (input$filterChoicePercentage == "2") {
        idInd <- sort(unique(data$id))
        set.seed(42)
        idToKeep <- sample(idInd, floor(length(idInd) * input$nb_ind / 100), replace = FALSE)
        data <- data[data$id %in% idToKeep, ]
      }
    })
    data
  })


  ## verify if a dataset exist
  output$fileUploaded <- reactive({
    return(!is.null(data_used()))
  })

  ## create a output in UI interface
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

  ## show data set
  output$head <- DT::renderDataTable(
    {
      data_used()
    },
    extensions = "Buttons",
    server = FALSE,
    options = list(
      dom = "Brtip",
      scrollX = TRUE,
      buttons = c("copy", "csv", "excel", "pdf"),
      rownames = FALSE
    )
  )

  ## filter widget for number of jumps
  output$jumpInt <- renderUI({
    tagList(
      numericInput("more", "Indiduals with more than ", min = 0, value = 0),
      numericInput("less", "and less than", min = 0, value = max(nJump_import()), max = max(nJump_import()))
    )
  })

  ## Number of jump of initial dataset
  nJump_import <- reactive({
    validate(need(
      is.numeric(data_import()[, "time"]),
      "time must be numeric, please choose correct decimal symbol"
    ))
    compute_number_jumps(data_import()[, c("id", "state", "time")], countDuplicated = FALSE)
  })

  ## Summary of data set
  output$Resume <- renderPrint({
    summary_cfd(data_used()[, c("id", "time", "state")])
  })


  ######################
  # 2. visualize data ##
  ######################

  ## List of the other variables of data set
  listGroupVar <- reactive({
    var <- which(!colnames(data_used()) %in% c("id", "time", "state"))
    choice <- colnames(data_used())[var]
    choice
  })

  ## Widget for the list of group variable (option "By group variable")
  output$groupVarVisualize <- renderUI({
    selectizeInput("groupVariableVisualize", "Select a group variable", choice = c(listGroupVar()))
  })

  ## Plot of trajectories
  plotOfData <- reactive({
    validate(need(is.numeric(data_used()[, "time"]), "time must be numeric, please choose correct decimal symbol"))

    ## Progress bar
    withProgress(
      message = "Making plot of individuals trajectories",
      detail = "Please wait until the end",
      value = 0,
      {
        incProgress(1 / 4)
        p <- plotData(
          data_used()[, c("id", "time", "state")],
          addId = FALSE,
          addBorder = FALSE,
          col = eval(parse(text = colorOfState()))
        ) + labs(title = "Trajectories of the Markov process")

        input$modPlotData
        isolate({
          if (input$choixParaGroupeVisualize == "All") {
            class <- NULL
          } else {
            validate(need(length(listGroupVar()) > 0, "this file doesn't have group variable"))
            checkGroupVariable(data_used(), input$groupVariableVisualize)

            r <- unique(data_used()[, c("id", input$groupVariableVisualize)])
            class <- r[, input$groupVariableVisualize]
          }
          p <- plotData(
            data_used()[, c("id", "time", "state")],
            group = class,
            addId = input$addId,
            addBorder = input$addBorder,
            sort = input$sort,
            col = eval(parse(text = colorOfState()))
          ) + labs(title = input$plotDataTitle)
        })
        for (i in 1:3) {
          incProgress(1 / 4)
          Sys.sleep(0.10)
        }
        p
      }
    )
  })


  ## plot of trajectories output
  output$traj <- renderPlot(
    {
      plotOfData()
    },
    height = 900
  )


  ## Download the plot of trajectories
  output$downloadPlotTraj <- downloadHandler(
    filename = function() {
      paste("plotData", "png", sep = ".")
    },
    content = function(file) {
      ggsave(file, plotOfData(), device = "png")
    }
  )

  #############################
  # 3. descriptive statistics #
  #############################

  ## Duration of trajectories for the filter data set (or the initial data set if user doesn't apply a filter)
  duration <- reactive({
    validate(need(is.numeric(data_used()[, "time"]), "time must be numeric, please choose correct decimal symbol"))
    duration <- compute_duration(data_used()[, c("id", "time", "state")])
  })

  ## time spent in each state for the filter data set (or the initial data set if user doesn't apply a filter)
  time_spent <- reactive({
    validate(need(
      is.numeric(data_used()[, "time"]),
      "time must be numeric, please choose correct decimal symbol"
    ))
    compute_time_spent(data_used()[, c("id", "state", "time")])
  })

  ## number of jumps for the filter data set (or the initial data set if user doesn't apply a filter)
  nJump <- reactive({
    validate(need(
      is.numeric(data_used()[, "time"]),
      "time must be numeric, please choose correct decimal symbol"
    ))
    compute_number_jumps(data_used()[, c("id", "time", "state")])
  })

  ## Widget for the list of group variable (option by group variable)
  output$groupVarStatistics <- renderUI({
    selectizeInput("groupVariableStatistics", "Select a group variable", choice = c(listGroupVar()))
  })

  ################################################
  ## Plots and table for descriptive statistics ##
  ################################################


  ##########
  ## All  ##
  ##########

  ## Summary of the filter data set
  output$summary <- renderPrint({
    summary_cfd(data_used()[, c("id", "time", "state")])
  })

  ## plots (duration, time spent, jumps)
  output$plots <- renderPlotly({
    withProgress(
      message = "Making plots",
      detail = "Please wait until the end",
      value = 0,
      {
        incProgress(1 / 4)
        if (input$choixGraphiqueStats == "jump") {
          jump_gp <- data.frame(jump = as.vector(nJump()))
          p <- ggplot(data.frame(jump_gp), aes(x = jump)) +
            labs(x = "Number of jump", y = "Frequency", title = "Distribution of number of jumps") +
            geom_bar(fill = "lightblue", color = "black")
        } else if (input$choixGraphiqueStats == "duration") {
          p <- hist(duration()) + labs(title = "Distribution of duration of trajectories")
        } else if (input$choixGraphiqueStats == "timeState") {
          p <- boxplot(time_spent()) +
            labs(title = "Distribution of time spent by state") +
            scale_fill_manual(values = eval(parse(text = colorOfState())))
        }
      }
    )
    p
  })

  ## Table with the summary of jumps and duration
  output$summaryStatsAll <- DT::renderDataTable(
    {
      if (input$choixGraphiqueStats == "duration") {
        tableOfStatsAll(duration(), "duration")
      } else if (input$choixGraphiqueStats == "jump") {
        tableOfStatsAll(nJump(), "jump")
      }
    },
    extensions = c("FixedColumns", "Buttons"),
    options = list(
      dom = "Btipr",
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 1),
      rownames = FALSE,
      buttons = c("copy", "csv", "excel", "pdf")
    )
  )

  ## Frequencies and proportions table of number of jumps
  output$nJumpTable <- DT::renderDataTable(
    {
      t <- table(as.vector(nJump()))
      name <- names(t)
      prop <- paste(round(prop.table(t), 4) * 100, "%")
      d <- rbind.data.frame(as.vector(t), prop)
      colnames(d) <- name
      row.names(d) <- c("Frequencies", "Proportions")
      d
    },
    extensions = c("FixedColumns", "Buttons"),
    server = FALSE,
    options = list(
      dom = "Btipr",
      scrollX = TRUE,
      buttons = c("copy", "csv", "excel", "pdf")
    )
  )

  ## Summary of time spent for each state
  output$timeSpentAllTable <- DT::renderDataTable(
    {
      tableOfStatsAll(time_spent(), "time")
    },
    server = FALSE,
    options = list(
      dom = "Btipr",
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 1)
    )
  )

  #######################
  ## By group variable ##
  #######################

  ## summary of data set by group (Create the outputs results)
  observe({
    req(input$groupVariableStatistics)
    checkGroupVariable(data_used(), input$groupVariableStatistics)
    mod <- unique(data_used()[, c(input$groupVariableStatistics)])
    checkNumberOfModality(length(mod))

    lapply(mod, function(par) {
      withProgress(
        message = "Making plots",
        detail = "Please wait until the end",
        value = 0,
        {
          incProgress(1 / 4)
          if (input$choixParaGroupeStatistics == "byGroup") {
            data <- data_used()[data_used()[, input$groupVariableStatistics] == par, ]
            if (input$choixGraphiqueStats == "summary") {
              output[[paste("summary", par, sep = "_")]] <- renderPrint({
                summary_cfd(data[, c("id", "state", "time")])
              })
            }
          }
        }
      )
    })
  })


  ## Create outputs for the summary of data set by group
  output$summaryGp <- renderUI({
    req(input$groupVariableStatistics)
      checkGroupVariable(data_used(), input$groupVariableStatistics)
    mod <- sort(unique(data_used()[, c(input$groupVariableStatistics)]))
    checkNumberOfModality(length(mod))

    summary_output_list <- lapply(mod, function(par) {
      plotname <- paste("summary", par, sep = "_")
      column(
        6,
        box(
          width = 12, title = paste("Group: ", par), status = "danger", solidHeader = FALSE,
          verbatimTextOutput(plotname)
        )
      )
    })
    do.call(tagList, summary_output_list)
  })

  ## Summary of number of jumps and duration by group
  output$summaryStatsByGroup <- DT::renderDataTable(
    {
      req(input$groupVariableStatistics)
      checkGroupVariable(data_used(), input$groupVariableStatistics)
      mod <- sort(unique(data_used()[, c(input$groupVariableStatistics)]))
      checkNumberOfModality(length(mod))

      if (input$choixGraphiqueStats == "duration") {
        tableOfStatsByGroup(data_used(), duration(), "duration", input$groupVariableStatistics, mod)
      } else if (input$choixGraphiqueStats == "jump") {
        tableOfStatsByGroup(data_used(), nJump(), "jump", input$groupVariableStatistics, mod)
      }
    },
    extensions = c("FixedColumns", "Buttons"),
    options = list(
      dom = "Btipr",
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 1),
      rownames = FALSE,
      buttons = c("csv", "excel", "pdf", "copy")
    )
  )

  ## Plots of duration by group
  output$durationGp <- renderPlotly({
    req(input$groupVariableStatistics)
    checkGroupVariable(data_used(), input$groupVariableStatistics)
    mod <- sort(unique(data_used()[, c(input$groupVariableStatistics)]))
    checkNumberOfModality(length(mod))

    d <- cbind.data.frame(duration = as.vector(duration()), id = names(duration()))
    dure_gp <- unique(merge(d, data_used()[, c("id", input$groupVariableStatistics)], by = "id"))
    g <- ggplot(data.frame(dure_gp), aes_string(x = "duration")) +
      labs(x = "Duration", y = "Frequency", title = paste("Duration of trajectories by", input$groupVariableStatistics)) +
      geom_histogram(
        fill = "lightblue",
        color = "black",
        bins = floor(1 + log2(length(duration())))
      ) +
      facet_wrap(input$groupVariableStatistics)
    g
  })


  ## Plots of number of jumps by group
  output$jumpGp <- renderPlotly({
    req(input$groupVariableStatistics)
    checkGroupVariable(data_used(), input$groupVariableStatistics)
    mod <- sort(unique(data_used()[, c(input$groupVariableStatistics)]))
    checkNumberOfModality(length(mod))

    d <- cbind.data.frame(jump = as.vector(nJump()), id = names(nJump()))
    jump_gp <- unique(merge(d, data_used()[, c("id", input$groupVariableStatistics)], by = "id"))
    g <- ggplot(data.frame(jump_gp), aes_string(x = "jump")) +
      labs(x = "Number of jump", y = "Frequency", title = paste("Number of jumps by", input$groupVariableStatistics)) +
      geom_bar(fill = "lightblue", color = "black") +
      facet_wrap(input$groupVariableStatistics)
    g
  })

  ## Frequencies table of number of jumps
  output$nJumpTableGroupFreq <- DT::renderDataTable(
    {
      req(input$groupVariableStatistics)
      checkGroupVariable(data_used(), input$groupVariableStatistics)
      mod <- unique(data_used()[, input$groupVariableStatistics])
      checkNumberOfModality(length(mod))

      jump <- data.frame(id = names(nJump()), jump = as.vector(nJump()))
      group <- unique(data_used()[, c("id", input$groupVariableStatistics)])
      jumpMerge <- merge(jump, group, by = "id")
      t <- as.data.frame.matrix(table(jumpMerge[, input$groupVariableStatistics], jumpMerge$jump))
      row_som <- apply(t, 1, sum)
      col_som <- apply(t, 2, sum)
      res <- rbind.data.frame(cbind.data.frame(t, total = row_som), total = c(col_som, sum(col_som)))

      res
    },
    extensions = c("FixedColumns", "Buttons"),
    server = FALSE,
    options = list(
      dom = "Btipr",
      scrollX = TRUE,
      fixedColumns = TRUE,
      buttons = c("copy", "csv", "excel", "pdf")
    )
  )

  ## proportion and profiles tables of number of jumps
  output$nJumpTableGroupTable <- DT::renderDataTable(
    {
      req(input$groupVariableStatistics)
      checkGroupVariable(data_used(), input$groupVariableStatistics)
      mod <- unique(data_used()[, input$groupVariableStatistics])
      checkNumberOfModality(length(mod))

      jump <- data.frame(id = names(nJump()), jump = as.vector(nJump()))
      group <- unique(data_used()[, c("id", input$groupVariableStatistics)])
      jumpMerge <- merge(jump, group, by = "id")
      t <- table(jumpMerge[, input$groupVariableStatistics], jumpMerge$jump)
      if (input$tableChoiceGroupDesc == "prop") {
        t <- as.data.frame.matrix(prop.table(t))
        row_som <- apply(t, 1, sum)
        col_som <- apply(t, 2, sum)
        res <- rbind.data.frame(cbind.data.frame(t, total = row_som), total = c(col_som, sum(col_som)))
        res <- round(res, 4) * 100
      } else if (input$tableChoiceGroupDesc == "row") {
        res <- as.data.frame.matrix(round(lprop(t), 2))
      } else {
        res <- as.data.frame.matrix(round(cprop(t), 2))
      }

      res
    },
    extensions = c("FixedColumns", "Buttons"),
    server = FALSE,
    options = list(
      dom = "Btipr",
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 1),
      buttons = c("copy", "csv", "excel", "pdf")
    )
  )

  ## Plots of time spent by group
  output$timeStateGp <- renderPlotly({
    req(input$groupVariableStatistics)
    checkGroupVariable(data_used(), input$groupVariableStatistics)
    mod <- sort(unique(data_used()[, c(input$groupVariableStatistics)]))
    checkNumberOfModality(length(mod))

    x <- time_spent()
    df <- data.frame(
      timeSpent = as.vector(x),
      state = factor(rep(colnames(x), each = nrow(x)), levels = colnames(x)),
      id = as.vector(rownames(x))
    )
    data <- unique(merge(df, data_used()[, c("id", input$groupVariableStatistics)], by = "id"))
    p <- ggplot(data, aes_string(x = "state", y = "timeSpent", fill = "state")) +
      geom_boxplot() +
      labs(
        x = "State", y = "Time Spent", fill = "State",
        title = paste("Time spent in each state by", input$groupVariableStatistics)
      ) +
      facet_wrap(input$groupVariableStatistics) +
      scale_fill_manual(values = eval(parse(text = colorOfState())))
    p
  })

  ## summary of Time spent by state by group (create outputs)
  observe({
    req(input$groupVariableStatistics)
    data <- data_used()

    checkGroupVariable(data, input$groupVariableStatistics)
    mod <- unique(data[, "state"])
    checkNumberOfModality(length(unique(data[, input$groupVariableStatistics])))

    timeSpent <- time_spent()
    gp <- unique(data[, input$groupVariableStatistics])
    lapply(mod, function(par) {
      output[[paste("timeSpentGroup", par, sep = "_")]] <-
        DT::renderDataTable({
            tableOfStatsByGroup(data, time_spent(), "time", input$groupVariableStatistics, gp, par)
          },
          extensions = c("FixedColumns", "Buttons"),
          server = FALSE,
          options = list(
            dom = "Btipr",
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 1),
            lengthMenu = c(2, 6, 12),
            pageLength = 2,
            buttons = c("copy", "csv", "excel", "pdf")
          )
        )
    })
  })

  ## create widget for the summary of time spent in each state by group
  output$timeSpentTableGp <- renderUI({
    req(input$groupVariableStatistics)
    checkGroupVariable(data_used(), input$groupVariableStatistics)
    mod <- unique(data_used()[, input$groupVariableStatistics])
    checkNumberOfModality(length(mod))

    mod <- colnames(time_spent())
    summary_output_list <- lapply(mod, function(par) {
      plotname <- paste("timeSpentGroup", par, sep = "_")
      column(
        12,
        box(
          width = 12, title = paste("State:", par), status = "danger", solidHeader = TRUE,
          DTOutput(plotname)
        )
      )
    })
    do.call(tagList, summary_output_list)
  })

  ##############################
  ## 4. estimate markov chain ##
  ##############################


  ## widget with the other variables of data set (option by group variable)
  output$groupVarMarkov <- renderUI({
    selectizeInput("groupVariableMarkov", "Select a group variable", choice = c(listGroupVar()))
  })

  ##########
  ## All  ##
  ##########

  ## Compute the estimation of the Markov chain
  estimateMarkovAll <- reactive({
    validate(
      need(
        is.numeric(data_used()[, "time"]),
        "time must be numeric, please choose correct decimal symbol"
      )
    )
    withProgress(
      message = "Making transition graph",
      detail = "Please wait until the end",
      value = 0,
      {
        p <- estimate_Markov(data_used()[, c("id", "time", "state")])
        for (i in 1:4) {
          incProgress(1 / 4)
          Sys.sleep(0.10)
        }
      }
    )
    p
  })

  ## Transition graph plot
  output$transGraphAll <- renderPlot(
    {
      r <- eval(parse(text = colorOfState()))
      r <- r[names(r) %in% colnames(estimateMarkovAll()$P)]
      plot(estimateMarkovAll(), box.col = r)
    },
    height = 900
  )

  ## transition matrix
  output$transMatAll <- renderPrint({
    round(estimateMarkovAll()$P, 3)
  })

  ## number of jumps state
  output$njumpMarkovAll <- renderPrint({
    statetable(data_used()[, c("id", "state", "time")])
  })

  ## probability to be in a state plot
  output$probaStateAll <- renderPlotly({
    plot(estimate_pt(data_used()[, c("id", "state", "time")]))
  })

  ## exponential law parameter of sojourn time
  output$expoLawMarkovAll <- renderPrint({
    round(estimateMarkovAll()$lambda, 3)
  })

  #######################
  ## by group variable ##
  #######################

  ## Create outputs of markov chain, transition matrix, exponential law, ect
  observe({
    req(input$groupVariableMarkov)
    checkGroupVariable(data_used(), input$groupVariableMarkov)
    mod <- unique(data_used()[, c(input$groupVariableMarkov)])
    checkNumberOfModality(length(mod))

    lapply(mod, function(par) {
      withProgress(
        message = "Making plots",
        detail = "Please wait until the end",
        value = 0,
        {
          incProgress(1 / 4)
          if (input$choixParaGroupeMarkov == "byGroup") {
            data <- data_used()[data_used()[, input$groupVariableMarkov] == par, ]
            mark <- estimate_Markov(data[, c("id", "state", "time")])
            proba <- estimate_pt(data[, c("id", "state", "time")])
            output[[paste("graphTransition", par, sep = "_")]] <-
              renderPlot({
                r <- eval(parse(text = colorOfState()))
                r <- r[names(r) %in% colnames(mark$P)]
                plot(mark, box.col = r)
              })
            output[[paste("matTransition", par, sep = "_")]] <-
              renderPrint({
                round(mark$P, 3)
              })
            output[[paste("nJumpMat", par, sep = "_")]] <-
              renderPrint({
                round(statetable(data), 3)
              })

            output[[paste("expoLaw", par, sep = "_")]] <-
              renderPrint({
                round(mark$lambda, 3)
              })
          }
        }
      )
    })
  })

  ## Transition graph by group
  output$transGraphByGroup <- renderUI({
    req(input$groupVariableMarkov)
    checkGroupVariable(data_used(), input$groupVariableMarkov)
    mod <- sort(unique(data_used()[, c(input$groupVariableMarkov)]))
    checkNumberOfModality(length(mod))

    plot_output_list <- lapply(mod, function(par) {
      plotname <- paste("graphTransition", par, sep = "_")
      nInd <- unique(data_used()[data_used()[, input$groupVariableMarkov] == par, c("id", input$groupVariableMarkov)])
      column(6, box(
        width = 12, title = paste("Group: ", par, "(n:", nrow(nInd), ")"),
        status = "danger", solidHeader = TRUE,
        shinycssloaders::withSpinner(
          plotOutput(plotname),
          type = getOption("spinner.type", default = 6),
          color = getOption("spinner.color", default = "#d73925")
        )
      ))
    })
    do.call(tagList, plot_output_list)
  })

  ## Transition matrix by group
  output$transMatByGroup <- renderUI({
    req(input$groupVariableMarkov)
    checkGroupVariable(data_used(), input$groupVariableMarkov)
    mod <- sort(unique(data_used()[, c(input$groupVariableMarkov)]))
    checkNumberOfModality(length(mod))

    plot_output_list <- lapply(mod, function(par) {
      plotname <- paste("matTransition", par, sep = "_")
      nInd <- unique(data_used()[data_used()[, input$groupVariableMarkov] == par, c("id", input$groupVariableMarkov)])
      column(6, box(
        width = 12, title = paste("Group: ", par, "(n:", nrow(nInd), ")"),
        status = "danger", solidHeader = TRUE,
        verbatimTextOutput(plotname)
      ))
    })
    do.call(tagList, plot_output_list)
  })


  ## Number jump state by group
  output$njumpMarkovByGroup <- renderUI({
    req(input$groupVariableMarkov)
    checkGroupVariable(data_used(), input$groupVariableMarkov)
    mod <- sort(unique(data_used()[, c(input$groupVariableMarkov)]))
    checkNumberOfModality(length(mod))

    plot_output_list <- lapply(mod, function(par) {
      plotname <- paste("nJumpMat", par, sep = "_")
      nInd <- nrow(unique((data_used()[data_used()[, input$groupVariableMarkov] == par, c("id", input$groupVariableMarkov)])))
      column(6, box(
        width = 12, title = paste("Group: ", par, "(n:", nInd, ")"),
        status = "danger", solidHeader = TRUE,
        verbatimTextOutput(plotname)
      ))
    })
    do.call(tagList, plot_output_list)
  })

  ## probability state to be in a state by group
  output$probaStateByGroup <- renderPlotly({
    req(input$groupVariableMarkov)
    checkGroupVariable(data_used(), input$groupVariableMarkov)
    mod <- sort(unique(data_used()[, c(input$groupVariableMarkov)]))
    checkNumberOfModality(length(mod))

    d <- data.frame(matrix(ncol = 4, nrow = 0))
    for (par in mod) {
      pt <- estimate_pt(data_used()[data_used()[, input$groupVariableMarkov] == par, c("id", "time", "state")])
      d <- rbind.data.frame(
        d,
        data.frame(
          factor(rep(rownames(pt$pt), each = ncol(pt$pt)), levels = rownames(pt$pt)),
          as.vector(t(pt$pt)),
          rep(pt$t, nrow(pt$pt)),
          par
        )
      )
    }

    colnames(d) <- c("State", "proba", "time", "groupe")
    p <- ggplot(d, aes_string(x = "time", y = "proba", group = "State", colour = "State")) +
      geom_line() +
      ylim(0, 1) +
      labs(x = "Time", y = "p(t)", title = "P(X(t) = x)") +
      facet_wrap("groupe") +
      scale_colour_manual(values = eval(parse(text = colorOfState())))
    p
  })

  ## exponential law of sojourn time by group
  output$expoLawMarkovByGroup <- renderUI({
    req(input$groupVariableMarkov)
    checkGroupVariable(data_used(), input$groupVariableMarkov)

    mod <- sort(unique(data_used()[, c(input$groupVariableMarkov)]))
    plot_output_list <- lapply(mod, function(par) {
      plotname <- paste("expoLaw", par, sep = "_")
      nInd <- nrow(unique((data_used()[data_used()[, input$groupVariableMarkov] == par, c("id", input$groupVariableMarkov)])))
      column(6, box(
        width = 12, title = paste("Group: ", par, "(n:", nInd, ")"),
        status = "danger", solidHeader = TRUE,
        verbatimTextOutput(plotname)
      ))
    })
    do.call(tagList, plot_output_list)
  })


  ###########################
  ## 5.1 Factorial analysis #
  ###########################

  ## Check if factorial analysis is computed
  output$fmcaUploaded <- reactive({
    return(!is.null(fmca()))
  })

  ## Title of the factorial parameter box
  output$titleBoxFactorial <- renderUI({
    h4(paste0("Path observed on [", min(data_used()$time), ";T]"))
  })

  ## widget with the list of the state to be extended
  output$uiAbsorbedState <- renderUI({
    state <- unique(data_used()[, "state"])
    multiInput(
      inputId = "absorbedState",
      label = "State(s) to be extended",
      choices = NULL,
      choiceNames = state,
      choiceValues = state
    )
  })

  outputOptions(output, "fmcaUploaded", suspendWhenHidden = FALSE)

  ## Color of a each state (included the non observed state)
  color_data_CFDA <- reactive({
    stateCFDA <- sort(unique(data_CFDA()$state))
    stateOrigin <- sort(unique(data_used()$state))
    if (length(stateCFDA) > length(stateOrigin)) {
      nonObState <- stateCFDA[!(stateCFDA %in% stateOrigin)]
      color <- substr(colorOfState(), 3, nchar(colorOfState()) - 1)
      r <- eval(parse(text = paste0("c(", color, ",", '"', nonObState, '"', "=", '"', "#565656", '")')))
      r[order(names(r))]
    } else {
      colorOfState()
      r <- eval(parse(text = colorOfState()))
    }
  })

  ## Cut data used for analysis
  data_CFDA <- reactive({
    input$soumettre
    isolate({
      tmax <- as.double(input$tpsmax)
      resume <- summary_cfd(data_used()[, c("id", "state", "time")])
      minT <- resume$timeRange[1]
      maxT <- resume$timeRange[2]
      validate(
        need(resume$uniqueStart, "All individuals must have the same time start value"),
        need(tmax >= minT, paste("End time must be greater than", minT))
      )
      d <- cut_data(
        data_used()[, c("id", "time", "state")],
        Tmax = tmax,
        prolongLastState = input$absorbedState,
        NAstate = input$nameNAstate
      )
      d
    })
  })

  ## Plot of trajectories of the cut data
  output$plotDataCFDA <- renderPlot({
    plotData(data_CFDA(), addId = FALSE, addBorder = FALSE, col = color_data_CFDA())
  })

  ## Summary of the cut data
  output$summaryCFDA <- renderPrint({
    summary_cfd(data_CFDA())
  })

  ## factorial analysis results
  fmca <- reactive({
    input$soumettre
    isolate({
      minTim <- min(data_CFDA()[, "time"])
      validate(
        need(
          is.numeric(data_used()[, "time"]),
          "Time must be numeric, please choose correct decimal separator"
        ),
        need(!is.na(as.double(input$tpsmax)), "end time T invalid "),
      )
      withProgress(
        message = "Computing principal component",
        detail = "Please wait until the end",
        value = 0,
        {
          incProgress(2 / 15)
          Sys.sleep(0.25)
          set.seed(42)
          tmax <- as.double(input$tpsmax)
          resume <- summary_cfd(data_used()[, c("id", "state", "time")])
          minT <- resume$timeRange[1]
          maxT <- resume$timeRange[2]
          validate(
            need(resume$uniqueStart, "All individuals must have the same time start value"),
            need(
              summary_cfd(data_CFDA())$nInd > 1,
              "There is only one row or less with this end time please change the value"
            ),
            need(tmax >= minT, paste("End time must be greater than", minT))
          )
          if (input$typeBasis == "spline") {
            basis <- create.bspline.basis(c(min(data_CFDA()[, "time"]), tmax),
              nbasis = input$nbasis,
              norder = input$norder
            )
          } else {
            basis <- create.fourier.basis(c(min(data_CFDA()[, "time"]), tmax), nbasis = input$nbasis)
          }
          fmca <- compute_optimal_encoding(data_CFDA(), basis)
          for (i in 3:15) {
            incProgress(1 / 15)
            Sys.sleep(0.25)
          }
          fmca
        }
      )
    })
  })

  ## Eigen value table
  eigenvalues <- reactive({
    vpselection <- which(round(fmca()$eigenvalues, 4) > 0)
    vp <- cbind.data.frame(
      "eigenvalues" = round(fmca()$eigenvalues[vpselection], 4),
      "Percentage of variance" = round(fmca()$eigenvalues[vpselection] / sum(fmca()$eigenvalues[vpselection]) * 100, 2),
      "Cumulative percentage of variance" = round(
        cumsum(fmca()$eigenvalues[vpselection]) / sum(fmca()$eigenvalues[vpselection]) * 100,
        2
      )
    )
    rownames(vp) <- paste("Dim", seq_len(length(fmca()$eigenvalues[vpselection])))
    vp
  })

  ## Show eigenvalues table
  output$eigenvaluesTable <- renderPrint({
    eigenvalues()
  })

  ## Plots of the eigen values
  output$valeurspropres <- renderPlotly({
    tmax <- as.double(input$tpsmax)
    min <- summary_cfd(data_used()[, c("id", "state", "time")])$timeRange[1]
    validate(
      need(
        summary_cfd(data_CFDA())$nInd > 1,
        "There is only one row or less with this end time please change the value"
      ),
      need(
        tmax >= min,
        paste("end time must be between greather than", min)
      )
    )
    if (input$cumulative) {
      g <-
        ggplot(cbind.data.frame(x = seq_len(nrow(eigenvalues())), y = eigenvalues()[, 3])) +
        ggtitle("Cumulative eigenvalues plot") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_continuous(breaks = seq_len(nrow(eigenvalues()))) +
        geom_col(fill = "blue") +
        aes(x = x, y = y) +
        xlab("Component") +
        ylab("Percentage of variance")
    } else {
      g <-
        ggplot(cbind.data.frame(x = seq_len(nrow(eigenvalues())), y = eigenvalues()[, 2])) +
        ggtitle("Eigenvalues plot") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_continuous(breaks = seq_len(nrow(eigenvalues()))) +
        geom_col(fill = "blue") +
        aes(x = x, y = y) +
        xlab("Component") +
        ylab("Percentage of variance")
    }
    g
  })

  ## Widget to choose dimension on Factorial plan part
  output$dim1 <- renderUI({
    input$soumettre
    maxi <- isolate(input$nbasis * length(summary_cfd(data_CFDA()[, c("id", "state", "time")])$states))
    selectInput(
      inputId = "choix_dim1",
      label = "Axis 1",
      selected = 1,
      choices = seq(1, maxi, 1),
      multiple = FALSE
    )
  })

  ## Widget to choose dimension on Factorial plan part
  output$dim2 <- renderUI({
    input$soumettre
    maxi <- isolate(input$nbasis * length(summary_cfd(data_CFDA()[, c("id", "state", "time")])$states))
    selectInput(
      inputId = "choix_dim2",
      label = "Axis 2 ",
      selected = 2,
      choices = seq(1, maxi, 1),
      multiple = FALSE
    )
  })

  ## Factorial plan plot
  output$planfact <- renderPlotly({
    req(input$choix_dim1, input$choix_dim2)
    if (input$groupVariableFactorialPlan == "NONE") {
      plotComponent(
        fmca(),
        comp = c(as.numeric(input$choix_dim1), as.numeric(input$choix_dim2)),
        addNames = FALSE
      )
    } else {
      group <- unique(data_used()[data_used()$id %in% row.names(fmca()$pc), c("id", input$groupVariableFactorialPlan)])
      validate(need(nrow(group) == nrow(fmca()$pc), "can't be used as group variable"))
      p <- plotComponent(fmca(), comp = c(as.numeric(input$choix_dim1), as.numeric(input$choix_dim2)), addNames = FALSE)
      if ((is.numeric(group[, input$groupVariableFactorialPlan]) | (is.integer(group[, input$groupVariableFactorialPlan]))
        ) & length(unique(group[, input$groupVariableFactorialPlan])) > MAXMOD) {
        p <- p + geom_point(aes(color = group[, input$groupVariableFactorialPlan])) +
          labs(color = input$groupVariableFactorialPlan)
      } else {
        p <- p + geom_point(aes(color = as.factor(group[, input$groupVariableFactorialPlan]))) +
          labs(color = input$groupVariableFactorialPlan)
      }
      p
    }
  })

  ## List of other variable (option group variable)
  output$groupVarFactorialPlan <- renderUI({
    selectizeInput(
      "groupVariableFactorialPlan",
      "Select a group variable",
      choice = c("NONE", listGroupVar())
    )
  })

  ## Optimal encoding plot (dimension 1 selected)
  optimalEncodingPlot1 <- reactive({
    req(input$choix_dim1)
    if (input$addCI) {
      plot(fmca(), harm = as.numeric(input$choix_dim1), addCI = TRUE) +
        ylab("a_x(t)") +
        scale_fill_manual(values = color_data_CFDA())
    } else {
      plot(fmca(), harm = as.numeric(input$choix_dim1), addCI = FALSE) +
        ylab("a_x(t)") +
        scale_color_manual(values = color_data_CFDA())
    }
  })

  ## plot of optimal encoding plot (dimension 1 selected)
  output$optimalEncoding1 <- renderPlotly({
    optimalEncodingPlot1()
  })

  ## Optimal encoding plot (dimension 2 selected)
  optimalEncodingPlot2 <- reactive({
    req(input$choix_dim2)
    if (input$addCI) {
      plot(fmca(), harm = as.numeric(input$choix_dim2), addCI = TRUE) +
        ylab("a_x(t)") +
        scale_fill_manual(values = color_data_CFDA())
    } else {
      plot(fmca(), harm = as.numeric(input$choix_dim2), addCI = FALSE) +
        ylab("a_x(t)") +
        scale_color_manual(values = color_data_CFDA())
    }
  })

  ## plot of optimal encoding plot (dimension 1 selected)
  output$optimalEncoding2 <- renderPlotly({
    optimalEncodingPlot2()
  })

  ########################
  ## Extreme individuals #
  ########################

  ## Widget to choose dimension for extreme individuals
  output$dim1Extrem <- renderUI({
    input$soumettre
    maxi <- isolate(input$nbasis * length(summary_cfd(data_CFDA()[, c("id", "state", "time")])$states))
    selectInput(
      inputId = "choix_dim1Extrem",
      label = "Axis 1",
      selected = 1,
      choices = seq(1, maxi, 1),
      multiple = FALSE
    )
  })

  ## Widget to choose dimension for extreme individuals
  output$dim2Extrem <- renderUI({
    input$soumettre
    maxi <- isolate(input$nbasis * length(summary_cfd(data_CFDA()[, c("id", "state", "time")])$states))
    selectInput(
      inputId = "choix_dim2Extrem",
      label = "Axis 2 ",
      selected = 2,
      choices = seq(1, maxi, 1),
      multiple = FALSE
    )
  })

  ## Widget to choose the percentage of extreme individuals
  output$extremComp1ui <- renderUI({
    sliderInput(
      "extremComp1",
      paste("Extreme individuals on dimension", input$choix_dim1Extrem),
      min = 0,
      max = 100,
      value = 10
    )
  })

  ## Widget to choose the percentage of extreme individuals
  output$extremComp2ui <- renderUI({
    sliderInput(
      "extremComp2",
      paste("Extreme individuals on dimension", input$choix_dim2Extrem),
      min = 0,
      max = 100,
      value = 10
    )
  })

  ## Title of box for extreme individuals
  output$axe1 <- renderUI({
    req(input$choix_dim1Extrem)
    h4(paste("Dimension", input$choix_dim1Extrem))
  })

  ## Title of box for extreme individuals
  output$axe2 <- renderUI({
    req(input$choix_dim2Extrem)
    h4(paste("Dimension", input$choix_dim2Extrem))
  })

  ## Selection of extreme individuals
  extremIndividuals <- reactive({
    minpc1 <- NULL
    minpc2 <- NULL
    maxpc1 <- NULL
    maxpc2 <- NULL
    n <- nrow(fmca()$pc)

    ## Extreme on dim 1
    dim1SortIncre <- sort(fmca()$pc[, as.numeric(input$choix_dim1Extrem)])
    dim1SortDecre <- sort(dim1SortIncre, decreasing = TRUE)
    if (input$extremComp1 > 0) {
      minpc1 <- names(dim1SortIncre[1:ceiling(n * input$extremComp1 / 100 / 2)])
      maxpc1 <- names(dim1SortDecre[1:ceiling(n * input$extremComp1 / 100 / 2)])
    }

    ## Extreme on dim 2
    dim2SortIncre <- sort(fmca()$pc[, as.numeric(input$choix_dim2Extrem)])
    dim2SortDecre <- sort(dim2SortIncre, decreasing = TRUE)
    if (input$extremComp2 > 0) {
      minpc2 <- names(dim2SortIncre[1:ceiling(n * input$extremComp2 / 100 / 2)])
      maxpc2 <- names(dim2SortDecre[1:ceiling(n * input$extremComp2 / 100 / 2)])
    }

    list(minpc1 = minpc1, maxpc1 = maxpc1, minpc2 = minpc2, maxpc2 = maxpc2)
  })

  ## Plot of extreme individuals on factorial plan
  output$planfactExtremeIndividuals <- renderPlotly({
    req(input$choix_dim2Extrem, input$choix_dim1Extrem)
    ids <- unique(data_CFDA()$id)
    group <- factor(
      rep("Not extreme", length(ids)),
      levels = c("Extreme on axis 2", "Extreme on axis 1", "Not extreme", "Extreme on both axis")
    )
    group[ids %in% extremIndividuals()$minpc1] <- "Extreme on axis 1"
    group[ids %in% extremIndividuals()$maxpc1] <- "Extreme on axis 1"
    group[ids %in% extremIndividuals()$minpc2] <- "Extreme on axis 2"
    group[ids %in% extremIndividuals()$maxpc2] <- "Extreme on axis 2"
    group[ids %in% intersect(extremIndividuals()$minpc1, extremIndividuals()$minpc2)] <- "Extreme on both axis"
    group[ids %in% intersect(extremIndividuals()$maxpc1, extremIndividuals()$maxpc2)] <- "Extreme on both axis"
    p <- plotComponent(fmca(),
      comp = c(
         as.numeric(input$choix_dim1Extrem),
         as.numeric(input$choix_dim2Extrem)
       ),
      addNames = FALSE
    ) +
      geom_point(aes(color = group)) +
      scale_color_manual(values = c("#000CFF", "#FF0000", "#000000", "#7000FF"))
    p
  })


  ## plot of trajectories of extreme individuals 1
  plotDataExtremAxe1 <- reactive({
    validate(need(input$extremComp1 > 0, "No extreme individuals selected"))
    plotExtreme(data_CFDA(), extremIndividuals()$minpc1, extremIndividuals()$maxpc1, input$choix_dim1, color_data_CFDA())
  })

  ## plot of trajectories of extreme individuals 1
  output$plotDataExtremAxe1 <- renderPlot({
    validate(need(input$extremComp1 > 0, "No extreme individuals selected"))
    plotDataExtremAxe1()
  })

  ## plot of trajectories of extreme individuals 2
  plotDataExtremAxe2 <- reactive({
    validate(need(input$choix_dim2Extrem > 0, "No extreme individuals selected"))
    plotExtreme(data_CFDA(), extremIndividuals()$minpc2, extremIndividuals()$maxpc2, input$choix_dim2, color_data_CFDA())
  })

  ## plot of trajectories of extreme individuals 2
  output$plotDataExtremAxe2 <- renderPlot({
    validate(need(input$choix_dim2Extrem > 0, "No extrem individuals selected"))
    plotDataExtremAxe2()
  })


  ## Download plot of trajectories
  output$downloadPlotDataExtremAxe1 <- downloadHandler(
    filename = function() {
      paste("extremeIndividualsAxe1", "png", sep = ".")
    },
    content = function(file) {
      ggsave(file, dendrogramme(), device = "png")
    }
  )

  ## Download plot of trajectories
  output$downloadPlotDataExtremAxe2 <- downloadHandler(
    filename = function() {
      paste("extremeIndividualsAxe2", "png", sep = ".")
    },
    content = function(file) {
      ggsave(file, dendrogramme(), device = "png")
    }
  )

  ## cut data with the potential other variable of initial data set
  data_used_CFDA <- reactive({
    if (length(listGroupVar()) == 0) {
      data_CFDA()
    } else {
      unique(merge(data_CFDA(), data_used()[, c("id", listGroupVar())], by = "id"))
    }
  })

  ## Download factorial analysis results
  output$downloadResultsCFDA <- downloadHandler(
    filename <- function() {
      paste("resFactorialAnalysis.RData")
    },
    content = function(file) {
      results_factorial_analysis <- list(dataUsed = data_used_CFDA(), optimal_encoding = fmca())
      save(results_factorial_analysis, file = file)
    }
  )

  ###################
  ## 5.2.Clustering #
  ###################

  ## stats by cluster
  time_spent_data_CFDA <- reactive({
    compute_time_spent(data_CFDA())
  })

  nJump_data_CFDA <- reactive({
    compute_number_jumps(data_CFDA())
  })

  output$nJumpGraphByCluster <- renderPlotly({
    jump_gp <- cbind.data.frame(
       jump = as.factor(as.vector(nJump_data_CFDA())),
       cluster = class()
    )
    g <- ggplot(data.frame(jump_gp), aes(x = jump)) +
      labs(x = "Number of jump", y = "Frequency") +
      geom_bar(fill = "lightblue", color = "black") +
      facet_wrap("cluster")
    g
  })

  ## size of clusters
  output$effecCluster <- renderPrint({
    req(input$nbclust)
    d <- as.data.frame(table(class()))
    prop <- round(prop.table(table(class())), 4) * 100
    d <- cbind.data.frame(d, prop = as.vector(prop))
    row.names(d) <- paste("cluster", 1:input$nbclust)
    d[, 2:3]
  })

  ## widget to choose the number of component for clustering
  output$nb_comp <- renderUI({
    input$soumettre
    maxi <- isolate(input$nbasis * length(summary_cfd(data_CFDA()[, c("id", "state", "time")])$states))
    selectInput(
      inputId = "nbcomp",
      label = "Number of components for clustering",
      selected = 1,
      choices = seq(1, maxi, 1),
      multiple = FALSE
    )
  })

  ## hierarchical clustering
  hc <- reactive({
    input$clusteringSubmit
    isolate({
      if (input$compsCAH == 1) {
        ncomp <- as.numeric(input$nbcomp)
        hclust(dist(fmca()$pc[, 1:ncomp]), method = input$method)
      } else {
        if (input$percentageVariance == 100) {
          hclust(dist(fmca()$pc), method = input$method)
        } else {
          ncomp <- which(cumsum(prop.table(fmca()$eigenvalues)) >= input$percentageVariance / 100)[1]
          hclust(dist(fmca()$pc[, 1:ncomp]), method = input$method)
        }
      }
    })
  })

  dendrogramme <- reactive({
    plot(hc(), labels = FALSE)
    if (is.null(input$nbclust)) {
      rect.hclust(hc(), 2, border = "green3")
    } else {
      if (input$couper) {
        rect.hclust(hc(), input$nbclust, border = "green3")
      }
    }
  })

  ## Plot of dendrogram
  output$dendrogramme <- renderPlot({
    dendrogramme()
  })

  ## Maximum number of cluster
  max_cluster <- reactive({
    max <- length(unique(data_CFDA()[, "id"])) - 1
  })

  ## widget to choose the number of clusters
  output$clus <- renderUI({
    numericInput("nbclust", "Number of clusters", min = 2, max = max_cluster(), value = 2)
  })

  ## vector the the clusters
  class <- reactive({
    req(input$nbclust)
    validate(
      need(
        input$nbclust <= max_cluster() & input$nbclust >= 2,
        paste("the number of clusters must be between", 2, "and", max_cluster())
      )
    )
    class <- cutree(hc(), k = input$nbclust)
  })

  ## plot trajectories by cluster
  groupe <- reactive({
    plotData(
      data_CFDA(),
      group = class(),
      addId = FALSE,
      addBorder = FALSE,
      col = color_data_CFDA(),
      sort = TRUE
    )
  })

  output$groupe <- renderPlot({
    groupe()
  })

  ## Download the dendrogram
  output$downloadDendogram <- downloadHandler(
    filename = function() {
      paste("dendogram", "png", sep = ".")
    },
    content = function(file) {
      ggsave(file, dendrogramme(), device = "png")
    }
  )

  ## Download the plot data
  output$downloadGroupPlot <- downloadHandler(
    filename = function() {
      paste("groupPlot", "png", sep = ".")
    },
    content = function(file) {
      ggsave(file, groupe(), device = "png")
    }
  )

  ## plots of time spent in each state by cluster (1 plots by clusters)
  output$timeStateGraphByCluster <- renderPlotly({
    x <- time_spent_data_CFDA()
    df <- data.frame(
      timeSpent = as.vector(x),
      state = factor(rep(colnames(x), each = nrow(x)), levels = colnames(x)),
      id = as.vector(rownames(x))
    )
    d <- cbind.data.frame(id = names(class()), cluster = class())
    data <- unique(merge(df, d, by = "id"))
    p <- ggplot(data, aes_string(x = "state", y = "timeSpent", fill = "state")) +
      geom_boxplot() +
      labs(x = "State", y = "Time Spent", fill = "State") +
      facet_wrap("cluster") +
      scale_fill_manual(values = color_data_CFDA())
    p
  })

  ## Summary of number of jumps by cluster
  output$SummaryJumpByCluster <- DT::renderDataTable(
    {
      tableOfStatsCluster(
        data = data_CFDA(),
        resStatsAll = nJump_data_CFDA(),
        stats = "jump",
        class = class(),
        nbClust = input$nbclust
      )
    },
    extensions = c("FixedColumns", "Buttons"),
    server = FALSE,
    options = list(
      dom = "Btipr",
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 1),
      buttons = c("copy", "csv", "excel", "pdf")
    )
  )

  ## frequencies table of number of jumps by clusters
  output$freqJumpByCluster <- DT::renderDataTable(
    {
      jump <- data.frame(id = names(nJump_data_CFDA()), jump = as.vector(nJump_data_CFDA()))
      group <- cbind.data.frame(id = names(class()), group = as.vector(class()))
      jumpMerge <- merge(jump, group, by = "id")
      t <- table(jumpMerge$group, jumpMerge$jump)
      t <- as.data.frame.matrix(t)
      row_som <- apply(t, 1, sum)
      col_som <- apply(t, 2, sum)
      res <- rbind.data.frame(cbind.data.frame(t, total = row_som), total = c(col_som, sum(col_som)))
      row.names(res) <- c(paste("cluster", 1:input$nbclust), "total")
      res
    },
    extensions = c("FixedColumns", "Buttons"),
    server = FALSE,
    options = list(
      dom = "Btipr",
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 1),
      buttons = c("copy", "csv", "excel", "pdf")
    )
  )

  ## proportions and profiles table by clusters
  output$tableJumpByCluster <- DT::renderDataTable(
    {
      jump <- data.frame(id = names(nJump_data_CFDA()), jump = as.vector(nJump_data_CFDA()))
      group <- cbind.data.frame(id = names(class()), group = as.vector(class()))
      jumpMerge <- merge(jump, group, by = "id")
      t <- table(jumpMerge$group, jumpMerge$jump)
      if (input$tableChoiceCluster == "prop") {
        t <- as.data.frame.matrix(prop.table(t))
        row_som <- apply(t, 1, sum)
        col_som <- apply(t, 2, sum)
        res <- rbind.data.frame(cbind.data.frame(t, total = row_som), total = c(col_som, sum(col_som)))
        res <- round(res, 4) * 100
        row.names(res) <- c(paste("cluster", 1:input$nbclust), "total")
      } else if (input$tableChoiceCluster == "row") {
        res <- as.data.frame.matrix(round(lprop(t), 2))
        row.names(res) <- c(paste("cluster", 1:input$nbclust), "Ensemble")
      } else {
        res <- as.data.frame.matrix(round(cprop(t), 2))
        row.names(res) <- c(paste("cluster", 1:input$nbclust), "total")
      }
      res
    },
    extensions = c("FixedColumns", "Buttons"),
    server = FALSE,
    options = list(
      dom = "Btipr",
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 1),
      buttons = c("copy", "csv", "excel", "pdf")
    )
  )

  ## Create output object for data table of the time spent in each state by clusters
  observe({
    req(input$nbclust)
    timeSpent <- time_spent_data_CFDA()
    mod <- colnames(timeSpent)
    lapply(mod, function(par) {
      tableOfStatsCluster(data_CFDA(), timeSpent, "time", class(), par, input$nbclust)
      plotname <- paste("timeSpentCluster", par, sep = "_")

      output[[paste("timeSpentCluster", par, sep = "_")]] <-
        DT::renderDataTable(
          {
            tableOfStatsCluster(data_CFDA(), timeSpent, "time", class(), par, input$nbclust)
          },
          extensions = c("FixedColumns", "Buttons"),
          server = FALSE,
          options = list(
            dom = "Btipr",
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 1, rightColumns = 1),
            buttons = c("copy", "csv", "excel", "pdf")
          )
        )
    })
  })

  ## create time spent by group
  output$timeStateByCluster <- renderUI({
    req(input$nbclust)
    mod <- colnames(time_spent_data_CFDA())
    summary_output_list <- lapply(mod, function(par) {
      plotname <- paste("timeSpentCluster", par, sep = "_")
      column(
        12,
        box(
          width = 12, title = paste("State:", par), solidHeader = TRUE, status = "danger",
          DTOutput(plotname)
        )
      )
    })
    do.call(tagList, summary_output_list)
  })

  ## Creation of the output for display the plots of time spent in each state by clusters (1 plots by state)
  observe({
    req(input$nbclust)
    timeSpent <- time_spent_data_CFDA()
    class_id <- data.frame(
      id = names(class()),
      res_class_cluster = as.vector(class())
    )
    d2 <- cbind.data.frame(as.data.frame(timeSpent[, seq_len(ncol(timeSpent))]), id = row.names(timeSpent))
    d1 <- unique(merge(d2, class_id, by = "id"))
    mod <- colnames(timeSpent)
    lapply(mod, function(par) {
      dAll <- d1
      dAll$res_class_cluster <- "All"
      d1 <- rbind.data.frame(d1, dAll)
      g <- ggplot(
          data = d1[, c("res_class_cluster", par)],
          aes(x = as.factor(res_class_cluster), y = d1[, par], fill = as.factor(res_class_cluster))
        ) +
        geom_boxplot() +
        xlab("Cluster") +
        ylab("Time spent") +
        labs(fill = "Cluster") +
        scale_fill_manual(values = rep(as.vector(color_data_CFDA()[names(color_data_CFDA()) == par]), input$nbclust + 1))

      output[[paste("timeSpentByStateCluster", par, sep = "_")]] <- renderPlotly(g)
    })
  })

  ## UI output for time spent among cluster
  output$timeStateGraphByAmongCluster <- renderUI({
    req(input$nbclust)
    mod <- colnames(time_spent_data_CFDA())
    summary_output_list <- lapply(mod, function(par) {
      plotname <- paste("timeSpentByStateCluster", par, sep = "_")
      column(
        12,
        box(
          width = 12, title = paste("State:", par), status = "danger", solidHeader = TRUE,
          shinycssloaders::withSpinner(
            plotlyOutput(plotname),
            type = getOption("spinner.type", default = 6),
            color = getOption("spinner.color", default = "#d73925")
          )
        )
      )
    })
    do.call(tagList, summary_output_list)
  })


  ## Creation of the output of Markov chain by clusters
  observe({
    req(input$nbclust)
    lapply(1:input$nbclust, function(par) {
      withProgress(
        message = "Making plots",
        detail = "Please wait until the end",
        value = 0,
        {
          incProgress(1 / 4)
          idToKeep <- names(class()[class() == par])
          data <- data_CFDA()[data_CFDA()$id %in% idToKeep, ]
          mark <- estimate_Markov(data[, c("id", "state", "time")])
          r <- color_data_CFDA()

          output[[paste("graphTransitionCluster", par, sep = "_")]] <- renderPlot({
            r <- r[names(r) %in% colnames(mark$P)]
            plot(mark, box.col = r)
          })
          output[[paste("matTransitionCluster", par, sep = "_")]] <- renderPrint(round(mark$P, 3))
          output[[paste("nJumpMatCluster", par, sep = "_")]] <- renderPrint(statetable(data))
          output[[paste("expoLawCluster", par, sep = "_")]] <- renderPrint(round(mark$lambda, 3))
        }
      )
    })
  })

  ## markov chain by clusters UI
  output$markovByCluster <- renderUI({
    t <- table(class())
    req(input$nbclust)
    if (input$choixStatsMarkovCluster == "transiMat") {
      plot_output_list <- lapply(c(1:input$nbclust), function(par) {
        plotname <- paste("matTransitionCluster", par, sep = "_")
        column(
          4,
          box(
            width = 12, status = "danger", solidHeader = TRUE, title = paste("Group:", par, "(n:", t[par], ")"),
            verbatimTextOutput(plotname),
            DTOutput(paste("dataCluster", par, sep = "_"))
          )
        )
      })
    } else if (input$choixStatsMarkovCluster == "transiGraph") {
      plot_output_list <- lapply(c(1:input$nbclust), function(par) {
        plotname <- paste("graphTransitionCluster", par, sep = "_")
        column(
          4,
          box(
            width = 12, status = "danger", solidHeader = TRUE, title = paste("Group:", par, "(n:", t[par], ")"),
            shinycssloaders::withSpinner(
              plotOutput(plotname),
              type = getOption("spinner.type", default = 6),
              color = getOption("spinner.color", default = "#d73925")
            )
          )
        )
      })
    } else if (input$choixStatsMarkovCluster == "jump") {
      plot_output_list <- lapply(c(1:input$nbclust), function(par) {
        plotname <- paste("nJumpMatCluster", par, sep = "_")
        column(
          4,
          box(
            width = 12, status = "danger", solidHeader = TRUE, title = paste("Group:", par, "(n:", t[par], ")"),
            verbatimTextOutput(plotname)
          )
        )
      })
    } else {
      plot_output_list <- lapply(c(1:input$nbclust), function(par) {
        plotname <- paste("expoLawCluster", par, sep = "_")
        column(
          4,
          box(
            width = 12, status = "danger", solidHeader = TRUE, title = paste("Group:", par, "(n:", t[par], ")"),
            verbatimTextOutput(plotname)
          )
        )
      })
    }
    do.call(tagList, plot_output_list)
  })


  ## Data set by cluster
  data_by_cluster <- reactive({
    req(input$nbclust)
    ldata <- lapply(c(1:input$nbclust), function(par) {
      idToKeep <- names(class()[class() == par])
      data <- data_CFDA()[data_CFDA()$id %in% idToKeep, ]
      if (length(listGroupVar()) == 0) {
        data
      } else {
        restData <- data_used()[data_used()$id %in% idToKeep, c("id", listGroupVar())]
        dataClust <- unique(merge(data, restData, by = "id"))
        dataClust
      }
    })
    names(ldata) <- paste0("cluster", 1:input$nbclust)
    ldata
  })

  ## Data CFDA with group variable
  data_with_group_var <- reactive({
    req(input$nbclust)
    if (length(listGroupVar()) == 0) {
      data_CAH()
    } else {
      class_id <- data.frame(
        id = names(class()),
        res_class_cluster = as.vector(class())
      )
      data <- merge(data_CFDA()[data_CFDA()$id %in% class_id$id, ], class_id, by = "id")
      restData <- data_used()[data_used()$id %in% class_id$id, c("id", listGroupVar())]
      dataClust <- unique(merge(data, restData, by = "id"))
      dataClust
    }
  })

  ## Download result of clustering
  output$downloadCAH <- downloadHandler(
    filename <- function() {
      paste("resClu.RData")
    },
    content = function(file) {
      results_clustering <- list(
        hc = hc(),
        data = data_with_group_var(),
        cluster = class(),
        dataByCluster = data_by_cluster()
      )
      save(results_clustering, file = file)
    }
  )

  ## Description of cluster by group variable
  output$groupVarDescCluster <- renderUI({
    selectizeInput("choixGroupVarClusterDesc", "Choose a variable", choices = listGroupVar())
  })


  ## Description with qualitative group variable by clusters (Frequencies table)
  output$freqGroupVarFiniByCluster <- DT::renderDataTable(
    {
      req(input$choixGroupVarClusterDesc)
      checkGroupVariable(data_used(), input$choixGroupVarClusterDesc)

      data <- data_with_group_var()
      data <- unique(data[, c("id", "res_class_cluster", input$choixGroupVarClusterDesc)])
      if (input$typeVarGroup %in% c("as.factor", "as.integer")) {
        if (input$typeVarGroup == "as.factor") {
          data[, input$choixGroupVarClusterDesc] <- as.factor(data[, input$choixGroupVarClusterDesc])
        } else {
          data[, input$choixGroupVarClusterDesc] <- as.integer(data[, input$choixGroupVarClusterDesc])
        }
        t <- table(data$res_class_cluster, data[, input$choixGroupVarClusterDesc])
        t <- as.data.frame.matrix(t)
        row_som <- apply(t, 1, sum)
        col_som <- apply(t, 2, sum)
        freq_table <- rbind.data.frame(cbind.data.frame(t, total = row_som), total = c(col_som, sum(col_som)))
        row.names(freq_table) <- c(paste("Cluster", 1:input$nbclust), "total")
        freq_table
      }
    },
    extensions = c("FixedColumns", "Buttons"),
    server = FALSE,
    options = list(
      dom = "Btipr",
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 1),
      buttons = c("copy", "csv", "excel", "pdf")
    )
  )


  ## Description with qualitative group variable by cluster (Proportions and profiles table)
  output$tableGroupVarFiniByCluster <- DT::renderDataTable(
    {
      req(input$choixGroupVarClusterDesc)
      checkGroupVariable(data_used(), input$choixGroupVarClusterDesc)

      data <- data_with_group_var()
      data <- unique(data[, c("id", "res_class_cluster", input$choixGroupVarClusterDesc)])
      if (input$typeVarGroup %in% c("as.factor", "as.integer")) {
        if (input$typeVarGroup == "as.factor") {
          data[, input$choixGroupVarClusterDesc] <- as.factor(data[, input$choixGroupVarClusterDesc])
        } else {
          data[, input$choixGroupVarClusterDesc] <- as.integer(data[, input$choixGroupVarClusterDesc])
        }
        t <- table(data$res_class_cluster, data[, input$choixGroupVarClusterDesc])
        if (input$tableGroupVarFiniChoiceCluster == "prop") {
          t <- as.data.frame.matrix(prop.table(t))
          row_som <- apply(t, 1, sum)
          col_som <- apply(t, 2, sum)
          res <- rbind.data.frame(cbind.data.frame(t, total = row_som), total = c(col_som, sum(col_som)))
          res <- round(res, 4) * 100
          row.names(res) <- c(paste("Cluster", 1:input$nbclust), "Total")
        } else if (input$tableGroupVarFiniChoiceCluster == "row") {
          res <- as.data.frame.matrix(round(lprop(t), 2))
          row.names(res) <- c(paste("Cluster", 1:input$nbclust), "Ensemble")
        } else {
          res <- as.data.frame.matrix(round(cprop(t), 2))
          row.names(res) <- c(paste("Cluster", 1:input$nbclust), "Total")
        }
        res
      }
    },
    extensions = c("FixedColumns", "Buttons"),
    server = FALSE,
    options = list(
      dom = "Btipr",
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 1),
      buttons = c("copy", "csv", "excel", "pdf")
    )
  )


  ## Description with real variable
  output$numVarGroupCluster <- DT::renderDataTable(
    {
      req(input$nbclust)
      req(input$choixGroupVarClusterDesc)
      checkGroupVariable(data_used(), input$choixGroupVarClusterDesc)

      data <- data_with_group_var()
      data <- unique(data[, c("id", "res_class_cluster", input$choixGroupVarClusterDesc)])
      if (input$typeVarGroup == "as.numeric") {
        data[, input$choixGroupVarClusterDesc] <- as.numeric(data[, input$choixGroupVarClusterDesc])
        d <- as.data.frame(matrix(nrow = 0, ncol = 8))

        for (i in 1:input$nbclust) {
          dt <- data[data$res_class_cluster == i, ]
          var <- dt[, input$choixGroupVarClusterDesc]
          q <- quantile(var)
          d <- rbind.data.frame(d, data.frame(
            Mean = round(mean(var), 2),
            Median = round(q[3], 2),
            Q1 = round(q[2], 2),
            Q3 = round(q[4], 2),
            Min = round(q[1], 2),
            Max = round(q[5], 2),
            Sd = round(sd(var), 2),
            Number = length(var)
          ))
        }
        q <- quantile(data[, input$choixGroupVarClusterDesc])
        d <- rbind.data.frame(d, data.frame(
          Mean = round(mean(data[, input$choixGroupVarClusterDesc]), 2),
          Median = round(q[3], 2),
          Q1 = round(q[2], 2),
          Q3 = round(q[4], 2),
          Min = round(q[1], 2),
          Max = round(q[5], 2),
          Sd = round(sd(data[, input$choixGroupVarClusterDesc]), 2),
          Number = length(data[, input$choixGroupVarClusterDesc])
        ))
        row.names(d) <- c(paste("Cluster", 1:input$nbclust), "All")
        d
      }
    },
    extensions = c("FixedColumns", "Buttons"),
    server = FALSE,
    options = list(
      dom = "Btipr",
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 1),
      buttons = c("copy", "csv", "excel", "pdf")
    )
  )


  ############################
  ## 6.Simulate markov chain #
  ############################

  ### Group Probability
  output$probabilityGp <- renderUI({
    matrixInput(
      "groupProbability",
      "Probability to be in a group",
      value = matrix(rep(1 / input$nbComponent, input$nbComponent), nrow = 1, ncol = input$nbComponent),
      class = "numeric",
      rows = list(names = FALSE),
      cols = list(names = FALSE)
    )
  })


  ### Transition Matrix
  output$listMatrix <- renderUI({
    t <- tagList()
    t <- tagList(
      t,
      matrixInput(
        paste("transitionMatMix", 1),
        paste("Transition matrix", 1),
        value = matrix(
          (1 - diag(input$nbStateMix)) / (input$nbStateMix - 1),
          nrow = input$nbStateMix,
          ncol = input$nbStateMix,
          dimnames = list(NULL, paste(rep("state", input$nbStateMix), c(1:input$nbStateMix)))
        ),
        class = "numeric",
        rows = list(names = FALSE),
        cols = list(names = TRUE, editableNames = TRUE)
      )
    )
    if (input$nbComponent > 1) {
      for (i in 2:input$nbComponent) {
        t <- tagList(
          t,
          matrixInput(
            paste("transitionMatMix", i),
            paste("Transition matrix", i),
            value = matrix(
              (1 - diag(input$nbStateMix)) / (input$nbStateMix - 1),
              nrow = input$nbStateMix,
              ncol = input$nbStateMix
            ),
            class = "numeric",
            rows = list(names = FALSE),
            cols = list(names = FALSE, editableNames = FALSE)
          )
        )
      }
    }
    t
  })

  ### Exponential parameters
  output$listLambda <- renderUI({
    t <- tagList()
    for (i in seq_len(input$nbComponent)) {
      t <- tagList(
        t,
        matrixInput(
          paste("sejourTimeParaMix", i),
          paste("Exponential parameter of sejour time", i),
          value = matrix(1, 1, input$nbStateMix),
          class = "numeric",
          rows = list(names = FALSE),
          cols = list(names = FALSE)
        )
      )
    }
    t
  })

  ### Initial Law
  output$listInitialLaw <- renderUI({
    t <- tagList()
    for (i in seq_len(input$nbComponent)) {
      t <- tagList(
        t,
        matrixInput(
          paste("initialLawMix", i),
          paste("Initial law", i),
          value = matrix(rep(1 / input$nbStateMix, input$nbStateMix), nrow = 1, ncol = input$nbStateMix),
          class = "numeric",
          rows = list(names = FALSE),
          cols = list(names = FALSE)
        )
      )
    }
    t
  })


  ## Mix model Data
  mixModelData <- reactive({
    input$SimulateMixtureModel
    isolate({
      p <- sample(
        seq_len(input$nbComponent),
        input$nbSimuMix,
        replace = TRUE,
        prob = as.vector(input$groupProbability)
      )
      r <- table(p)
      resData <- data.frame()
      d <- generate_Markov(
        r[1],
        input$nbStateMix,
        input[[paste("transitionMatMix", 1)]],
        as.vector(input[[paste("sejourTimeParaMix", 1)]]),
        as.vector(input[[paste("initialLawMix", 1)]]),
        input$TmaxMix,
        colnames(input[[paste("transitionMatMix", 1)]])
      )
      d <- cbind.data.frame(d, Component = 1)
      resData <- d
      for (i in 2:input$nbComponent) {
        d <- generate_Markov(
          r[i],
          input$nbStateMix,
          input[[paste("transitionMatMix", i)]],
          as.vector(input[[paste("sejourTimeParaMix", i)]]),
          as.vector(input[[paste("initialLawMix", i)]]),
          input$TmaxMix,
          colnames(input[[paste("transitionMatMix", 1)]])
        )
        d <- cbind.data.frame(d, Component = i)
        d$id <- d$id + max(resData$id)
        resData <- rbind(resData, d)
      }
      resData
    })
  })

  ## Data frame output
  output$headSimulatedMix <- renderDataTable(
    {
      mixModelData()
    },
    extensions = "Buttons",
    server = FALSE,
    options = list(
      dom = "Brtip",
      scrollX = TRUE,
      buttons = c("copy", "csv", "excel", "pdf"),
      rownames = FALSE
    )
  )

  ## Trajectories plot
  output$trajSimulatedMix <- renderPlot({
    gp <- mixModelData() %>% distinct(id, .keep_all = TRUE)
    plotData(mixModelData()[, -4], group = gp$Component, addId = FALSE, addBorder = FALSE, sort = TRUE)
  })
})
