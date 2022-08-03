if (!require('shiny'))
  install.packages("shiny")
if (!require('plotly'))
  install.packages("plotly")
if (!require('shinyMatrix'))
  install.packages("shinyMatrix")
if (!require('cfda'))
  install.packages("cfda")
if (!require('tidyverse'))
  install.packages("tidyverse")
if (!require('tractor.base'))
  install.packages("tractor.base")
if (!require('dplyr'))
  install.packages("dplyr")
if (!require('shinydashboard'))
  install.packages("shinydashboard")
if (!require('ggpubr'))
  install.packages("ggpubr")
if (!require('DT'))
  install.packages("DT")
if (!require('questionr'))
  install.packages("questionr")
if (!require('shinyWidgets'))
  install.packages("shinyWidgets")
if (!require('shinycssloaders'))
  install.packages("shinycssloaders")
if (!require('scales'))
  install.packages("scales")
if (!require('stringr'))
  install.packages("stringr")

MAXMOD<-12
shinyServer(function(input, output, session) {
  ##################
  #1. Import data ##
  ##################
  
  colorOfState<-reactive({
    etat=sort(unique(data_used()$state))
    codeColor<-hue_pal()(length(etat))
    res<-paste(paste0('"',etat,'"'),"=",paste0('"',codeColor,'"'))
    vecColor=paste("c(",toString(res),")")
    vecColor
 })
  

  ##Import a data set
  data_import <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == 'csv', "Please upload a csv file"))
    data <-
      read.csv(
        file$datapath,
        header = T,
        sep = input$sep,
        dec = input$dec
      )
  })
  
  ##Check if a file has been import
  output$filetest <- reactive({
    return(is.null(input$file1$datapath))
  })
  
  ##Create a output in UI for filetest
  outputOptions(output, 'filetest', suspendWhenHidden = FALSE)
  
  
  ##filter data set
  data_used <- reactive({
    if (sum(colnames(data_import()) %in% "id") != 1 |
        sum(colnames(data_import()) %in% "time") != 1 |
        sum(colnames(data_import()) %in% "state") != 1) {
      validate(
        "Error! data must have exatly one column named 'id', one column named 'state' and one column name 'time'"
      )
    }
    validate(need(
      is.numeric(data_import()[, "time"]),
      'time must be numeric, please choose correct decimal symbol'
    ))
    data <- data_import()
    input$applyMod
    isolate({
      ##Filter by length trajectories
      
      if (input$filterChoiceLength == '2') {
        minT <- data %>%
          group_by(id) %>%  filter(time == min(time, na.rm = TRUE))
        maxT <- data %>%
          group_by(id) %>%  filter(time == max(time, na.rm = TRUE))
       if (input$upper=="" & input$lower=="") {
         idToKeep <- minT$id
       } else if (input$upper=="" & !is.na(input$lower)) {
         tryCatch({ lower = as.double(input$lower)},
           error=function(cond) {
             validate(need(FALSE,"invalid time please check format"))
           })
          idToKeep <- minT[minT$time <= lower, "id"]$id
        } else if (!is.na(input$upper) & input$lower=="") {
          tryCatch({ upper = as.double(input$upper)},
                   error=function(cond) {
                     validate(need(FALSE,"invalid time please check format"))
                   })
          idToKeep <- maxT[maxT$time >= upper, "id"]$id
        } else{
         tryCatch({ upper = as.double(input$upper)
         lower = as.double(input$lower)},
                  error=function(cond) {
                    validate(need(FALSE,"invalid time please check format"))
                  })
          idToKeep <-intersect(minT[minT$time <= lower, "id"], maxT[maxT$time >= upper, "id"])$id
        }
        data <- data[data$id %in% idToKeep, ]
      }
      
      ##Filter by number of jump
      if (input$filterChoiceJump == '2') {
        nJump <- nJump_import()
        if (is.na(input$more) & is.na(input$less)) {
          idToKeep <- unique(data$id)
        } else if (is.na(input$more) & !is.na(input$less)) {
          idToKeep <- names(nJump[nJump <= input$less])
        } else if (!is.na(input$more) & is.na(input$less)) {
          idToKeep <- names(nJump[nJump >= input$more])
        } else{
          idToKeep <- names(nJump[nJump >= input$more & nJump <= input$less])
        }
        data <- data[data$id %in% idToKeep, ]
      }
      
      ##Filter by peurcentage
      if (input$filterChoicePercentage == '2') {
        idInd <- sort(unique(data$id))
        idToKeep <- idInd[1:floor(length(idInd) * input$nb_ind / 100)]
        data <- data[data$id %in% idToKeep, ]
      }
    })
    data
    
  })
  

  ##verify if a dataset exist
  output$fileUploaded <- reactive({
    return(!is.null(data_used()))
  })
  
  ##create a output in UI interface
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  ##show data set
  output$head <- DT::renderDataTable({
    data_used()
  }, extensions = 'Buttons', server = FALSE, options = list(
    dom = 'Brtip',
    scrollX = TRUE,
    buttons = c('copy', 'csv', 'excel', 'pdf'),
    rownames = FALSE
  ))

  ##filter on number of jump
  output$jumpInt <- renderUI({
    tagList(
      numericInput(
        "more",
        "Indiduals with more than ",
        min = 0,
        value = 0
      ),
      numericInput(
        "less",
        "and less than",
        min = 0,
        value = max(nJump()),
        max = max(nJump())
      )
    )
  })
  
  ##Number of jump of initial dataset
  nJump_import <- reactive({
    validate(need(
      is.numeric(data_import()[, "time"]),
      'time must be numeric, please choose correct decimal symbol'
    ))
    compute_number_jumps(data_import()[, c("id", "state", "time")], countDuplicated = FALSE)
  })
  
  output$Resume <- renderPrint({
    summary_cfd(data_used()[, c("id", "time", "state")])
  })
  
  
  output$downloadDataSet <- downloadHandler(
    filename = function() {
      paste("filterData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_used(), file)
    }
  )
  
  #2 visualize data
  #################
  
  listGroupVar <- reactive({
    var <- which(!colnames(data_used()) %in% c("id", "time", "state"))
    choice = colnames(data_used())[var]
    choice
  })
  output$groupVarVisualize <- renderUI({
    selectizeInput("groupVariableVisualize",
                   "Select a group variable",
                   choice = c(listGroupVar()))
  })
  
  ##Plot of trajectories
  plotOfData <- reactive({
    validate(need(
      is.numeric(data_used()[, "time"]),
      'time must be numeric, please choose correct decimal symbol'
    ))
    withProgress(message = 'making plot of individuals trajectories',
                 detail = 'please wait until the end',
                 value = 0,
                 {
                   incProgress(1 / 4)
                   p <-plotData(data_used()[, c("id", "time", "state")], addId = FALSE, addBorder =
                                FALSE,col=eval(parse(text=colorOfState()))
                                ) + labs(title = "Trajectories of the Markov process")
                   input$modPlotData
                   isolate({
                     if (input$choixParaGroupeVisualize == "All") {
                       class <- NULL
                     } else{
                       validate(need(
                         length(listGroupVar()) > 0,
                         "this file doesn't have group variable"
                       ))
                       validate(
                         need(
                           nrow(unique(data_used()[, c("id", input$groupVariableVisualize)])) == length(unique(data_used()$id)),
                           "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
                         )
                       )
                       r <-
                         unique(data_used()[, c("id", input$groupVariableVisualize)])
                       class <- r[, input$groupVariableVisualize]
                     }
                     p <-plotData(
                         data_used()[, c("id", "time", "state")],
                         group = class,
                         addId = input$addId,
                         addBorder = input$addBorder,
                         sort = input$sort,
                        col=eval(parse(text=colorOfState()))
                         
                       ) + labs(title = input$plotDataTitle)
                   })
                   for (i in 1:3) {
                     incProgress(1 / 4)
                     Sys.sleep(0.10)
                   }
                   p
                 })
  })
  
  output$traj <- renderPlot({
    plotOfData()
  }, height = 900)
  
  output$downloadPlotTraj = downloadHandler(
    filename =  function() {
      paste("plotData", "png", sep = ".")
    },
    content = function(file) {
      ggsave(file, plotOfData() , device = "png")
    }
  )
  
  ###########################
  #3. descriptive statistics#
  ###########################
  duration <- reactive({
    validate(need(
      is.numeric(data_used()[, "time"]),
      'time must be numeric, please choose correct decimal symbol'
    ))
    duration <- compute_duration(data_used()[, c("id", "time", "state")])
  })
  
  time_spent <- reactive({
    validate(need(
      is.numeric(data_used()[, "time"]),
      'time must be numeric, please choose correct decimal symbol'
    ))
    compute_time_spent(data_used()[, c("id", "state", "time")])
  })
  
  nJump <- reactive({
    validate(need(
      is.numeric(data_used()[, "time"]),
      'time must be numeric, please choose correct decimal symbol'
    ))
    compute_number_jumps(data_used()[, c("id", "time", "state")])
  })
  
  #Variable de groupe et plot
  output$groupVarStatistics <- renderUI({
    selectizeInput("groupVariableStatistics",
                   "Select a group variable",
                   choice = c(listGroupVar()))
  })
  
  
  ##Plot for descriptives statistics
  
  #summary
  observe({
    req(input$groupVariableStatistics)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableStatistics)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    mod = unique(data_used()[, c(input$groupVariableStatistics)])
    validate(need(
      length(mod) <= MAXMOD,
      paste(
        "this variable has too many modalities (",
        length(mod),
        "), the limit is",
        MAXMOD
      )
    ))
    lapply(mod, function(par) {
      withProgress(message = 'making plots',
                   detail = 'please wait until the end',
                   value = 0,
                   {
                     incProgress(1 / 4)
                     if (input$choixParaGroupeStatistics == 'byGroup') {
                       data <- data_used()[data_used()[, input$groupVariableStatistics] == par, ]
                       if (input$choixGraphiqueStats == "summary") {
                         output[[paste("summary", par, sep = "_")]] <-renderPrint({
                           summary_cfd(data[, c("id", "state", "time")])
                           })
                       }
                     }
                   })
    })
  })
  
  # Createsummary by groupe
  output$summaryGp <- renderUI({
    req(input$groupVariableStatistics)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableStatistics)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    mod = sort(unique(data_used()[, c(input$groupVariableStatistics)]))
    validate(need(
      length(mod) <= MAXMOD,
      paste(
        "this variable has too many modalities (",
        length(mod),
        "), the limit is",
        MAXMOD
      )
    ))
    summary_output_list <- lapply(mod, function(par) {
      plotname <- paste("summary", par, sep = "_")
      column(6, 
             box(width=12,title=paste("Group : ", par),status = "danger", solidHeader = FALSE,
      
      verbatimTextOutput(plotname)))
      
    })
    do.call(tagList, summary_output_list)
    
  })
  
  
  output$summary <- renderPrint({
    summary_cfd(data_used()[, c("id", "time", "state")])
  })
  
  output$plots <- renderPlotly({
    withProgress(message = 'making plots',
                 detail = 'please wait until the end',
                 value = 0,
                 {
                   incProgress(1 / 4)
                   if (input$choixGraphiqueStats == "jump") {
                     jump_gp <- data.frame(jump = as.vector(nJump()))
                     p <-
                       ggplot(data.frame(jump_gp), aes(x = jump)) + labs(x = "Number of jump", y = "Frequency",title="Distribution of number of jumps") +
                       geom_bar(fill = "lightblue", color = "black")
                     
                   } else if (input$choixGraphiqueStats == "duration") {
                     p <- hist(duration()) +labs(title="Distribution of duration of trajectories") 
                     
                   }
                   else if (input$choixGraphiqueStats == "timeState") {
                     p <- boxplot(time_spent()) + labs(title="Distribution of time spent by state") +scale_fill_manual(values=eval(parse(text=colorOfState())))
                   }
                 })
    p
  })
  
  
  output$timeStateGp <- renderPlotly({
    req(input$groupVariableStatistics)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableStatistics)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    mod = sort(unique(data_used()[, c(input$groupVariableStatistics)]))
    validate(need(
      length(mod) <= MAXMOD,
      paste(
        "this variable has too many modalities (",
        length(mod),
        "), the limit is",
        MAXMOD
      )
    ))
    x <- time_spent()
    df <-
      data.frame(
        timeSpent = as.vector(x),
        state = factor(rep(colnames(x), each = nrow(x)), levels = colnames(x)),
        id = as.vector(rownames(x))
      )
    data <-
      merge(df, data_used()[, c("id", input$groupVariableStatistics)], by = "id")
    p <-
      ggplot(data, aes_string(x = "state", y = "timeSpent", fill = "state")) +
      geom_boxplot() +
      labs(x = "State", y = "Time Spent", fill = "State",title=paste("Time spent in each state by",input$groupVariableStatistics)) + facet_wrap(input$groupVariableStatistics)+scale_fill_manual(values=eval(parse(text=colorOfState())))
    p
  })
  
  output$durationGp <- renderPlotly({
    req(input$groupVariableStatistics)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableStatistics)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    mod = sort(unique(data_used()[, c(input$groupVariableStatistics)]))
    validate(need(
      length(mod) <= MAXMOD,
      paste(
        "this variable has too many modalities (",
        length(mod),
        "), the limit is",
        MAXMOD
      )
    ))
    d <-
      cbind.data.frame(duration = as.vector(duration()),
                       id = names(duration()))
    dure_gp <-
      unique(merge(d, data_used()[, c("id", input$groupVariableStatistics)], by =
                     "id"))
    g <-
      ggplot(data.frame(dure_gp), aes_string(x = "duration")) + labs(x = "Duration", y = "Frequency",title=paste("Duration of trajectories by",input$groupVariableStatistics)) +
      geom_histogram(fill = "lightblue",
                     color = "black",
                     bins = floor(1 + log2(length(duration(
                     ))))) + facet_wrap(input$groupVariableStatistics)
    g
  })
  
  output$jumpGp <- renderPlotly({
    req(input$groupVariableStatistics)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableStatistics)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    
    mod = sort(unique(data_used()[, c(input$groupVariableStatistics)]))
    validate(need(
      length(mod) <= MAXMOD,
      paste(
        "this variable has too many modalities (",
        length(mod),
        "), the limit is",
        MAXMOD
      )
    ))
    d <- cbind.data.frame(jump = as.vector(nJump()), id = names(nJump()))
    jump_gp <-
      unique(merge(d, data_used()[, c("id", input$groupVariableStatistics)], by =
                     "id"))
    g <-
      ggplot(data.frame(jump_gp), aes_string(x = "jump")) + labs(x = "Number of jump", y = "Frequency",title=paste("Number of jumps by",input$groupVariableStatistics)) +
      geom_bar(fill = "lightblue", color = "black") + facet_wrap(input$groupVariableStatistics)
    g
  })
  
  
  
  output$summaryStatsAll <- DT::renderDataTable({
    if (input$choixGraphiqueStats == 'duration') {
      duration <- duration()
      q <- quantile(duration, seq(0, 1, 0.25))
      d <- data.frame(
        Mean = round(mean(duration), 2),
        Median = round(q[3], 2),
        Q1 = round(q[2], 2),
        Q3 = round(q[4], 2),
        Min = round(q[1], 2),
        Max = round(q[5], 2),
        Sd = round(sd(duration), 2)
      )
      row.names(d) <- c("All")
      
      d
    } else if (input$choixGraphiqueStats == 'jump') {
      nJump <- compute_number_jumps(data_used()[, c("id", "time", "state")])
      q <- quantile(nJump, seq(0, 1, 0.25))
      d <- data.frame(
        Mean = round(mean(nJump), 2),
        Median = round(q[3], 2),
        Q1 = round(q[2], 2),
        Q3 = round(q[4], 2),
        Min = round(q[1], 2),
        Max = round(q[5], 2),
        Sd = round(sd(nJump), 2)
      )
      row.names(d) <- c("All")
      d
    }
    
  }, extensions = c('FixedColumns', 'Buttons'), options = list(
    dom = 'Btipr',
    scrollX = TRUE,
    fixedColumns = list(leftColumns = 1),
    rownames = FALSE,
    buttons = c('copy', 'csv', 'excel', 'pdf')
  ))
  
  output$summaryStatsByGroup <- DT::renderDataTable({
    req(input$groupVariableStatistics)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableStatistics)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    mod = sort(unique(data_used()[, c(input$groupVariableStatistics)]))
    validate(need(
      length(mod) <= MAXMOD,
      paste(
        "this variable has too many modalities (",
        length(mod),
        "), the limit is",
        MAXMOD
      )
    ))
    if (input$choixGraphiqueStats == 'duration') {
      d <- as.data.frame(matrix(ncol = 9, nrow = 0))
      colnames(d) <- c("Mean", "Median", "Q1", "Q3", "Min", "Max", "Sd", "Number")
      for (i in mod) {
        data <- data_used()[data_used()[, input$groupVariableStatistics] == i, ]
        duration <- compute_duration(data[, c("id", "time", "state")])
        q <- quantile(duration, seq(0, 1, 0.25))
        d <-
          rbind.data.frame(
            d,
            data.frame(
              Mean = round(mean(duration), 2),
              Median = round(q[3], 2),
              Q1 = round(q[2], 2),
              Q3 = round(q[4], 2),
              Min = round(q[1], 2),
              Max = round(q[5], 2),
              Sd = round(sd(duration), 2),
              Number = length(duration)
            )
          )
      }
      q <- quantile(duration(), seq(0, 1, 0.25))
      d <-
        rbind.data.frame(
          d,
          data.frame(
            Mean = round(mean(duration()), 2),
            Median = round(q[3], 2),
            Q1 = round(q[2], 2),
            Q3 = round(q[4], 2),
            Min = round(q[1], 2),
            Max = round(q[5], 2),
            Sd = round(sd(duration()), 2),
            Number = length(duration())
          )
        )
      row.names(d) <- c(mod,"All")
      d
    } else if (input$choixGraphiqueStats == 'jump') {
      d <- as.data.frame(matrix(ncol = 9, nrow = 0))
      colnames(d) <-
        c("Mean", "Median", "Q1", "Q3", "Min", "Max", "Sd", "Number")
      for (i in mod) {
        data <- data_used()[data_used()[, input$groupVariableStatistics] == i, ]
        jump <- compute_number_jumps(data[, c("id", "time", "state")])
        q <- quantile(jump, seq(0, 1, 0.25))
        d <- rbind.data.frame(
          d,
          data.frame(
            Mean = round(mean(jump), 2),
            Median = round(q[3], 2),
            Q1 = round(q[2], 2),
            Q3 = round(q[4], 2),
            Min = round(q[1], 2),
            Max = round(q[5], 2),
            Sd = round(sd(jump), 2),
            Number = length(jump)
          )
        )
      }
      q <- quantile(nJump(), seq(0, 1, 0.25))
      d <- rbind.data.frame(
        d,
        data.frame(
          Mean = round(mean(nJump()), 2),
          Median = round(q[3], 2),
          Q1 = round(q[2], 2),
          Q3 = round(q[4], 2),
          Min = round(q[1], 2),
          Max = round(q[5], 2),
          Sd = round(sd(nJump()), 2),
          Number = length(nJump())
        )
      )
      row.names(d) <- c(mod,"All")
      d
      
    }
  }, extensions = c('FixedColumns', 'Buttons'), options = list(
    dom = 'Btipr',
    scrollX = TRUE,
    fixedColumns = list(leftColumns = 1),
    rownames = FALSE,
    buttons = c('csv', 'excel', 'pdf', 'copy')
  ))
  
  output$nJumpTable <- DT::renderDataTable({
    t <- table(as.vector(nJump()))
    name <- names(t)
    prop <- paste(round(prop.table(t), 4) * 100, "%")
    d <- rbind.data.frame(as.vector(t), prop)
    colnames(d) <- name
    row.names(d) <- c("Frequencies", "Proportions")
    d
  }, extensions = c('FixedColumns', 'Buttons'), server = FALSE, options = list(
    dom = 'Btipr',
    scrollX = TRUE,
    buttons = c('copy', 'csv', 'excel', 'pdf')
  ))
  
  ##Time spnet by state by groupe
  observe({
    req(input$groupVariableStatistics)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableStatistics)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    data <- data_used()
    mod = unique(data[, "state"])
    validate(need(
      length(unique(data[, input$groupVariableStatistics])) <= MAXMOD,
      paste(
        "this variable has too many modalities (",
        length(mod),
        "), the limit is",
        MAXMOD
      )
    ))
    timeSpent <- time_spent()
    gp <- unique(data[, input$groupVariableStatistics])
    lapply(mod, function(par) {
      d <- as.data.frame(matrix(ncol = 8, nrow = 0))
      for (i in gp) {
        idToKeep <- unique(data[data[, input$groupVariableStatistics] == i, "id"])
        time <- timeSpent[names(timeSpent[, par]) %in% idToKeep, par]
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
      time <- timeSpent[, par]
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
      row.names(d) <- c(gp, "All")
      d
      output[[paste("timeSpentGroup", par, sep = "_")]] <-
        DT::renderDataTable({
          d
        }, extensions = c('FixedColumns', 'Buttons'), server = FALSE, options = list(
          dom = 'Btipr',
          scrollX = TRUE,
          fixedColumns = list(leftColumns = 1),
          lengthMenu = c(2, 6, 12),
          pageLength = 2,
          buttons = c('copy', 'csv', 'excel', 'pdf')
        ))
      
    })
  })
  
  ##create time spent by group
  
  output$timeSpentTableGp <- renderUI({
    req(input$groupVariableStatistics)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableStatistics)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    mod = unique(data_used()[, input$groupVariableStatistics])
    validate(need(
      length(mod) <= MAXMOD,
      paste(
        "this variable has too many modalities (",
        length(mod),
        "), the limit is",
        MAXMOD
      )
    ))
    mod = colnames(time_spent())
    summary_output_list <- lapply(mod, function(par) {
      plotname <- paste("timeSpentGroup", par, sep = "_")
      column(12,
             box(width=12,title=paste("State :", par),status = "danger", solidHeader = TRUE,
                       DTOutput(plotname)))
      
    })
    do.call(tagList, summary_output_list)
    
  })
  
  
  output$timeSpentAllTable <- DT::renderDataTable({
    d <- as.data.frame(matrix(ncol = 8, nrow = 0))
    colnames(d) <- c("Mean", "Median", "Q1", "Q3", "Min", "Max", "Sd")
    timeSpent <- time_spent()
    mod = colnames(timeSpent)
    for (i in mod) {
      time <- timeSpent[, i]
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
          Sd = round(sd(time), 2)
        )
      )
    }
    row.names(d) <- mod
    
    d
  }, server = FALSE, options = list(
    dom = 'Btipr',
    scrollX = TRUE,
    fixedColumns = list(leftColumns = 1)
  ))
  
  
  output$nJumpTableGroupFreq <- DT::renderDataTable({
    req(input$groupVariableStatistics)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableStatistics)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    mod = unique(data_used()[, input$groupVariableStatistics])
    validate(need(
      length(mod) <= MAXMOD,
      paste(
        "this variable has too many modalities (",
        length(mod),
        "), the limit is",
        MAXMOD
      )
    ))
    
    jump <- data.frame(id = names(nJump()), jump = as.vector(nJump()))
    group <-
      unique(data_used()[, c("id", input$groupVariableStatistics)])
    jumpMerge <- merge(jump, group, by = "id")
    t <-
      as.data.frame.matrix(table(jumpMerge[, input$groupVariableStatistics], jumpMerge$jump))
    row_som <- apply(t, 1, sum)
    col_som <- apply(t, 2, sum)
    res <-
      rbind.data.frame(cbind.data.frame(t, total = row_som), total = c(col_som, sum(col_som)))
    
    res
  }, extensions = c('FixedColumns', 'Buttons'), server = FALSE, options = list(
    dom = 'Btipr',
    scrollX = TRUE,
    fixedColumns = TRUE,
    buttons = c('copy', 'csv', 'excel', 'pdf')
  ))
  
  
  output$nJumpTableGroupTable <- DT::renderDataTable({
    req(input$groupVariableStatistics)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableStatistics)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    mod = unique(data_used()[, input$groupVariableStatistics])
    validate(need(
      length(mod) <= MAXMOD,
      paste(
        "this variable has too many modalities (",
        length(mod),
        "), the limit is",
        MAXMOD
      )
    ))
    jump <- data.frame(id = names(nJump()), jump = as.vector(nJump()))
    group <-
      unique(data_used()[, c("id", input$groupVariableStatistics)])
    jumpMerge <- merge(jump, group, by = "id")
    t <-
      table(jumpMerge[, input$groupVariableStatistics], jumpMerge$jump)
    if (input$tableChoiceGroupDesc == "prop") {
      t <- as.data.frame.matrix(prop.table(t))
      row_som <- apply(t, 1, sum)
      col_som <- apply(t, 2, sum)
      res <-
        rbind.data.frame(cbind.data.frame(t, total = row_som),
                         total = c(col_som, sum(col_som)))
      res <- round(res, 4) * 100
    } else if (input$tableChoiceGroupDesc == "row") {
      res <- as.data.frame.matrix(round(lprop(t), 2))
    } else{
      res <- as.data.frame.matrix(round(cprop(t), 2))
    }
    
    
    res
  }, extensions = c('FixedColumns', 'Buttons'), server = FALSE, options = list(
    dom = 'Btipr',
    scrollX = TRUE,
    fixedColumns = list(leftColumns = 1),
    buttons = c('copy', 'csv', 'excel', 'pdf')
  ))
  
  
  output$statsRes <- renderUI({
    t <- tagList(
      conditionalPanel(
        "input.choixParaGroupeStatistics=='All'",
        conditionalPanel(
          "input.choixGraphiqueStats=='summary'",
          box(
            width = 12,
            title = "Summary of data set",status = "danger", solidHeader = TRUE,
            verbatimTextOutput("summary")
          )
          
        ),
        conditionalPanel(
          "input.choixGraphiqueStats=='jump' ||input.choixGraphiqueStats=='duration' ",
          column(4,
                 tags$div(
                   box(
                     width = 12,
                     title = "Summary of number of jumps",status = "danger", solidHeader = TRUE,
                     DTOutput("summaryStatsAll")
                     
                     
                   ),
                   conditionalPanel(
                     "input.choixGraphiqueStats=='jump'",
                     box(
                       width = 12,
                       title = "Frequencies and proportions",status = "danger", solidHeader = TRUE,
                       DTOutput("nJumpTable")
                     )
                   )
                 ))
        ),
        conditionalPanel(
          "input.choixGraphiqueStats=='timeState'",
          column(4,
                 box(
                   width = 12,status = "danger", solidHeader = TRUE,
                   title = "Summary of time spent by state",
                   DTOutput("timeSpentAllTable")
                 ))
        ),
        conditionalPanel("input.choixGraphiqueStats!='summary'",
                         column(8,
                           box(
                             width = 12,
                             status = "danger", 
                             shinycssloaders::withSpinner( plotlyOutput("plots", height = "1100px"),type = getOption("spinner.type", default = 6),
                                                           color = getOption("spinner.color", default = "#d73925"))
                           )
                         ))
        
        
      ),
      conditionalPanel(
        "input.choixParaGroupeStatistics=='byGroup'",
        conditionalPanel(
          "input.choixGraphiqueStats=='summary'",
          box(
            width = 12,
            title = "Summary of data set by group",status = "danger", solidHeader = TRUE,
            uiOutput("summaryGp")
          )
          
        ),
        conditionalPanel(
          "input.choixGraphiqueStats=='jump' ||input.choixGraphiqueStats=='duration'",
          column(4,
                 tags$div(
                   box(
                     width = 12,
                     title = "Summary of statistic by group",status = "danger", solidHeader = TRUE,
                     DTOutput("summaryStatsByGroup")
                   ),
                   conditionalPanel(
                     "input.choixGraphiqueStats=='jump'",
                     box(
                       width = 12,
                       title = "Frequencies by group variable",status = "danger", solidHeader = TRUE,
                       DTOutput("nJumpTableGroupFreq")
                     ),
                     box(
                       width = 12,
                       title = "Proportions by group variable",status = "danger", solidHeader = TRUE,
                       selectizeInput(
                         "tableChoiceGroupDesc",
                         "Choose a table",
                         choices = c(
                           "Proportions" = "prop",
                           "Row profiles" = "row",
                           "Column profiles" = "column"
                         ),
                         selected = "prop"
                       ),
                       DTOutput("nJumpTableGroupTable")
                     )
                   )
                 )),
          column( 8,
            box(
              width = 12,
              status = "danger",
              conditionalPanel(
                "input.choixGraphiqueStats=='jump'",
                shinycssloaders::withSpinner( plotlyOutput("jumpGp", height = "1100px"),type = getOption("spinner.type", default = 6),
                                              color = getOption("spinner.color", default = "#d73925"))
              ),
              conditionalPanel(
                "input.choixGraphiqueStats=='duration'",
                shinycssloaders::withSpinner( plotlyOutput("durationGp", height = "1100px"),type = getOption("spinner.type", default = 6),
                                              color = getOption("spinner.color", default = "#d73925"))
              ),
            )
          )
          
        ),
        conditionalPanel(
          "input.choixGraphiqueStats=='timeState'",
          column(4,
                 box(
                   width = 12,status = "danger", 
                   uiOutput("timeSpentTableGp")
                 )),
          column(8,
                 box(
                   width = 12,status = "danger",
                   shinycssloaders::withSpinner( plotlyOutput("timeStateGp", height = "1100px"),type = getOption("spinner.type", default = 6),
                                                 color = getOption("spinner.color", default = "#d73925"))
                 ))
        )
        
        
        
      )
    )
    t
  })
  
  ##4. estimate markov chain
  #######################
  output$groupVarMarkov <- renderUI({
    selectizeInput("groupVariableMarkov",
                   "Select a group variable",
                   choice = c(listGroupVar()))
  })
  
  estimateMarkovAll <- reactive({
    validate(need(
      is.numeric(data_used()[, "time"]),
      'time must be numeric, please choose correct decimal symbol'
    ),)
    withProgress(message = 'making transition graph',
                 detail = 'please wait until the end',
                 value = 0,
                 {
                   p <- estimate_Markov(data_used()[, c("id", "time", "state")])
                   for (i in 1:4) {
                     incProgress(1 / 4)
                     Sys.sleep(0.10)
                   }
                 })
    p
  })
  
  
  observe({
    req(input$groupVariableMarkov)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableMarkov)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    mod = unique(data_used()[, c(input$groupVariableMarkov)])
    validate(need(
      length(mod) <= MAXMOD,
      paste(
        "this variable has too many modalities (",
        length(mod),
        "), the limit is",
        MAXMOD
      )
    ))
    lapply(mod, function(par) {
      withProgress(message = 'making plots',
                   detail = 'please wait until the end',
                   value = 0,
                   {
                     incProgress(1 / 4)
                     if (input$choixParaGroupeMarkov == 'byGroup') {
                       data <- data_used()[data_used()[, input$groupVariableMarkov] == par, ]
                       mark <- estimate_Markov(data[, c("id", "state", "time")])
                       proba <- estimate_pt(data[, c("id", "state", "time")])
                       output[[paste("graphTransition", par, sep = "_")]] <-
                         renderPlot({
                           r<-eval(parse(text=colorOfState())) 
                           r<-r[names(r) %in% colnames(mark$P)]
                           plot(mark,box.col = r)

                         })
                       output[[paste("matTransition", par, sep = "_")]] <-
                         renderPrint({
                           round(mark$P,3)
                         })
                       output[[paste("nJumpMat", par, sep = "_")]] <-
                         renderPrint({
                           round(statetable(data),3)
                         })
                       
                       output[[paste("expoLaw", par, sep = "_")]] <-
                         renderPrint({
                           round(mark$lambda,3)
                         })
                       
                     }
                   })
    })
  })
  
  ##Transition graph by group
  output$transGraphByGroup <- renderUI({
    req(input$groupVariableMarkov)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableMarkov)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    mod = sort(unique(data_used()[, c(input$groupVariableMarkov)]))
    validate(need(
      length(mod) <= MAXMOD,
      paste(
        "this variable has too many modalities (",
        length(mod),
        "), the limit is",
        MAXMOD
      )
    ))
    plot_output_list <- lapply(mod, function(par) {
      plotname <- paste("graphTransition", par, sep = "_")
      nInd <-
        unique(data_used()[data_used()[, input$groupVariableMarkov] == par, c("id", input$groupVariableMarkov)])
      column(6, box(width=12,title=paste("Group : ", par , "(n:", nrow(nInd), ")")
      ,status = "danger", solidHeader = TRUE,
      shinycssloaders::withSpinner( plotOutput(plotname),type = getOption("spinner.type", default = 6),
                                    color = getOption("spinner.color", default = "#d73925"))))
      
    })
    do.call(tagList, plot_output_list)
    
  })
  
  ## Transition graph all
  output$transGraphAll <- renderPlot({
    r<-eval(parse(text=colorOfState())) 
    r<-r[names(r) %in% colnames(estimateMarkovAll()$P)]
    plot(estimateMarkovAll(),box.col=r)
  }, height = 900)
  
  
  ##Transition mat by group
  output$transMatByGroup <- renderUI({
    req(input$groupVariableMarkov)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableMarkov)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    mod = sort(unique(data_used()[, c(input$groupVariableMarkov)]))
    validate(need(
      length(mod) <= MAXMOD,
      paste(
        "this variable has too many modalities (",
        length(mod),
        "), the limit is",
        MAXMOD
      )
    ))
    plot_output_list <- lapply(mod, function(par) {
      plotname <- paste("matTransition", par, sep = "_")
      nInd <-
        unique(data_used()[data_used()[, input$groupVariableMarkov] == par, c("id", input$groupVariableMarkov)])
      column(6, box(width=12,title=paste("Group : ", par , "(n :", nrow(nInd), ")")
      ,status="danger",solidHeader = TRUE,
      verbatimTextOutput(plotname)))
      
    })
    do.call(tagList, plot_output_list)
  })
  
  ##transition mat all
  output$transMatAll <- renderPrint({
    round(estimateMarkovAll()$P,3)
  })
  
  
  ##Number jump state by group
  output$njumpMarkovByGroup <- renderUI({
    req(input$groupVariableMarkov)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableMarkov)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    mod = sort(unique(data_used()[, c(input$groupVariableMarkov)]))
    validate(need(
      length(mod) <= MAXMOD,
      paste(
        "this variable has too many modalities (",
        length(mod),
        "), the limit is",
        MAXMOD
      )
    ))
    
    plot_output_list <- lapply(mod, function(par) {
      plotname <- paste("nJumpMat", par, sep = "_")
      nInd <-
        nrow(unique((data_used()[data_used()[, input$groupVariableMarkov] == par, c("id", input$groupVariableMarkov)])))
      column(6, box(width=12,title=paste("Group : ", par , "(n :", nInd, ")")
                    ,status="danger",solidHeader = TRUE,
      verbatimTextOutput(plotname)))
      
    })
    do.call(tagList, plot_output_list)
    
  })
  
  ##number of jump state
  output$njumpMarkovAll <- renderPrint({
    statetable(data_used()[, c("id", "state", "time")])
  })
  
  ##proba state by group
  output$probaStateByGroup <- renderPlotly({
    req(input$groupVariableMarkov)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableMarkov)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    mod = sort(unique(data_used()[, c(input$groupVariableMarkov)]))
    validate(need(
      length(mod) <= MAXMOD,
      paste(
        "this variable has too many modalities (",
        length(mod),
        "), the limit is",
        MAXMOD
      )
    ))
    d <- data.frame(matrix(ncol = 4, nrow = 0))
    for (par in mod) {
      pt <-
        estimate_pt(data_used()[data_used()[, input$groupVariableMarkov] == par, c("id", "time", "state")])
      d <- rbind.data.frame(d,
                            data.frame(
                              factor(rep(rownames(pt$pt), each = ncol(pt$pt)), levels = rownames(pt$pt)),
                              as.vector(t(pt$pt)),
                              rep(pt$t, nrow(pt$pt)),
                              par
                            ))
    }
    
    colnames(d) <- c("State", "proba", "time", "groupe")
    p <-
      ggplot(d,
             aes_string(
               x = "time",
               y = "proba",
               group = "State",
               colour = "State"
             )) +
      geom_line() +
      ylim(0, 1) +
      labs(x = "Time", y = "p(t)", title = "P(X(t) = x)") + facet_wrap("groupe")+scale_colour_manual(values=eval(parse(text=colorOfState())))
    p
  })
  
  ##proba state
  output$probaStateAll <- renderPlotly({
    plot(estimate_pt(data_used()[, c("id", "state", "time")]))
  })
  
  ##exponentiel law by group
  output$expoLawMarkovByGroup <- renderUI({
    req(input$groupVariableMarkov)
    validate(
      need(
        nrow(unique(data_used()[, c("id", input$groupVariableMarkov)])) == length(unique(data_used()$id)),
        "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
      )
    )
    mod = sort(unique(data_used()[, c(input$groupVariableMarkov)]))
    plot_output_list <- lapply(mod, function(par) {
      plotname <- paste("expoLaw", par, sep = "_")
      nInd <-nrow(unique((data_used()[data_used()[, input$groupVariableMarkov] == par, c("id", input$groupVariableMarkov)])))
      column(6, box(width=12,title=paste("Group : ", par , "(n :", nInd, ")")
                    ,status="danger",solidHeader = TRUE,
      verbatimTextOutput(plotname)))
      
    })
    do.call(tagList, plot_output_list)
    
  })
  
  ##exponentiel law all
  output$expoLawMarkovAll <- renderPrint({
    round(estimateMarkovAll()$lambda,3)
  })
  
  ########################
  ##5. Factorial analysis#
  ########################
  
  
  
  output$fmcaUploaded <- reactive({
    return(!is.null(fmca()))
  })
  
  output$titleBoxFactorial<-renderUI({
    
    h4(paste0("Path observed on [",min(data_used()$time),";T]"))
  })
  output$uiAbsorbedState<-renderUI({
    state<-unique(data_used()[,"state"])
    multiInput(
      inputId = "absorbedState",
      label = "State(s) to be extended", 
      choices = NULL,
      choiceNames = state,
      choiceValues = state
    )
  })
  
  outputOptions(output, 'fmcaUploaded', suspendWhenHidden = FALSE)
  
  color_data_CFDA<-reactive({
    stateCFDA<-sort(unique(data_CFDA()$state))
    stateOrigin<-sort(unique(data_used()$state))
    if(length(stateCFDA)>length(stateOrigin)){
      nonObState<-stateCFDA[!(stateCFDA%in% stateOrigin)]
      color<-substr(colorOfState(),3,nchar(colorOfState())-1) 
      paste0("c(",color,",",'"',nonObState,'"',"=",'"',"#565656",'")')
      r<-eval(parse(text=paste0("c(",color,",",'"',nonObState,'"',"=",'"',"#565656",'")')))
      r[ order(names(r))]
    }else{
      colorOfState()
      r<-eval(parse(text=colorOfState()))
    }
  })
  data_CFDA <- reactive({
    input$soumettre
    isolate({
      tmax = as.double(input$tpsmax)
      resume <- summary_cfd(data_used()[, c("id", "state", "time")])
      minT = resume$timeRange[1]
      maxT = resume$timeRange[2]
      validate(
        need(
          resume$uniqueStart,
          "All individuals must have the same time start value"
        ),
        need(
          tmax >= minT,
          paste('End time must be greater than', minT)
        )
      )
      d <- cut_data(data_used()[, c("id", "time", "state")], Tmax=tmax, prolongLastState = input$absorbedState,NAstate=input$nameNAstate)
      d
    })
  })
  
  output$plotDataCFDA<-renderPlot({
    plotData(data_CFDA(),addId = FALSE,addBorder = FALSE,col=color_data_CFDA()
             )
  })
  output$summaryCFDA <- renderPrint({
    summary_cfd(data_CFDA())
  })
  
  fmca <- reactive({
    input$soumettre
    isolate({
      minTim <- min(data_CFDA()[, "time"])
      validate(
        need(
          is.numeric(data_used()[, "time"]),
          'Time must be numeric, please choose correct decimal separator'
        ),
        need(!is.na(as.double(input$tpsmax)) , 'end time T invalid '),
      )
      withProgress(message = 'Compute principal component',
                   detail = 'please wait until the end',
                   value = 0,
                   {
                     incProgress(2 / 15)
                     Sys.sleep(0.25)
                     set.seed(42)
                     tmax = as.double(input$tpsmax)
                     resume <- summary_cfd(data_used()[, c("id", "state", "time")])
                     minT = resume$timeRange[1]
                     maxT = resume$timeRange[2]
                     validate(
                       need(
                         resume$uniqueStart,
                         "all individuals must have the same time start value"
                       ),
                       need(
                         summary_cfd(data_CFDA())$nInd > 1,
                         'There is only one row or less with this End time please change the value'
                       ),
                       need(
                         tmax >= minT,
                         paste('end time must be greater than', minT)
                       )
                     )
                     if (input$typeBasis == 'spline') {
                       basis <-create.bspline.basis(c(min(data_CFDA()[, "time"]), tmax),
                                              nbasis = input$nbasis,
                                              norder = input$norder)
                     } else{
                       basis <- create.fourier.basis(c(min(data_CFDA()[, "time"]), tmax), nbasis = input$nbasis)
                     }
                     fmca <- compute_optimal_encoding(data_CFDA(), basis)
                     for (i in 3:15) {
                       incProgress(1 / 15)
                       Sys.sleep(0.25)
                     }
                     fmca
                   })
    })
  })
  
  eigenvalues <- reactive({
    vpselection <- which(round(fmca()$eigenvalues, 4) > 0)
    vp <- cbind.data.frame(
      eigenvalues = round(fmca()$eigenvalues[vpselection], 4),
      "Percentage of variance" = round(fmca()$eigenvalues[vpselection] /
                                         sum(fmca()$eigenvalues[vpselection]) * 100, 2),
      "Cumulative percentage of variance" = round(
        cumsum(fmca()$eigenvalues[vpselection]) / sum(fmca()$eigenvalues[vpselection]) *
          100,
        2
      )
    )
    rownames(vp) <-
      paste("Dim", 1:length(fmca()$eigenvalues[vpselection]))
    vp
  })
  
  output$eigenvaluesTable <- renderPrint({
    eigenvalues()
  })
  
  output$valeurspropres <- renderPlotly({
    tmax = as.double(input$tpsmax)
    min<-summary_cfd(data_used()[, c("id", "state", "time")])$timeRange[1]
    validate(
      need(
        summary_cfd(data_CFDA())$nInd > 1,
        'There is only one row or less with this End time please change the value'
      ),
      need(
        
          tmax >= min,
        paste('end time must be between greather than',min)
      )
    )
    if (input$cumulative) {
      g <-
        ggplot(cbind.data.frame(x = 1:nrow(eigenvalues()), y = eigenvalues()[, 3])) +
        ggtitle("Cumulative eigenvalues plot") +
        theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks =
                                                                             1:nrow(eigenvalues())) +
        geom_col(fill = "blue") + aes(x = x, y = y) +
        xlab("Component") + ylab("Percentage of variance")
      
    } else{
      g <-
        ggplot(cbind.data.frame(x = 1:nrow(eigenvalues()), y = eigenvalues()[, 2])) +
        ggtitle("Eigenvalues plot") +
        theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks =
                                                                             1:nrow(eigenvalues())) +
        geom_col(fill = "blue") + aes(x = x, y = y) +
        xlab("Component") + ylab("Percentage of variance")
    }
    g
  })
  
  output$dim1 <- renderUI({
    input$soumettre
    maxi <-isolate(input$nbasis * length(summary_cfd(data_CFDA()[, c("id", "state", "time")])$states))
    selectInput(
      inputId = "choix_dim1",
      label = "Axis 1",
      selected = 1,
      choices = seq(1, maxi, 1),
      multiple = FALSE
    )
  })
  
  output$dim2 <- renderUI({
    input$soumettre
    maxi <-
      isolate(input$nbasis * length(summary_cfd(data_CFDA()[, c("id", "state", "time")])$states))
    selectInput(
      inputId = "choix_dim2",
      label = "Axis 2 ",
      selected = 2,
      choices = seq(1, maxi, 1),
      multiple = FALSE
    )
  })
  
  
  output$planfact <- renderPlotly({
    req(input$choix_dim1, input$choix_dim2)
    if (input$groupVariableFactorialPlan == 'NONE') {
      plotComponent(fmca(),
                    comp = c(
                      as.numeric(input$choix_dim1),
                      as.numeric(input$choix_dim2)
                    ),
                    addNames = FALSE)
    } else{
      group <-
        unique(data_used()[data_used()$id %in% row.names(fmca()$pc), c("id", input$groupVariableFactorialPlan)])
      validate(need(
        nrow(group) == nrow(fmca()$pc),
        "can't be used as group variable"
      ))
     p<- plotComponent(fmca(),
                    comp = c(
                      as.numeric(input$choix_dim1),
                      as.numeric(input$choix_dim2)
                    ),
                    addNames = FALSE) 
     if(is.numeric(group[, input$groupVariableFactorialPlan]) |(is.integer(group[, input$groupVariableFactorialPlan]) & length(unique(group[, input$groupVariableFactorialPlan]))>MAXMOD)){
       p<-p+ geom_point(aes(color = group[, input$groupVariableFactorialPlan])) +
        labs(color =input$groupVariableFactorialPlan) 
     }else{
      p<-p+ geom_point(aes(color = as.factor(group[, input$groupVariableFactorialPlan]))) +
         labs(color =input$groupVariableFactorialPlan) 
       }
      p
    }
  })
  
  output$groupVarFactorialPlan <- renderUI({
    selectizeInput(
      "groupVariableFactorialPlan",
      "Select a group variable",
      choice = c("NONE", listGroupVar())
    )
  })
  
  optimalEncodingPlot1 <- reactive({
    req(input$choix_dim1)
    if(input$addCI){
      plot(fmca(),
           harm = as.numeric(input$choix_dim1),
           addCI = TRUE)+ylab("a_x(t)")+scale_fill_manual(values=color_data_CFDA())
    }else{
      plot(fmca(),
           harm = as.numeric(input$choix_dim1),
           addCI = FALSE)+ylab("a_x(t)")+scale_color_manual(values=color_data_CFDA())
    }
    
  })
  
  output$optimalEncoding1 <- renderPlotly({
    optimalEncodingPlot1()
  })
  
  optimalEncodingPlot2 <- reactive({
    req(input$choix_dim2)
    if(input$addCI){
      plot(fmca(),
           harm = as.numeric(input$choix_dim1),
           addCI = TRUE)+ylab("a_x(t)")+scale_fill_manual(values=color_data_CFDA())
    }else{
      plot(fmca(),
           harm = as.numeric(input$choix_dim2),
           addCI = FALSE)+ylab("a_x(t)")+scale_color_manual(values=color_data_CFDA())
    }
  })
  
  output$optimalEncoding2 <- renderPlotly({
    optimalEncodingPlot2()
  })
  
  
  ##. EXTREME IND
  ###############
  
  output$dim1Extrem <- renderUI({
    input$soumettre
    maxi <-
      isolate(input$nbasis * length(summary_cfd(data_CFDA()[, c("id", "state", "time")])$states))
    selectInput(
      inputId = "choix_dim1Extrem",
      label = "Axis 1",
      selected = 1,
      choices = seq(1, maxi, 1),
      multiple = FALSE
    )
  })
  
  output$dim2Extrem <- renderUI({
    input$soumettre
    maxi <-
      isolate(input$nbasis * length(summary_cfd(data_CFDA()[, c("id", "state", "time")])$states))
    selectInput(
      inputId = "choix_dim2Extrem",
      label = "Axis 2 ",
      selected = 2,
      choices = seq(1, maxi, 1),
      multiple = FALSE
    )
  })
  
  output$extremComp1ui <- renderUI({
    sliderInput(
      "extremComp1",
      paste("Extreme individuals on dimension", input$choix_dim1Extrem),
      min = 0,
      max = 100,
      value = 10
    )
  })
  
  output$extremComp2ui <- renderUI({
    sliderInput(
      "extremComp2",
      paste("Extreme individuals on dimension", input$choix_dim2Extrem),
      min = 0,
      max = 100,
      value = 10
    )
  })
  
  output$axe1 <- renderUI({
    req(input$choix_dim1Extrem)
    h4(paste("Dimension", input$choix_dim1Extrem))
  })
  
  output$axe2 <- renderUI({
    req(input$choix_dim2Extrem)
    h4(paste("Dimension", input$choix_dim2Extrem))
  })
  
  extremIndividuals <- reactive({
    minpc1 <- NULL
    minpc2 <- NULL
    maxpc1 <- NULL
    maxpc2 <- NULL
    n = nrow(fmca()$pc)
    
    ##Extrem on dim 1
    dim1SortIncre <-
      sort(fmca()$pc[, as.numeric(input$choix_dim1Extrem)])
    dim1SortDecre <- sort(dim1SortIncre, decreasing = TRUE)
    if (input$extremComp1 > 0) {
      minpc1 <- names(dim1SortIncre[1:ceiling(n * input$extremComp1 / 100 /2)])
      maxpc1 <- names(dim1SortDecre[1:ceiling(n * input$extremComp1 / 100 /2)])
    }
    
    ##Extrem on dim 2
    dim2SortIncre <-
      sort(fmca()$pc[, as.numeric(input$choix_dim2Extrem)])
    dim2SortDecre <- sort(dim2SortIncre, decreasing = TRUE)
    if (input$extremComp2 > 0) {
      minpc2 <- names(dim2SortIncre[1:ceiling(n * input$extremComp2 / 100/2)])
      maxpc2 <-
        names(dim2SortDecre[1:ceiling(n * input$extremComp2 / 100/2)])
    }
    
    list(
      minpc1 = minpc1,
      maxpc1 = maxpc1,
      minpc2 = minpc2,
      maxpc2 = maxpc2
    )
  })
  
  output$planfactExtremeIndividuals <- renderPlotly({
    req(input$choix_dim2Extrem, input$choix_dim1Extrem)
    ids <- unique(data_CFDA()$id)
    group <-
      factor(
        rep("Not extrem", length(ids)),
        levels = c(
          "Extrem on axis 2",
          "Extrem on axis 1",
          "Not extrem",
          "Extrem on both axis"
        )
      )
    group[ids %in% extremIndividuals()$minpc1] = "Extrem on axis 1"
    group[ids %in% extremIndividuals()$maxpc1] = "Extrem on axis 1"
    group[ids %in% extremIndividuals()$minpc2] = "Extrem on axis 2"
    group[ids %in% extremIndividuals()$maxpc2] = "Extrem on axis 2"
    group[ids %in% intersect(extremIndividuals()$minpc1, extremIndividuals()$minpc2)] =
      "Extrem on both axis"
    group[ids %in% intersect(extremIndividuals()$maxpc1, extremIndividuals()$maxpc2)] =
      "Extrem on both axis"
    p <-
      plotComponent(fmca(),
                    comp = c(
                      as.numeric(input$choix_dim1Extrem),
                      as.numeric(input$choix_dim2Extrem)
                    ),
                    addNames = FALSE) +
      geom_point(aes(color = group)) + scale_color_manual(values = c("#000CFF", "#FF0000", "#000000", "#7000FF"))
    p
    
  })
  
  plotDataExtremAxe1 <- reactive({
    validate(need(input$extremComp1 > 0, "No extrem individuals selected"))
    ids <- unique(data_CFDA()$id)
    group <-
      factor(rep(NA, length(ids)),
             levels = c("Lowest component values", "Highest component values"))
    group[ids %in% extremIndividuals()$minpc1] = "Lowest component values"
    group[ids %in% extremIndividuals()$maxpc1] = "Highest component values"
    plotData(
      data_CFDA(),
      group = group,
      addBorder = F,
      addId = F,
      col=color_data_CFDA()
    ) + labs(title = paste("Extreme individuals on component", input$choix_dim1))
    
  })
  
  output$plotDataExtremAxe1 <- renderPlot({
    validate(need(input$extremComp1 > 0, "no extrem individuals selected"))
    plotDataExtremAxe1()
  })
  
  plotDataExtremAxe2 <- reactive({
    validate(need(input$choix_dim2Extrem > 0, "no extrem individuals selected"))
    ids <- unique(data_CFDA()$id)
    group <-
      factor(rep(NA, length(ids)),
             levels = c("Lowest component values", "Highest component values"))
    group[ids %in% extremIndividuals()$minpc2] = "Lowest component values"
    group[ids %in% extremIndividuals()$maxpc2] = "Highest component values"
    plotData(
      data_CFDA(),
      group = group,
      addBorder = F,
      addId = F,
      col=color_data_CFDA()
    ) + labs(title = paste("Extreme individuals on component", input$choix_dim2))
  })
  
  
  output$plotDataExtremAxe2 <- renderPlot({
    validate(need(input$choix_dim2Extrem > 0, "no extrem individuals selected"))
    plotDataExtremAxe2()
  })
  
  output$downloadPlotDataExtremAxe1 = downloadHandler(
    filename =  function() {
      paste("extremeIndividualsAxe1", "png", sep = ".")
    },
    content = function(file) {
      ggsave(file, dendogramme() , device = "png")
    }
  )
  
  output$downloadPlotDataExtremAxe2 = downloadHandler(
    filename =  function() {
      paste("extremeIndividualsAxe2", "png", sep = ".")
    },
    content = function(file) {
      ggsave(file, dendogramme() , device = "png")
    }
  )
  
  data_used_CFDA <- reactive({
    merge(data_CFDA(), data_used()[, c("id", listGroupVar())], by = "id")
  })
  
  output$downloadResultsCFDA <- downloadHandler(
    filename <- function() {
      paste("resFactorialAnalysis.RData")
    },
    
    content = function(file) {
      results_factorial_analysis <-
        list(dataUsed = data_used_CFDA(), optimal_encoding = fmca())
      save(results_factorial_analysis, file = file)
    }
  )
  ################
  ##4.Clustering #
  ################
  
  ##stat by cluster
  time_spent_data_CFDA <- reactive({
    compute_time_spent(data_CFDA())
  })
  
  nJump_data_CFDA <- reactive({
    compute_number_jumps(data_CFDA())
  })
  
  output$nJumpGraphByCluster <- renderPlotly({
    jump_gp <-
      cbind.data.frame(jump = as.factor(as.vector(nJump_data_CFDA())), cluster =
                         class())
    g <-
      ggplot(data.frame(jump_gp), aes(x = jump)) + labs(x = "Number of jump", y = "Frequency") +
      geom_bar(fill = "lightblue", color = "black") + facet_wrap("cluster")
    g
  })
  
  output$effecCluster<-renderPrint({
    req(input$nbclust)
    d<-as.data.frame(table(class()))
    prop<-round(prop.table(table(class())),4)*100
    d<-cbind.data.frame(d,prop=as.vector(prop))
    row.names(d)<-paste("cluster",1:input$nbclust)
    d[,2:3]
  })
  
  output$nb_comp <- renderUI({
    input$soumettre
    maxi <-
      isolate(input$nbasis * length(summary_cfd(data_CFDA()[, c("id", "state", "time")])$states))
    selectInput(
      inputId = "nbcomp",
      label = "Number of components for clustering",
      selected = 1,
      choices = seq(1, maxi, 1),
      multiple = FALSE
    )
  })
  
  
  hc <- reactive({
    input$clusteringSubmit
    isolate({
      if (input$compsCAH == 1) {
        ncomp <- as.numeric(input$nbcomp)
        hclust(dist(fmca()$pc[, 1:ncomp]), method = input$method)
      } else{
        if (input$percentageVariance == 100) {
          hclust(dist(fmca()$pc), method = input$method)
        } else{
          ncomp <-
            which(cumsum(prop.table(fmca()$eigenvalues)) >= input$percentageVariance /
                    100)[1]
          hclust(dist(fmca()$pc[, 1:ncomp]), method = input$method)
        }
        
      }
    })
    
  })
  dendogramme <- reactive({
    plot(hc(), labels = FALSE)
    if (is.null(input$nbclust)) {
      rect.hclust(hc(), 2, border = "green3")
    } else{
      if (input$couper) {
        rect.hclust(hc(), input$nbclust, border = "green3")
      }
    }
  })
  
  output$dendogramme <- renderPlot({
    dendogramme()
  })
  
max_cluster<-reactive({
  max=length(unique(data_CFDA()[,"id"]))-1
})
output$clus <- renderUI({
  numericInput(
    "nbclust",
    "Number of clusters",
    min = 2,
    max = max_cluster(),
    value = 2
  )
})

class <- reactive({
  req(input$nbclust)
  validate(
    need(input$nbclust<=max_cluster() &input$nbclust>=2,paste("the number of clusters must be between",2,"and",max_cluster()))
  )
  class <- cutree(hc(), k = input$nbclust)
})
groupe <- reactive({
  plotData(
    data_CFDA(),
    group = class(),
    addId = FALSE,
    addBorder = FALSE,
    col=color_data_CFDA(),
    sort = TRUE
  )
})

output$groupe <- renderPlot({
  groupe()
})


output$downloadDendogram = downloadHandler(
  filename =  function() {
    paste("dendogram", "png", sep = ".")
  },
  content = function(file) {
    ggsave(file, dendogramme() , device = "png")
  }
)

output$downloadGroupPlot = downloadHandler(
  filename =  function() {
    paste("groupPlot", "png", sep = ".")
  },
  content = function(file) {
    ggsave(file, groupe(), device = "png")
  }
)

output$timeStateGraphByCluster <- renderPlotly({
  x <- time_spent_data_CFDA()
  df <-
    data.frame(
      timeSpent = as.vector(x),
      state = factor(rep(colnames(x), each = nrow(x)), levels = colnames(x)),
      id = as.vector(rownames(x))
    )
  d <- cbind.data.frame(id = names(class()), cluster = class())
  data <- unique(merge(df, d, by = "id"))
  p <-
    ggplot(data, aes_string(x = "state", y = "timeSpent", fill = "state")) +
    geom_boxplot() +
    labs(x = "State", y = "Time Spent", fill = "State") + facet_wrap("cluster")+scale_fill_manual(values=color_data_CFDA())
  p
  
})



output$SummaryJumpByCluster <- DT::renderDataTable({
  d <- as.data.frame(matrix(ncol = 8, nrow = 0))
  colnames(d) <-c("Mean", "Median", "Q1", "Q3", "Min", "Max", "Sd", "Number")
  for (i in c(1:input$nbclust)) {
    idToKeep <- names(class()[class() == i])
    data <- data_CFDA()[data_CFDA()$id %in% idToKeep, ]
    jump <- compute_number_jumps(data[, c("id", "time", "state")])
    q <- quantile(jump, seq(0, 1, 0.25))
    d <- rbind.data.frame(
      d,
      data.frame(
        Mean = round(mean(jump), 2),
        Median = round(q[3], 2),
        Q1 = round(q[2], 2),
        Q3 = round(q[4], 2),
        Min = round(q[1], 2),
        Max = round(q[5], 2),
        Sd = round(sd(jump), 2),
        Number = length(jump)
      )
    )
  }
  jump <- compute_number_jumps(data_CFDA()[, c("id", "time", "state")])
  q <- quantile(jump, seq(0, 1, 0.25))
  d <- rbind.data.frame(
    d,
    data.frame(
      Mean = round(mean(jump), 2),
      Median = round(q[3], 2),
      Q1 = round(q[2], 2),
      Q3 = round(q[4], 2),
      Min = round(q[1], 2),
      Max = round(q[5], 2),
      Sd = round(sd(jump), 2),
      Number = length(jump)
    )
  )
  row.names(d) <- c(paste("cluster",1:input$nbclust),"All")
  d
}, extensions = c('FixedColumns', 'Buttons'), server = FALSE, options = list(
  dom = 'Btipr',
  scrollX = TRUE,
  fixedColumns = list(leftColumns = 1),
  buttons = c('copy', 'csv', 'excel', 'pdf')
))

output$freqJumpByCluster <- DT::renderDataTable({
  jump <-data.frame(id = names(nJump_data_CFDA()),
                    jump = as.vector(nJump_data_CFDA()))
  group <-cbind.data.frame(id = names(class()), group = as.vector(class()))
  jumpMerge <- merge(jump, group, by = "id")
  t <- table(jumpMerge$group, jumpMerge$jump)
  t <- as.data.frame.matrix(t)
  row_som <- apply(t, 1, sum)
  col_som <- apply(t, 2, sum)
  res <-rbind.data.frame(cbind.data.frame(t, total = row_som), total = c(col_som, sum(col_som)))
  row.names(res) <- c(paste("cluster",1:input$nbclust),"total")
  res
  
}, extensions = c('FixedColumns', 'Buttons'), server = FALSE, options = list(
  dom = 'Btipr',
  scrollX = TRUE,
  fixedColumns = list(leftColumns = 1),
  buttons = c('copy', 'csv', 'excel', 'pdf')
))

output$tableJumpByCluster <- DT::renderDataTable({
  jump <-
    data.frame(id = names(nJump_data_CFDA()),
               jump = as.vector(nJump_data_CFDA()))
  group <-
    cbind.data.frame(id = names(class()), group = as.vector(class()))
  jumpMerge <- merge(jump, group, by = "id")
  t <- table(jumpMerge$group, jumpMerge$jump)
  if (input$tableChoiceCluster == "prop") {
    t <- as.data.frame.matrix(prop.table(t))
    row_som <- apply(t, 1, sum)
    col_som <- apply(t, 2, sum)
    res <-
      rbind.data.frame(cbind.data.frame(t, total = row_som),
                       total = c(col_som, sum(col_som)))
    res <- round(res, 4) * 100
    row.names(res) <- c(paste("cluster",1:input$nbclust),"total")
  } else if (input$tableChoiceCluster == "row") {
    res <- as.data.frame.matrix(round(lprop(t), 2))
    row.names(res) <- c(paste("cluster",1:input$nbclust),"Ensemble")
    
  } else{
    res <- as.data.frame.matrix(round(cprop(t), 2))
    row.names(res) <- c(paste("cluster",1:input$nbclust),"total")
    
  }
  
  res
  
}, extensions = c('FixedColumns', 'Buttons'), server = FALSE, options = list(
  dom = 'Btipr',
  scrollX = TRUE,
  fixedColumns = list(leftColumns = 1),
  buttons = c('copy', 'csv', 'excel', 'pdf')
))

##Time spnet by state by groupe
observe({
  req(input$nbclust)
  timeSpent <- time_spent_data_CFDA()
  mod <- colnames(timeSpent)
  lapply(mod, function(par) {
    d <- as.data.frame(matrix(ncol = 8, nrow = 0))
    for (i in 1:input$nbclust) {
      idToKeep <- names(class()[class() == i])
      time <- timeSpent[names(timeSpent[, par]) %in% idToKeep, par]
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
    time <- timeSpent[, par]
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
    row.names(d) <- c(paste("Cluster",1:input$nbclust), "All")
    plotname <- paste("timeSpentCluster", par, sep = "_")
    
    output[[paste0("download", plotname)]] <- downloadHandler(
      filename =  function() {
        paste("bygroup", "csv", sep = ".")
      },
      content = function(file) {
        write.csv(d, file = file)
      }
    )
    
    output[[paste("timeSpentCluster", par, sep = "_")]] <-
      DT::renderDataTable({
        d
      }, extensions = c('FixedColumns', 'Buttons'), server = FALSE, options = list(
        dom = 'Btipr',
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 1, rightColumns = 1),
        buttons = c('copy', 'csv', 'excel', 'pdf')
      ))
  })
})

##create time spent by group
output$timeStateByCluster <- renderUI({
  req(input$nbclust)
  mod = colnames(time_spent_data_CFDA())
  summary_output_list <- lapply(mod, function(par) {
    plotname <- paste("timeSpentCluster", par, sep = "_")
    column(12,
           box(width=12,title=paste("State :", par),solidHeader = TRUE,status="danger",
               DTOutput(plotname)))
    
  })
  do.call(tagList, summary_output_list)
  
})


observe({
  req(input$nbclust)
  timeSpent <- time_spent_data_CFDA()
  class_id <-data.frame(id = names(class()),
                        res_class_cluster = as.vector(class()))
  d2<-cbind.data.frame(as.data.frame(timeSpent[,1:ncol(timeSpent)]),id=row.names(timeSpent))
  d1<-merge(d2,class_id,by="id")
  mod <- colnames(timeSpent)
  lapply(mod, function(par){
    dAll<-d1
    dAll$res_class_cluster<-"All"
    d1<-rbind.data.frame(d1,dAll)
    g<-ggplot(data=d1[,c("res_class_cluster",par)],aes(x=as.factor(res_class_cluster),y=d1[,par],fill=as.factor(res_class_cluster)))+geom_boxplot()+xlab("Cluster")+ylab("Time spent")+labs(fill="Cluster")+scale_fill_manual(values=rep(as.vector(color_data_CFDA()[names(color_data_CFDA())==par]),input$nbclust+1))
    output[[paste("timeSpentByStateCluster",par,sep="_")]]<-renderPlotly({
      g
    })
  })
})

output$timeStateGraphByAmongCluster <- renderUI({
  req(input$nbclust)
  mod = colnames(time_spent_data_CFDA())
  summary_output_list <- lapply(mod, function(par) {
    plotname <- paste("timeSpentByStateCluster", par, sep = "_")
    column(12,
           box(width=12,title= paste("State :", par),status="danger", solidHeader=TRUE,
               shinycssloaders::withSpinner( plotlyOutput(plotname),type = getOption("spinner.type", default = 6),
                                             color = getOption("spinner.color", default = "#d73925"))))
    
  })
  do.call(tagList, summary_output_list)
  
})

observe({
  req(input$nbclust)
  lapply(c(1:input$nbclust), function(par) {
    withProgress(message = 'making plots',
                 detail = 'please wait until the end',
                 value = 0,
                 {
                   incProgress(1 / 4)
                   idToKeep <- names(class()[class() == par])
                   data <- data_CFDA()[data_CFDA()$id %in% idToKeep, ]
                   mark <- estimate_Markov(data[, c("id", "state", "time")])
                   r<-color_data_CFDA()
                   
                   output[[paste("graphTransitionCluster", par, sep = "_")]] <-
                     renderPlot({
                       r<-r[names(r) %in% colnames(mark$P)]
                      plot(mark,box.col = r)
                     })
                   output[[paste("matTransitionCluster", par, sep = "_")]] <-
                     renderPrint({
                      round(mark$P,3)
                     })
                   output[[paste("nJumpMatCluster", par, sep = "_")]] <-
                     renderPrint({
                       statetable(data)
                     })
                   output[[paste("expoLawCluster", par, sep = "_")]] <-
                     renderPrint({
                       round(mark$lambda,3)
                     })
                 })
  })
})

output$markovByCluster <- renderUI({
  t <- table(class())
  req(input$nbclust)
  if (input$choixStatsMarkovCluster == 'transiMat') {
    plot_output_list <- lapply(c(1:input$nbclust), function(par) {
      plotname <- paste("matTransitionCluster", par, sep = "_")
      column(4,
             box(width=12,status="danger", solidHeader=TRUE,title=paste("Group :", par, "(n :", t[par], ")"),
                 verbatimTextOutput(plotname),
                 DTOutput(paste("dataCluster", par, sep = "_"))))
    })
  } else if (input$choixStatsMarkovCluster == 'transiGraph') {
    plot_output_list <- lapply(c(1:input$nbclust), function(par) {
      plotname <- paste("graphTransitionCluster", par, sep = "_")
      column(4,
             box(width=12,status="danger", solidHeader=TRUE,title=paste("Group :", par, "(n :", t[par], ")"),
                 shinycssloaders::withSpinner(plotOutput(plotname),type = getOption("spinner.type", default = 6),
                                              color = getOption("spinner.color", default = "#d73925"))))
    })
  } else if (input$choixStatsMarkovCluster == 'jump') {
    plot_output_list <- lapply(c(1:input$nbclust), function(par) {
      plotname <- paste("nJumpMatCluster", par, sep = "_")
      column(4,
             box(width=12,status="danger", solidHeader=TRUE,title=paste("Group :", par, "(n :", t[par], ")"),
                 verbatimTextOutput(plotname)))
    })
  } else{
    plot_output_list <- lapply(c(1:input$nbclust), function(par) {
      plotname <- paste("expoLawCluster", par, sep = "_")
      column(4,
             box(width=12,status="danger", solidHeader=TRUE,title=paste("Group :", par, "(n :", t[par], ")"),
                 verbatimTextOutput(plotname)))
    })
  }
  do.call(tagList, plot_output_list)
})

##Download result of clustering
data_by_cluster <- reactive({
  req(input$nbclust)
  ldata <- lapply(c(1:input$nbclust), function(par) {
    idToKeep <- names(class()[class() == par])
    data <- data_CFDA()[data_CFDA()$id %in% idToKeep, ]
    if(length(listGroupVar())==0){
      data
    }else{
      restData <-data_used()[data_used()$id %in% idToKeep, c("id", listGroupVar())]
      dataClust <- unique(merge(data, restData, by = "id"))
      dataClust
    }
     
  })
  names(ldata) <- paste0("cluster", 1:input$nbclust)
  ldata
})

data_with_group_var <- reactive({
  req(input$nbclust)
  if(length(listGroupVar())==0){
    data_CAH()
  }else{
    class_id <-data.frame(id = names(class()),
                 res_class_cluster = as.vector(class()))
    data <-
      merge(data_CFDA()[data_CFDA()$id %in% class_id$id, ], class_id, by = "id")
    restData <-
      data_used()[data_used()$id %in% class_id$id, c("id", listGroupVar())]
    dataClust <- unique(merge(data, restData, by = "id"))
    dataClust}
})

output$downloadCAH <- downloadHandler(
  filename <- function() {
    paste("resClu.RData")
  },
  
  content = function(file) {
    results_clustering <-
      list(
        hc = hc(),
        data = data_with_group_var(),
        cluster = class() ,
        dataByCluster = data_by_cluster()
      )
    save(results_clustering, file = file)
  }
)



##Descriptiion of cluster by group variable
output$groupVarDescCluster <- renderUI({
  selectizeInput("choixGroupVarClusterDesc",
                 "Choose a variable",
                 choices = listGroupVar())
})



##Descrip with qualitative
output$freqGroupVarFiniByCluster <- DT::renderDataTable({
  validate(
    need(
      nrow(unique(data_used()[, c("id", input$choixGroupVarClusterDesc)])) == length(unique(data_used()$id)),
      "
      this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
    )
  )
  
  data <- data_with_group_var()
  data <-unique(data[, c("id",
                         "res_class_cluster",
                         input$choixGroupVarClusterDesc)])
  if (input$typeVarGroup %in% c("as.factor", "as.integer")) {
    if (input$typeVarGroup == "as.factor") {
      data[, input$choixGroupVarClusterDesc] <-
        as.factor(data[, input$choixGroupVarClusterDesc])
    } else{
      data[, input$choixGroupVarClusterDesc] <-
        as.integer(data[, input$choixGroupVarClusterDesc])
    }
    t <-table(data$res_class_cluster, data[, input$choixGroupVarClusterDesc])
    t <- as.data.frame.matrix(t)
    row_som <- apply(t, 1, sum)
    col_som <- apply(t, 2, sum)
    freq_table <-rbind.data.frame(cbind.data.frame(t, total = row_som),
                                  total = c(col_som, sum(col_som)))
    row.names(freq_table)<-c(paste("Cluster",1:input$nbclust),"total")
    freq_table
  }
}, extensions = c('FixedColumns', 'Buttons'), server = FALSE, options = list(
  dom = 'Btipr',
  scrollX = TRUE,
  fixedColumns = list(leftColumns = 1),
  buttons = c('copy', 'csv', 'excel', 'pdf')
))

output$tableGroupVarFiniByCluster <- DT::renderDataTable({
  validate(
    need(
      nrow(unique(data_used()[, c("id", input$choixGroupVarClusterDesc)])) == length(unique(data_used()$id)),
      "this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
    )
  )
  
  data <- data_with_group_var()
  data <-unique(data[, c("id",
                         "res_class_cluster",
                         input$choixGroupVarClusterDesc)])
  if (input$typeVarGroup %in% c("as.factor", "as.integer")) {
    if (input$typeVarGroup == "as.factor") {
      data[, input$choixGroupVarClusterDesc] <-
        as.factor(data[, input$choixGroupVarClusterDesc])
    } else{
      data[, input$choixGroupVarClusterDesc] <-
        as.integer(data[, input$choixGroupVarClusterDesc])
    }
    t <-table(data$res_class_cluster, data[, input$choixGroupVarClusterDesc])
    if (input$tableGroupVarFiniChoiceCluster == "prop") {
      t <- as.data.frame.matrix(prop.table(t))
      row_som <- apply(t, 1, sum)
      col_som <- apply(t, 2, sum)
      res <-
        rbind.data.frame(cbind.data.frame(t, total = row_som),
                         total = c(col_som, sum(col_som)))
      res <- round(res, 4) * 100
      row.names(res)<-c(paste("Cluster",1:input$nbclust),"Total")
    } else if (input$tableGroupVarFiniChoiceCluster == "row") {
      res <- as.data.frame.matrix(round(lprop(t), 2))
      row.names(res)<-c(paste("Cluster",1:input$nbclust),"Ensemble")
    } else{
      res <- as.data.frame.matrix(round(cprop(t), 2))
      row.names(res)<-c(paste("Cluster",1:input$nbclust),"Total")
    }
    res
  }
  
}, extensions = c('FixedColumns', 'Buttons'), server = FALSE, options = list(
  dom = 'Btipr',
  scrollX = TRUE,
  fixedColumns = list(leftColumns = 1),
  buttons = c('copy', 'csv', 'excel', 'pdf')
))

output$numVarGroupCluster <- DT::renderDataTable({
  req(input$nbclust)
  validate(
    need(
      nrow(unique(data_used()[, c("id", input$choixGroupVarClusterDesc)])) == length(unique(data_used()$id)),
      "
      this variable can't be used as group variable because some indiviudas has more than 1 modality for this variable"
    )
  )
  
  data <- data_with_group_var()
  data <-
    unique(data[, c("id",
                    "res_class_cluster",
                    input$choixGroupVarClusterDesc)])
  if (input$typeVarGroup == "as.numeric") {
    data[, input$choixGroupVarClusterDesc] <-
      as.numeric(data[, input$choixGroupVarClusterDesc])
    d = as.data.frame(matrix(nrow = 0, ncol = 8))
    for (i in 1:input$nbclust) {
      dt <- data[data$res_class_cluster == i, ]
      var <- dt[, input$choixGroupVarClusterDesc]
      q <- quantile(var)
      d <- rbind.data.frame(
        d,
        data.frame(
          Mean = round(mean(var), 2),
          Median = round(q[3], 2),
          Q1 = round(q[2], 2),
          Q3 = round(q[4], 2),
          Min = round(q[1], 2),
          Max = round(q[5], 2),
          Sd = round(sd(var), 2),
          Number = length(var)
        )
      )
    }
    q = quantile(data[, input$choixGroupVarClusterDesc])
    d <- rbind.data.frame(
      d,
      data.frame(
        Mean = round(mean(data[, input$choixGroupVarClusterDesc]), 2),
        Median = round(q[3], 2),
        Q1 = round(q[2], 2),
        Q3 = round(q[4], 2),
        Min = round(q[1], 2),
        Max = round(q[5], 2),
        Sd = round(sd(data[, input$choixGroupVarClusterDesc]), 2),
        Number = length(data[, input$choixGroupVarClusterDesc])
      )
    )
    row.names(d) <- c(paste("Cluster", 1:input$nbclust), "All")
    
    
    d
  }
  
}, extensions = c('FixedColumns', 'Buttons'), server = FALSE, options = list(
  dom = 'Btipr',
  scrollX = TRUE,
  fixedColumns = list(leftColumns = 1),
  buttons = c('copy', 'csv', 'excel', 'pdf')
))



  ##5.Simulate markov chain
  #########################
  
  ###Group Probability
  output$probabilityGp <- renderUI({
    matrixInput(
      "groupProbability",
      "Probability to be in a group",
      value = matrix(
        rep(1 / input$nbComponent, input$nbComponent),
        1,
        input$nbComponent
      ),
      class = "numeric",
      rows = list(names = FALSE),
      cols = list(names = FALSE)
    )
  })
  
  
  ###Transition Mat
  output$listMatrix <- renderUI({
    t <- tagList()
    t <-
      tagList(
        t,
        matrixInput(
          paste("transitionMatMix", 1),
          paste("Transition matrix", 1),
          value = matrix((1 - diag(input$nbStateMix)) /
                           (input$nbStateMix - 1),
                         input$nbStateMix,
                         input$nbStateMix,
                         dimnames = list(NULL, paste(
                           rep("state", input$nbStateMix), c(1:input$nbStateMix)
                         ))
          ),
          class = "numeric",
          rows = list(names = FALSE),
          cols = list(names = TRUE, editableNames =
                        TRUE)
        )
      )
    if(input$nbComponent>1){
    for (i in 2:input$nbComponent) {
      t <-
        tagList(
          t,
          matrixInput(
            paste("transitionMatMix", i),
            paste("Transition matrix", i),
            value = matrix((1 - diag(input$nbStateMix)) /
                             (input$nbStateMix - 1),
                           input$nbStateMix,
                           input$nbStateMix
            ),
            class = "numeric",
            rows = list(names = FALSE),
            cols = list(names = FALSE, editableNames =
                          FALSE)
          )
        )
    }
    }
    t
    
  })
  
  ###Expoential parametre
  output$listLambda <- renderUI({
    t <- tagList()
    for (i in 1:input$nbComponent) {
      t <-
        tagList(
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
  
  ###Initial Law
  output$listInitialLaw <- renderUI({
    t <- tagList()
    for (i in 1:input$nbComponent) {
      t <-
        tagList(
          t,
          matrixInput(
            paste("initialLawMix", i),
            paste("Initial law", i),
            value = matrix(
              rep(1 / input$nbStateMix, input$nbStateMix),
              1,
              input$nbStateMix
            ),
            class = "numeric",
            rows = list(names = FALSE),
            cols = list(names = FALSE)
          )
        )
    }
    t
  })
  
  
  ##Mix model Data
  mixModelData <- reactive({
    input$SimulateMixtureModel
    isolate({
      p <-
        sample(
          c(1:input$nbComponent),
          input$nbSimuMix,
          replace = TRUE,
          prob = as.vector(input$groupProbability)
        )
      r <- table(p)
      resData = data.frame()
      d <-
        generate_Markov(
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
        d <-
          generate_Markov(
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
  
  ##Data frame output
  output$headSimulatedMix <- renderDataTable({
    mixModelData()
  }, extensions = 'Buttons', server = FALSE, options = list(
    dom = 'Brtip',
    scrollX = TRUE,
    buttons = c('copy', 'csv', 'excel', 'pdf'),
    rownames = FALSE
  ))
  
  ##Traj output
  output$trajSimulatedMix <- renderPlot({
    gp <- mixModelData() %>% distinct(id, .keep_all = TRUE)
    plotData(
      mixModelData()[, -4],
      group = gp$Component,
      addId = FALSE,
      addBorder = FALSE,
      sort = TRUE
    )
  })
  
})
