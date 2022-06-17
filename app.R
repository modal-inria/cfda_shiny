library(shiny)
library(cfda)
library(shinyMatrix)
library(tidyverse)
library(tractor.base)
library(dplyr)
library(aricode)
library(plotly)

# Define UI for application that draws a histogram
ui <- navbarPage(
    title = "CFDA",id="navBar",
    ##########################################
    ## Part 1: Import and visualize data    ##
    ##########################################
    tabPanel(title = "Import and visualize data",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   fileInput("file1", "Import file (file must have a least 3 column named id, time et state) ",multiple = FALSE,
                             accept = c("text/csv",
                                        ".csv")),
                   radioButtons(inputId = "sep", label = "separator character",
                                choices = c("semi colon" = ";", "tabulation" = "\t","space"=" ","comma"=',')),
                   radioButtons(inputId = "dec", label = "decimal separator",
                                choices = c("comma" = ",", "dot" = "."))
                 ),
                 conditionalPanel("output.fileUploaded",
                                  
                                  wellPanel(
                                    checkboxInput("plotDataOptions","data selection options",value=T),
                                    conditionalPanel("input.plotDataOptions",
                                                     fluidRow(
                                                       column(4,hr(),
                                                              h5("length of trajectories"),
                                                              radioButtons("filterChoiceLength","",choices = c("All available"="1","Filter"="2"),selected = "1"),
                                                              conditionalPanel("input.filterChoiceLength=='2'",
                                                                               
                                                                               uiOutput("lengthInt")
                                                                               
                                                              )),
                                                       column(4, hr(),
                                                              h5("number of jump"),
                                                              radioButtons("filterChoiceJump","",choices = c("All available"="1","Filter"="2"),selected = "1"),
                                                              conditionalPanel("input.filterChoiceJump=='2'",
                                                                               uiOutput("jumpInt")
                                                                               
                                                              )),
                                                       column(4, hr(),
                                                              h5("percentage"),
                                                              radioButtons("filterChoicePercentage","",choices = c("All available"="1","Filter"="2"),selected = "1"),
                                                              conditionalPanel("input.filterChoicePercentage=='2'",
                                                                               sliderInput("nb_ind", "percentage of first individuals who verify conditions", min = 5,100,value=100,step=5),
                                                              ))),
                                                     #textOutput("test"),
                                                     actionButton("applyMod","apply")
                                                     
                                                     
                                    )
                                  )
                                  
                 )
               ),
               mainPanel(conditionalPanel("output.fileUploaded",
                                          tabsetPanel(
                                            tabPanel("Data", dataTableOutput("head")),
                                            tabPanel("Plot of data",
                                                     fluidRow(
                                                       column(9,
                                                              wellPanel(
                                                                h4("Plot of data"),
                                                                plotOutput("traj")
                                                              )
                                                       ),
                                                       column(3, 
                                                              
                                                              h5("Graph options"),
                                                              checkboxInput("addId","Add id labels in the graph",FALSE),
                                                              checkboxInput("addBorder","Add border in graph",FALSE),
                                                              checkboxInput("sort","sort plot"),
                                                              
                                                              textInput("plotDataTitle","title","Trajectories of the Markov process"),
                                                              uiOutput("groupVar"),
                                                              actionButton("modPlotData","update plot data"),
                                                              downloadButton('downloadPlotTraj', 'Download Plot')
                                                              
                                                       )
                                                     )
                                                     
                                                     ,
                                                     wellPanel(
                                                       h4("Summary of data"),
                                                       verbatimTextOutput("Resume")
                                                     )
                                            ),
                                            tabPanel("Basics Statistics of data",
                                                     fluidRow(
                                                       column(6,wellPanel(
                                                         plotlyOutput("durationData"),
                                                         checkboxInput("summaryDurationOption","Summary of duration (mean, median, ect)"),
                                                         conditionalPanel("input.summaryDurationOption",
                                                                          tableOutput("summaryDuration")),
                                                         checkboxInput("durationTitleOption","Update the title of histogram"),
                                                         conditionalPanel("input.durationTitleOption",
                                                                          fluidRow(
                                                                            textInput("durationTitle", "title", value = "Duration of trajectories")
                                                                            
                                                                          ),
                                                                          actionButton("updateDuration","update title"))
                                                       )
                                                       ),
                                                       column(6,
                                                              wellPanel(
                                                                plotlyOutput("jumpData"),
                                                                checkboxInput("summaryJumpOption","Summary of jump (mean, median, ect)"),
                                                                conditionalPanel("input.summaryJumpOption",
                                                                                 tableOutput("summaryJump")),
                                                                checkboxInput("jumpTitleOption","Update the title of histogram"),
                                                                conditionalPanel("input.jumpTitleOption",
                                                                                 fluidRow(
                                                                                   
                                                                                   textInput("jumpTitle", "title", value = "number of jumps")
                                                                                 ),
                                                                                 actionButton("updateJump","update title"))
                                                                
                                                              )
                                                       )
                                                     ),
                                                     wellPanel(
                                                       h4("Time spent in each state"),
                                                       plotlyOutput("timeSpentData"),
                                                       checkboxInput("timeStateTitleOption","Update the title of histogram"),
                                                       conditionalPanel("input.timeStateTitleOption",
                                                                        fluidRow(
                                                                          textInput("timeStateTitle", "title", value = "time spent in each state")
                                                                          
                                                                        ),
                                                                        actionButton("updateTimeState","update title"))
                                                       
                                                     )
                                            ),
                                            tabPanel("estimation of markov chain",
                                                     wellPanel(
                                                       checkboxInput("transitionMatDataOption","show transition matrix"),
                                                       conditionalPanel("input.transitionMatDataOption",
                                                                        verbatimTextOutput("transitionMatData")),
                                                       checkboxInput("lambdaDataOption","show parameter of exponentiel sejourn time"),
                                                       conditionalPanel("input.lambdaDataOption",
                                                                        verbatimTextOutput("lambdaDataData")
                                                       ),
                                                       checkboxInput("numberJumpDataTableOption","show number of jump"),
                                                       conditionalPanel("input.numberJumpDataTableOption",
                                                                        verbatimTextOutput("numberJumpDataTable")
                                                       )
                                                     ),
                                                     wellPanel(
                                                       plotOutput("graphData"),
                                                       checkboxInput("graphOption","transition graph options"),
                                                       conditionalPanel("input.graphOption",
                                                                        textInput("graphTitle", "title", value = "transition graph"),
                                                                        actionButton("updateGraphTitle","update title"),
                                                                        downloadButton('downloadPlotGraph', 'Download Plot'))
                                                     ),
                                                     wellPanel(
                                                       h3("Probability to be in a state regarding time"),
                                                       plotlyOutput("probaStateData"),
                                                       checkboxInput("ribbon","ribbon plot")
                                                     )
                                            )
                                          )       
               )
               )),
             conditionalPanel("!output.fileUploaded && !output.filetest",
                              h4("Error in import, please check data set format (column name, delimiter, decimal separator"))
    ),
    navbarMenu("Analysis",
               tabPanel("Factorial Analysis",
                        conditionalPanel("output.fileUploaded", 
                                         sidebarLayout(
                                           sidebarPanel(
                                             wellPanel(
                                               checkboxInput(inputId = "optionsACM", label = "show factorial analysis options",TRUE),
                                               conditionalPanel("input.optionsACM",
                                                                selectizeInput("typeBasis","select a type of basis",choice=c("spline"="spline","fourier"="fourier"),selected="spline"),
                                                                
                                                                
                                                                numericInput("nbasis","Nombre de basis",min=1, value=10, max=20),
                                                                conditionalPanel("input.typeBasis=='spline'",
                                                                                 numericInput("norder","Degres des splines",min=2,max=10, value=4)),
                                                                uiOutput("max"),
                                                                
                                                                
                                                                
                                                                actionButton("soumettre","Soumettre")
                                               ))
                                           )
                                           
                                           ,
                                           mainPanel(
                                             conditionalPanel("output.fmcaUploaded",
                                                              tabsetPanel(
                                                                tabPanel("eigenvalues",
                                                                         wellPanel(plotlyOutput("valeurspropres")),
                                                                         wellPanel(
                                                                           fluidRow(
                                                                             column(7,verbatimTextOutput("eigenvaluesTable")),
                                                                             column(5,
                                                                                    h4("Eigenvalues plot options"),
                                                                                    checkboxInput("cumulative","cumulative"),
                                                                                    checkboxInput("normalize","normalize eigenvalues")
                                                                             )
                                                                           )
                                                                         )),
                                                                tabPanel("factorial plan", 
                                                                         
                                                                         
                                                                         h4("Factorial plan"),
                                                                         fluidRow(
                                                                           wellPanel(
                                                                             column(10,
                                                                                    plotlyOutput("planfact")),
                                                                             column(2,
                                                                                    uiOutput("dim1"),
                                                                                    uiOutput("dim2"),
                                                                                    checkboxInput("addCI","add confidence interval")))),
                                                                         fluidRow(
                                                                           h4("Optimal encoding function"),
                                                                           column(6 ,
                                                                                  wellPanel(
                                                                                    plotOutput("optimalEncoding1"),
                                                                                    downloadButton("saveOptimalEncoding1")
                                                                                  )),
                                                                           column(6,
                                                                                  wellPanel(
                                                                                    plotOutput("optimalEncoding2"),
                                                                                    downloadButton("saveOptimalEncoding2")
                                                                                  )
                                                                                  
                                                                           ))
                                                                ),
                                                                tabPanel("extrem individuals",
                                                                         
                                                                         wellPanel(
                                                                           fluidRow(
                                                                             column(9,
                                                                                    plotlyOutput("planfactExtremeIndividuals")),
                                                                             column(3,
                                                                                    sliderInput("extremComp1","extreme individuals axe 1",min=0, max=50, value=10),
                                                                                    sliderInput("extremComp2","extreme individuals axe 2",min=0, max=50, value=10),
                                                                                    actionButton("submitExtrem","submit")
                                                                             ))),
                                                                         wellPanel(
                                                                           h4("axe 1"),
                                                                           plotOutput("plotDataExtremAxe1"),
                                                                           downloadButton("downloadPlotDataExtremAxe1"),
                                                                           h4("axe 2"),
                                                                           plotOutput("plotDataExtremAxe2"),
                                                                           downloadButton("downloadPlotDataExtremAxe2")
                                                                         )
                                                                )
                                                              ))
                                             
                                           ))),
                        conditionalPanel("!output.fileUploaded",
                                         h3("You need to import file to use this fonctionnality!!"))),
               tabPanel("Clustering",
                        conditionalPanel("output.fmcaUploaded",
                                         sidebarLayout(
                                           sidebarPanel(
                                             wellPanel(
                                               # checkboxInput(inputId = "optionsClustering", label = "show clustering options"),
                                               #conditionalPanel("input.optionsClustering",
                                               radioButtons(inputId = "compsCAH", label = "Select one",
                                                            choices = c("Number of component" = 1, "percentage of variance" = 2),
                                                            selected = 2),
                                               conditionalPanel("input.compsCAH==1",
                                                                uiOutput("nb_comp")),
                                               conditionalPanel("input.compsCAH==2",
                                                                sliderInput("percentageVariance","select a percentage of variance", min=5, max=100, value=90)
                                               ),
                                               selectizeInput("method","select a method",selected="ward.D2",choice=c("ward.D2"="ward.D2","ward.D"="ward.D","single"="single",
                                                                                                                     "complete"="complete","average"="average","centroid"="centroid","median"="median","mcquitty"="mcquitty")),
                                               actionButton("clusteringSubmit","submit")
                                               
                                               
                                             )
                                           )
                                           
                                           ,
                                           mainPanel(conditionalPanel("input.clusteringSubmit>0",
                                                                      wellPanel(
                                                                        h4("dendogramm"),
                                                                        fluidRow(
                                                                          column(10,plotOutput("dendogramme")),
                                                                          column(2,checkboxInput(inputId = "couper", label = "cut dendogram?"),
                                                                                 downloadButton("downloadDendogram")))),
                                                                      wellPanel(
                                                                        h4("Cluster"),
                                                                        fluidRow(
                                                                          column(10,plotOutput("groupe")),
                                                                          column(2,
                                                                                 uiOutput("clus"),
                                                                                 downloadButton("downloadGroupPlot"))))
                                                                      
                                                                      
                                                                      
                                           ))))
                        
                        ,
                        conditionalPanel("!output.fmcaUploaded",
                                         h1("You need to submit factortial analysis before use clustering analysis"))
               )
    ),
    navbarMenu("Help",
               tabPanel("import and visialize data"),
               tabPanel("analysis"),
               tabPanel("Simulate mixture model")
    ),
    tabPanel(title = "Quit", value="stop", icon = icon("circle-o-notch"))
  )

# Define server logic required to draw a histogram
server <- function(input, output) {

  ## quit app
  observe({
    if (input$navBar == "stop") 
      stopApp()
  })
  ##########################################
  ## Part 1: Import and visualize data    ##
  ##########################################
  
  ##Data selection option#
  ########################
  ##filter on trajectories lenght
  output$lengthInt<-renderUI({
    validate(
      need(is.numeric(data_used()[,"time"]), 'time must be numeric, please choose correct decimal symbol'),
    )
    tagList(
      numericInput("lower","Indiduals observed between time",min=summary_cfd(data_import())$timeRange[1],max=summary_cfd(data_import())$timeRange[2], value=summary_cfd(data_import())$timeRange[1]),
      numericInput("upper","and time", min=1,max=floor(summary_cfd(data_import())$timeRange[2]),value=floor((summary_cfd(data_import())$timeRange[2]-summary_cfd(data_import())$timeRange[1])/2))
    )
  })
  
  ##filter on number of jump
  output$jumpInt<-renderUI({
    tagList(
      numericInput("more","Indiduals with more than ",min=0,value=0),
      numericInput("less","and less than", min=0, value=max(nJump()),max=max(nJump()))
    )
  })
  
  ##Number of jump of initial dataset
  nJump <- reactive({
    compute_number_jumps(data_import()[,c("id","state","time")], countDuplicated = FALSE)
  })
  
  
  #1. DATA#
  ##########
  data_import<-reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == 'csv', "Please upload a csv file"))
    data<-read.csv(file$datapath, header = T,sep=input$sep,dec=input$dec)
    
  })
  
  output$filetest<-reactive({
    return(is.null(input$file1$datapath))
  })
  
  
  outputOptions(output, 'filetest', suspendWhenHidden=FALSE)
  ##Fiter individuals
  data_used<-reactive({
    
    # validate(
    # need(sum(colnames(data_import()) %in% "id")==1,"data frame must have exatly one column name 'id'"),
    # need(sum(colnames(data_import()) %in% "time")==1,"data frame must have exatly one column name 'time'"),
    # need(sum(colnames(data_import()) %in% "state")==1,"data frame must have exatly one column name 'state'")
    #  )
    if(sum(colnames(data_import()) %in% "id")!=1 | sum(colnames(data_import()) %in% "time")!=1 |sum(colnames(data_import()) %in% "state")!=1){
      validate("Error! data must have exatly one column named 'id', one column named 'state' and one column name 'time'")
    }
    validate(
      need(is.numeric(data_import()[,"time"]), 'time must be numeric, please choose correct decimal symbol'),
      
    )
    #output$test<-renderText({
    #c(input$upper)
    #})
    data<-data_import()
    input$applyMod
    isolate({
      
      if(input$filterChoiceLength=='2'){
        minT<-data %>% 
          group_by(id) %>%  filter(time== min(time, na.rm = TRUE)) 
        maxT<-data %>% 
          group_by(id) %>%  filter(time== max(time, na.rm = TRUE)) 
        
        if(is.na(input$upper) & is.na(input$lower)){
          idToKeep<-minT$id
        }else if(is.na(input$upper) & !is.na(input$lower)){
          idToKeep<-minT[minT$time<=input$lower,"id"]$id
        }else if(!is.na(input$upper) & is.na(input$lower)){
          idToKeep<-maxT[maxT$time>=input$upper,"id"]$id
        }else{
          idToKeep<-intersect(minT[minT$time<=input$lower,"id"], maxT[maxT$time>=input$upper,"id"])$id
        }
        data<-data[data$id %in% idToKeep,]
      }
      
      if(input$filterChoiceJump=='2'){
        # if(input$more==input$less){
        # idToKeep<-names(nJump()[nJump()==input$more])
        # data<-data[data$id %in% idToKeep,]
        # }else{
        idToKeep<-names(nJump()[nJump()>=input$more & nJump()<=input$less])
        data<-data[data$id %in% idToKeep,]
        # }
      }
      
      if(input$filterChoicePercentage=='2'){
        idInd<-sort(unique(data$id))
        idToKeep<-idInd[1:floor(length(idInd)*input$nb_ind/100)]
        data<-data[data$id %in% idToKeep,] 
      }
    })
    data
    
  })
  
  
  ##verify if a dataset exist
  output$fileUploaded <- reactive({
    return(!is.null(data_used()))
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  ##show data set
  output$head <- renderDataTable({
    data_used()
  })
  
  #2. PLOT OF DATA#
  #################
  
  output$groupVar<-renderUI({
    var<-which(!colnames(data_used()) %in% c("id","time","state"))
    choice=colnames(data_used())[var]
    selectizeInput("groupVariable","select a group variable",choice=c("NONE",choice))
  })
  
  ##Plot of trajectories
  plotOfData<-reactive({
    validate(
      need(is.numeric(data_used()[,"time"]), 'time must be numeric, please choose correct decimal symbol'),
      
    )
    withProgress(message = 'making plot of individuals trajectories',detail='please wait until the end', value = 0, {
      incProgress(1/4)
      p<-plotData(data_used()[,c("id","time","state")],addId=FALSE,addBorder=FALSE)+labs(title = "Trajectories of the Markov process")
      input$modPlotData
      isolate({
        if(is.null(input$groupVariable)){
          class<-NULL
        }else{
          if(input$groupVariable!="NONE"){
            r<-unique(data_used()[,c("id",input$groupVariable)])
            class<-r[,input$groupVariable] 
          }else{
            class<-NULL
          }
        }
        p<-plotData(data_used()[,c("id","time","state")],group=class,addId=input$addId,addBorder=input$addBorder,sort=input$sort)+labs(title = input$plotDataTitle)
      })
      for (i in 1:3) {
        incProgress(1/4)
        Sys.sleep(0.10)
      }
      p
    })
  })
  
  output$traj<-renderPlot({
    plotOfData()   
  })
  
  
  ##Summary
  output$Resume<-renderPrint({
    input$applyMod
    isolate({
      #if(is.null(input$groupVariable)){
      #summary_cfd(data_used()[,c("id","time","state")])
      # }else{
      #  if(input$groupVariable!="NONE"){
      # r<-unique(data_used()[,c(input$groupVariable)])
      #for(i in 1:length(r)){
      #  print(h4(paste("Groupe :",r[i])))
      # summary_cfd(data_used()[data_used()[,input$groupVariable]==r[i],c("id","time","state")])
      # }
      #}else{
      summary_cfd(data_used()[,c("id","time","state")])
      #}}
    })
  })
  
  ##Download data plot
  output$downloadPlotTraj = downloadHandler(
    filename =  function() {
      paste("plot", "png", sep=".")
    },
    content = function(file) {
      ggsave(file, plotOfData())
    } 
  )
  
  #3. BASICS STATISTICS#
  ######################
  
  #Compute duration of individuals
  duration<-reactive({
    validate(
      need(is.numeric(data_used()[,"time"]), 'time must be numeric, please choose correct decimal symbol'),
      
    )
    duration<-compute_duration(data_used()[,c("id","time","state")])
  })
  
  ##histogram of trajectories durations
  output$durationData<-renderPlotly({
    withProgress(message = 'making plot of duration',detail='please wait until the end', value = 0, {
      incProgress(1/4)
      histo<-hist(duration())+labs(title = "duration of trajectories")
      input$updateDuration
      isolate({
        histo<- hist(duration(),breaks = input$binsDuration)+labs(title = input$durationTitle)
      })
      for (i in 1:3) {
        incProgress(1/4)
        Sys.sleep(0.10)
      }
      histo
    })
  })
  
  
  #table with mean, median, ect of duration
  output$summaryDuration<-renderTable({
    withProgress(message = 'compute statitics of duration',detail='please wait until the end', value = 0, {
      incProgress(1/4)
      q<-quantile(duration(),seq(0,1,0.25))
      d<-data.frame(mean=round(mean(duration()),2),
                    sd=round(sd(duration()),2),
                    Q1=round(q[2],2),
                    median=round(q[3],2),
                    Q3=round(quantile(q[4],seq(0,1,0.25))[3],2)
      )
      d
    })
  })
  
  ##Number of jump of the dataset used (with filters applied)
  nJump_dataUsed<-reactive({
    nJump <- compute_number_jumps(data_used()[,c("id","time","state")], countDuplicated = FALSE)
  })
  
  ##Histogramm of number of jump
  output$jumpData<-renderPlotly({
    withProgress(message = 'making plot of number of jumps',detail='please wait until the end', value = 0, {
      incProgress(1/4)
      histo<-hist(nJump_dataUsed())+labs(title = "number of jumps")
      input$updateJump
      isolate({
        histo<- hist(nJump_dataUsed(),breaks = input$binsJump)+labs(title = input$jumpTitle)
      })
      for (i in 1:3) {
        incProgress(1/4)
        Sys.sleep(0.10)
      }
      histo
    })
  })
  
  #table with mean, median, ect of jump
  output$summaryJump<-renderTable({
    withProgress(message = 'compute statitics of jump',detail='please wait until the end', value = 0, {
      incProgress(1/4)
      q<-quantile(nJump_dataUsed(),seq(0,1,0.25))
      d<-data.frame(mean=round(mean(nJump_dataUsed()),2),
                    sd=round(sd(nJump_dataUsed()),2),
                    min=round(q[1],2),
                    Q1=round(q[2],2),
                    median=round(q[3],2),
                    Q3=round(q[4],2),
                    max=round(q[5],2)
      )
      d
    })
  })
  
  
  ##boxplot of time spent in each state
  output$timeSpentData<-renderPlotly({
    validate(
      need(is.numeric(data_used()[,"time"]), 'time must be numeric, please choose correct decimal symbol'),
      
    )
    withProgress(message = 'making plot of time spent in each state',detail='please wait until the end', value = 0, {
      incProgress(1/4)
      timeSpent <- compute_time_spent(data_used()[,c("id","time","state")])
      b<-boxplot(timeSpent)+labs(title="time spent in each state")
      input$updateTimeState
      isolate({
        b<-boxplot(timeSpent)+labs(title=input$timeStateTitle) 
      })
      for (i in 1:3) {
        incProgress(1/4)
        Sys.sleep(0.10)
      }
      b
    })
  })
  
  #4. MARKOV CHAIN
  
  
  ##estimate markov
  
  estimateMarkov <-reactive({
    validate(
      need(is.numeric(data_used()[,"time"]), 'time must be numeric, please choose correct decimal symbol'),
      
    )
    withProgress(message = 'making transition graph',detail='please wait until the end', value = 0, {  
      p<-estimate_Markov(data_used()[,c("id","time","state")])
      for (i in 1:4) {
        incProgress(1/4)
        Sys.sleep(0.10)
      }
    })
    p
  })
  
  
  ##transition graph
  output$graphData <-renderPlot({
    plot(estimateMarkov(), main = input$graphTitle)
  })
  
  
  ##Transition matrix
  output$transitionMatData<-renderText({
    
  })
  
  ##Exponentiel law
  output$lambdaDataData<-renderText({
    
  })
  
  ##number of jump
  output$numberJumpDataTable<-renderText({
    
  })
  
  ##Plot of probability to be in a state
  output$probaStateData<-renderPlotly({
    validate(
      need(is.numeric(data_used()[,"time"]), 'time must be numeric, please choose correct decimal symbol'),
      
    )
    withProgress(message = 'compute probability',detail='please wait until the end', value = 0, {
      proba <- estimate_pt(data_used()[,c("id","time","state")])
      for (i in 1:3) {
        incProgress(1/4)
        Sys.sleep(0.10)
      }
      incProgress(1/4)
      plot(proba,ribbon = input$ribbon)
    })
  })
  
  
  
  ##Download transition graph
  output$downloadPlotGraph = downloadHandler(
    filename =  function() {
      paste("transition graph", "png", sep=".")
    },
    content = function(file) {
      ggsave(file, transitionGraph(),device="png")
    } 
  )
  
  ##########################################
  ## Part 2: Analysis                     ##
  ##########################################
  
  ##1. CFDA Analysis
  
  ##Data used for factorial analysis 
  data_CFDA<-reactive({
    input$soumettre
    isolate({
      idToKeep<-names(duration()[duration()>=input$tpsmax])
      cut_data(data_used()[data_used()$id %in% idToKeep,c("id","time","state")],input$tpsmax)
    })
  })
  
  fmca<-reactive({
    input$soumettre
    validate(
      need(is.numeric(data_used()[,"time"]), 'time must be numeric, please choose correct decimal symbol'),
      
    )
    isolate({
      withProgress(message = 'Compute principal component',detail='please wait until the end', value = 0, {
        incProgress(2/15)
        Sys.sleep(0.25)
        set.seed(42)
        if(input$typeBasis=='spline'){
          basis<-create.bspline.basis(c(0, input$tpsmax), nbasis = input$nbasis, norder = input$norder)
        }else{
          basis<-create.fourier.basis(c(0, input$tpsmax), nbasis = input$nbasis)
        }
        fmca <- compute_optimal_encoding(data_CFDA(), basis)
        for (i in 3:15) {
          incProgress(1/15)
          Sys.sleep(0.25)
        }
        fmca
      })
    })
  })
  
  output$fmcaUploaded <- reactive({
    return(!is.null(fmca()))
  })
  
  outputOptions(output, 'fmcaUploaded', suspendWhenHidden=FALSE)
  
  
  output$dim1<-renderUI({
    
    input$soumettre
    maxi<-isolate(input$nbasis*length(summary_cfd(data_CFDA()[,c("id","state","time")])$states))
    selectInput(inputId = "choix_dim1", label = "axe 1", selected = 1,
                choices = seq(1,maxi,1), multiple = FALSE)
    
  })
  
  output$dim2<-renderUI({
    
    input$soumettre
    maxi<-isolate(input$nbasis*length(summary_cfd(data_CFDA()[,c("id","state","time")])$states))
    selectInput(inputId = "choix_dim2", label = "axe 2 ", selected = 2,
                choices = seq(1,maxi,1), multiple = FALSE)
    
    
  })
  
  output$planfact<-renderPlotly({
    
    plotComponent(fmca(), comp = c(as.numeric(input$choix_dim1), as.numeric(input$choix_dim2)), addNames = FALSE)
    
  })
  
  output$valeurspropres<-renderPlotly({
    
    if(input$cumulative){
      ggplot(cbind.data.frame(x=1:nrow(eigenvalues()),y=eigenvalues()[,3]))+ggtitle("eigenvalues plot") +
        theme(plot.title = element_text(hjust =0.5)) + scale_x_continuous(breaks=1:nrow(eigenvalues())) +
        geom_col(fill="blue") + aes(x=x, y=y)+ 
        xlab("component") + ylab("Percentage of variance") 
      
    }else{
      ggplot(cbind.data.frame(x=1:nrow(eigenvalues()),y=eigenvalues()[,2]))+ggtitle("cumulative eigenvalues plot") +
        theme(plot.title = element_text(hjust =0.5)) + scale_x_continuous(breaks=1:nrow(eigenvalues())) +
        geom_col(fill="blue") + aes(x=x, y=y)+ 
        xlab("component") + ylab("Percentage of variance") 
    }
    
  })
  
  output$max<-renderUI({
    validate(
      need(is.numeric(data_used()[,"time"]), 'time must be numeric, please choose correct decimal symbol  for compute principal component'),
      
    )
    min<-summary_cfd(data_used()[,c("id","state","time")])$timeRange[1]
    max<-summary_cfd(data_used()[,c("id","state","time")])$timeRange[2]
    if(is.null(input$upper)){
      numericInput("tpsmax","Temps max des individus",min=min,max=max,value=floor((max-min)/2)) 
    }else{
      if(is.na(input$upper)){
        numericInput("tpsmax","Temps max des individus",min=min,max=max,value=floor((max-min)/2))
      }else{
        numericInput("tpsmax","Temps max des individus",min=min,max=max,value=input$upper)
      }
    }
    
  })
  
  eigenvalues<-reactive({
    vpselection<-which(round(fmca()$eigenvalues,4)>0)
    vp<-cbind.data.frame(
      eigenvalues=round(fmca()$eigenvalues[vpselection],4),
      "percentage of variance"=round(fmca()$eigenvalues[vpselection]/sum(fmca()$eigenvalues[vpselection])*100,2),
      "cumulative percentage of variance"=round(cumsum(fmca()$eigenvalues[vpselection])/sum(fmca()$eigenvalues[vpselection])*100,2))
    rownames(vp)<-paste("dim",1:length(fmca()$eigenvalues[vpselection]))
    vp
  })
  
  output$eigenvaluesTable<-renderPrint({
    eigenvalues()
  })
  
  
  optimalEncodingPlot1<-reactive({
    
    plot(fmca(),harm=as.numeric(input$choix_dim1),addCI=input$addCI)
    
  })
  output$optimalEncoding1<-renderPlot({
    optimalEncodingPlot1()
  })
  
  optimalEncodingPlot2<-reactive({
    
    plot(fmca(),harm=as.numeric(input$choix_dim2),addCI=input$addCI)
    
  })
  output$optimalEncoding2<-renderPlot({
    optimalEncodingPlot2()
  })
  
  
  output$saveOptimalEncoding1 = downloadHandler(
    filename =  function() {
      paste("plotOptimalEncoding", "png", sep=".")
    },
    content = function(file) {
      ggsave(file, optimalEncodingPlot1())
    } 
  )
  
  output$saveOptimalEncoding2 = downloadHandler(
    filename =  function() {
      paste("plotOptimalEncoding", "png", sep=".")
    },
    content = function(file) {
      ggsave(file, optimalEncodingPlot2())
    } 
  )
  
  
  ### Extreme individuals
  
  extremIndividuals<-reactive({
    minpc1 <- names(which(fmca()$pc[,as.numeric(input$choix_dim1)] <= quantile(fmca()$pc[,as.numeric(input$choix_dim1)], input$extremComp1/100)))
    maxpc1 <- names(which(fmca()$pc[,as.numeric(input$choix_dim1)] >= quantile(fmca()$pc[,as.numeric(input$choix_dim1)], 1-(input$extremComp1/100))))
    minpc2 <- names(which(fmca()$pc[,as.numeric(input$choix_dim2)] <= quantile(fmca()$pc[,as.numeric(input$choix_dim2)], input$extremComp2/100)))
    maxpc2 <- names(which(fmca()$pc[,as.numeric(input$choix_dim2)] >= quantile(fmca()$pc[,as.numeric(input$choix_dim2)], 1-(input$extremComp2/100))))
    list(minpc1=minpc1,maxpc1=maxpc1,minpc2=minpc2,maxpc2=maxpc2)
  })
  
  output$planfactExtremeIndividuals<-renderPlotly({
    
    ids <- unique(data_CFDA()$id)
    group <- factor(rep("not extrem", length(ids)), levels = c("extrem on axe 2","extrem on axe 1","not extrem","extrem on both axes"))
    group[ids %in% extremIndividuals()$minpc1] = "extrem on axe 1"
    group[ids %in% extremIndividuals()$maxpc1] = "extrem on axe 1"
    group[ids %in% extremIndividuals()$minpc2] = "extrem on axe 2"
    group[ids %in% extremIndividuals()$maxpc2] = "extrem on axe 2"
    group[ids %in% intersect(extremIndividuals()$minpc1,extremIndividuals()$minpc2)]="extrem on both axes"
    group[ids %in% intersect(extremIndividuals()$maxpc1,extremIndividuals()$maxpc2)]="extrem on both axes"
    p<-plotComponent(fmca(),comp=c(as.numeric(input$choix_dim1),as.numeric(input$choix_dim2)),addNames = FALSE)+
      geom_point(aes(color=group))+scale_color_manual(values=c("#000CFF", "#FF0000", "#000000","#7000FF"))
    p
    
  })
  
  plotDataExtremAxe1<-reactive({
    ids <- unique(data_CFDA()$id)
    group <- factor(rep(NA, length(ids)), levels = c("lowest component values","highest component values"))
    group[ids %in% extremIndividuals()$minpc1] = "lowest component values"
    group[ids %in% extremIndividuals()$maxpc1] = "highest component values"
    plotData(data_CFDA(),group=group,addBorder = F,addId=F)+labs(title=paste("Extreme individuals on component",input$choix_dim1))
    
  })
  
  output$plotDataExtremAxe1<-renderPlot({
    plotDataExtremAxe1()
  })
  
  plotDataExtremAxe2<-reactive({
    ids <- unique(data_CFDA()$id)
    group <- factor(rep(NA, length(ids)), levels = c("lowest component values","highest component values"))
    group[ids %in% extremIndividuals()$minpc2] = "lowest component values"
    group[ids %in% extremIndividuals()$maxpc2] = "highest component values"
    plotData(data_CFDA(),group=group,addBorder = F,addId=F)+labs(title=paste("Extreme individuals on component",input$choix_dim2)) 
  })
  
  
  output$plotDataExtremAxe2<-renderPlot({
    plotDataExtremAxe2()
  })
  
  output$downloadPlotDataExtremAxe1= downloadHandler(
    filename =  function() {
      paste("extremeIndividualsAxe1", "png", sep=".")
    },
    content = function(file) {
      ggsave(file,dendogramme() ,device="png")
    } 
  )
  
  
  output$downloadPlotDataExtremAxe2= downloadHandler(
    filename =  function() {
      paste("extremeIndividualsAxe2", "png", sep=".")
    },
    content = function(file) {
      ggsave(file,dendogramme() ,device="png")
    } 
  )
  
  ##2. Clustering analysis
  
  output$nb_comp<-renderUI({
    input$soumettre
    maxi<-isolate(input$nbasis*length(summary_cfd(data_CFDA()[,c("id","state","time")])$states))
    selectInput(inputId = "nbcomp", label = "number of component for clustering", selected = 1,
                choices = seq(1,maxi,1), multiple = FALSE)
  })
  
  hc <-reactive({
    input$clusteringSubmit
    isolate({
      if(input$compsCAH==1){
        ncomp<-input$nbcomp
        hclust(dist(fmca()$pc[, 1:ncomp]), method = input$method)
      }else{
        if(input$percentageVariance==100){
          hclust(dist(fmca()$pc), method = input$method) 
        }else{
          ncomp<-which(cumsum(prop.table(fmca()$eigenvalues))>=input$percentageVariance/100)[1]
          hclust(dist(fmca()$pc[, 1:ncomp]), method = input$method) 
        }
        
      }
    })
    
  })
  
  
  dendogramme<-reactive({ 
    plot(hc(), labels = FALSE)
    if(is.null(input$nbclust)){
      rect.hclust(hc(), 2, border = "green3")
    }else{
      if(input$couper){
        rect.hclust(hc(), input$nbclust, border = "green3")
      }
    }
  })
  
  output$dendogramme<-renderPlot({ 
    dendogramme()
  })
  
  output$clus<-renderUI({
    numericInput("nbclust","Nombre de groupe",min=2,max=100,value=2)
  })
  
  groupe<-reactive({
    class <- cutree(hc(), k = input$nbclust)
    plotData(data_CFDA(), group = class, addId = FALSE, addBorder = FALSE, sort = TRUE)
    
    
  })
  
  output$groupe<-renderPlot({
    groupe()
  })
  output$downloadDendogram = downloadHandler(
    filename =  function() {
      paste("dendogram", "png", sep=".")
    },
    content = function(file) {
      ggsave(file,dendogramme() ,device="png")
    } 
  )
  
  output$downloadGroupPlot = downloadHandler(
    filename =  function() {
      paste("groupPlot", "png", sep=".")
    },
    content = function(file) {
      ggsave(file, groupe(),device="png")
    } 
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
