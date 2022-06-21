
library(shiny)
library(plotly)
library("shinyMatrix")
if (interactive()) {
  shinyUI(
    navbarPage(
      title = "CFDA",id="navBar",
      ##########################################
      ## Part 1: Import and visualize data    ##
      ##########################################
      tabPanel(title = "Import and visualize data",
               sidebarPanel(
                 wellPanel(
                   fileInput("file1", "Import file (file must have a least 3 column named id, time et state) ",multiple = FALSE,
                             accept = c("text/csv",
                                        ".csv")),
                   radioButtons(inputId = "sep", label = "separator character",
                                choices = c("semi colon" = ";", "tabulation" = "\t","space"=" ","comma"=',')),
                   radioButtons(inputId = "dec", label = "decimal separator",
                                choices = c("comma" = ",", "dot" = ".")),
                   
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
                                                     downloadButton("downloa")
                                                     actionButton("applyMod","apply"),
                                                     
                                                     
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
                                                                  
                                                                  textInput("jumpTitle", "title", value = "number of jumps"),
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
                                                         textInput("timeStateTitle", "title", value = "time spent in each state"),
                                                         
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
                                                     checkboxInput("ribbon","ribbon plot"),
                                                   ),
                                          )
               )       
              )
      ),
      conditionalPanel("!output.fileUploaded && !output.filetest",
                       h4("Error in import, please check data set format (column name, delimiter, decimal separator"))
      ),
      navbarMenu("Analysis",
                 tabPanel("Factorial Analysis",
                          conditionalPanel("output.fileUploaded", 
                                           sidebarPanel(
                                              wellPanel(
                                               checkboxInput(inputId = "optionsACM", label = "show factorial analysis options",TRUE),
                                               conditionalPanel("input.optionsACM",
                                                                selectizeInput("typeBasis","select a type of basis",choice=c("spline"="spline","fourier"="fourier"),selected="spline"),
                                                                
                                                                
                                                                numericInput("nbasis","Nombre de basis",min=1, value=10, max=20),
                                                                conditionalPanel("input.typeBasis=='spline'",
                                                                numericInput("norder","Degres des splines",min=2,max=10, value=4)),
                                                                uiOutput("max"),
                                                               
                                                                conditionalPanel("output.fmcaUploaded",
                                                                downloadButton("downloadResultsCFDA")),
                                                                actionButton("soumettre","Soumettre")
                                               ))
                                              )
                                           ,
                                           mainPanel(
                                             conditionalPanel("output.fmcaUploaded",
                                              tabsetPanel(
                                                tabPanel("data used for factorial analysis",
                                                         dataTableOutput("headDataCFDA"),
                                                         verbatimTextOutput("summaryDataCFDA")
                                                ),   
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
                                                      
                                                      wellPanel(
                                                      h4("Factorial plan"),
                                                      fluidRow(
                                                        column(10,
                                                        plotlyOutput("planfact")),
                                                        column(2,
                                                        uiOutput("dim1"),
                                                        uiOutput("dim2"),
                                                        uiOutput("groupVarFactorialPlan"),
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
                                                        downloadButton("downloadPlotDataExtremAxe2"),
                                                      )
                                                      )
                                             )),
                          
                          )),
                          conditionalPanel("!output.fileUploaded",
                                           h3("You need to import file to use this fonctionnality!!"))),
                 tabPanel("Clustering",
                          conditionalPanel("output.fmcaUploaded",
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
                                                                conditionalPanel("input.clusteringSubmit>0",
                                                                downloadButton("downloadResultsCAH")),
                                                                actionButton("clusteringSubmit","submit"),
                                                               
                                                                
                                              )
                                             )
                                             
                                           ),
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
                                                        downloadButton("downloadGroupPlot")))),
                                                        
                                               
                                             
                                           ))
                                           
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
  )
}