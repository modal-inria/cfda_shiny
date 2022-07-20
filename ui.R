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
if (!require('dashboardthemes'))
  install.packages("dashboardthemes")

ui<-dashboardPage(
  dashboardHeader(title = "CFDA"),
  dashboardSidebar(
    width = "300px",
    sidebarMenu(
      style = "font-size:20px;",
      menuItem("Import data", tabName = "importData"),
      menuItem("Visualize data", tabName = "visualizeData"),
      menuItem("Descriptive statistics", tabName = "descriptiveStatistics"),
      menuItem("Estimation of markov chain", tabName = "estimationOfMarkovChain"),
      menuItem(
        "Analysis",
        tabName = "analysis",
        menuSubItem("factorial analysis", tabName = "factorialAnalysis"),
        menuSubItem("clustering", tabName = "clustering")
      ),
      menuItem("Simulate a mixture model", tabName = "simulateMarkov"),
      menuItem("Help", tabName = "help")
    )
  ),
  dashboardBody(style = "height: auto;font-size:20px;",   
                fluidRow(
                  tabItems(
                    tabItem(
                      ##################
                      #1. Import data ##
                      ##################
                      tabName = "importData",
                      box(
                        width = 4,
                        title = "Import dataset and filter data",
                        box(
                          width = 12,
                          fileInput(
                            "file1",
                            "Import file (file must have at least 3 columns named : id, time and state)",
                            multiple = FALSE,
                            accept = c("text/csv", ".csv")
                          ),
                          radioGroupButtons(
                            inputId = "sep",
                            label = "separator",
                            choices = c(
                              "semicolon" = ";",
                              "tabulation" = "\t",
                              "space" = " ",
                              "comma" = ','
                            ),
                            individual = TRUE,
                            checkIcon = list(
                              yes = tags$i(class = "fa fa-circle",
                                           style = "color: steelblue"),
                              no = tags$i(class = "fa fa-circle-o",
                                          style = "color: steelblue")
                            )
                          ),
                          radioGroupButtons(
                            inputId = "dec",
                            label = "decimal",
                            choices = c("comma" = ",", "dot" = "."),
                            individual = TRUE,
                            checkIcon = list(
                              yes = tags$i(class = "fa fa-circle",
                                           style = "color: steelblue"),
                              no = tags$i(class = "fa fa-circle-o",
                                          style = "color: steelblue")
                            )
                          )
                        ),
                        
                        conditionalPanel(
                          "output.fileUploaded",
                          box(
                            width = 12,
                            checkboxInput("plotDataOptions", "data selection options", value =
                                            T),
                            conditionalPanel(
                              "input.plotDataOptions",
                              fluidRow(
                                h3("Data selection"),
                                column(
                                  4,
                                  hr(),
                                  h5("length of trajectories"),
                                  radioGroupButtons(
                                    inputId = "filterChoiceLength",
                                    "",
                                    choices = c("All available" = "1", "Filter" = "2"),
                                    selected = "1",
                                    status = "primary",
                                    checkIcon = list(
                                      yes = icon("ok", lib = "glyphicon"),
                                      no = icon("remove", lib = "glyphicon")
                                    )
                                  ),
                                  conditionalPanel("input.filterChoiceLength=='2'", 
                                          textInput("lower", "Indiduals observed between time", placeholder = "no space between digits and decimal separateur must be the dot"),
                                          textInput("upper", "and time:", placeholder = "no space between digits and decimal separateur must be the dot"))
                                ),
                                column(
                                  4,
                                  hr(),
                                  h5("number of jumps"),
                                  radioGroupButtons(
                                    inputId = "filterChoiceJump",
                                    "",
                                    choices = c("All available" = "1", "Filter" = "2"),
                                    selected = "1",
                                    status = "primary",
                                    checkIcon = list(
                                      yes = icon("ok", lib = "glyphicon"),
                                      no = icon("remove", lib = "glyphicon")
                                    )
                                  ),
                                  conditionalPanel("input.filterChoiceJump=='2'", uiOutput("jumpInt"))
                                ),
                                column(
                                  4,
                                  hr(),
                                  h5("percentage"),
                                  radioGroupButtons(
                                    inputId = "filterChoicePercentage",
                                    "",
                                    choices = c("All available" = "1", "Filter" = "2"),
                                    selected = "1",
                                    status = "primary",
                                    checkIcon = list(
                                      yes = icon("ok", lib = "glyphicon"),
                                      no = icon("remove", lib = "glyphicon")
                                    )
                                  ),
                                  conditionalPanel(
                                    "input.filterChoicePercentage=='2'",
                                    sliderInput(
                                      "nb_ind",
                                      "The percentage of the first individuals who verify conditions",
                                      min = 5,
                                      100,
                                      value = 100,
                                      step = 5
                                    )
                                  )
                                )
                              ),
                              downloadButton("downloadDataSet", "download dataset"),
                              actionButton("applyMod", "apply")
                              
                            )
                          )
                        )
                      ),
                      conditionalPanel(
                        "output.fileUploaded",
                        box(title = "data table", width = 8, DTOutput("head")),
                        box(
                          title = "summary",
                          width = 8,
                          verbatimTextOutput("Resume")
                        )
                      )
                    ),
                    tabItem(
                      #####################
                      #2. visualize Data ##
                      #####################
                      tabName = "visualizeData",
                      conditionalPanel(
                        "output.fileUploaded",
                        box(width=2,
                          tags$h3("parameter"),
                          checkboxInput("addId", "Add id labels in the graph", FALSE),
                          checkboxInput("addBorder", "Add border in the graph", FALSE),
                          checkboxInput("sort", "sort plot by the time spent in the first state"),
                          textInput("plotDataTitle", "title", "Trajectories of the Markov process"),
                           radioButtons(
                            "choixParaGroupeVisualize",
                            "choose an option",
                            choices = c("All", "By group variable" = "byGroup"),
                            selected = "All"
                          ),
                          conditionalPanel(
                            "input.choixParaGroupeVisualize=='byGroup'",
                            uiOutput("groupVarVisualize")
                          ),
                          actionButton("modPlotData", "update plot of data"),
                          downloadButton('downloadPlotTraj', 'Download Plot')#,
                          
                          #style = "unite",
                          #icon = icon("gear"),
                         # status = "primary",
                         # width = "300px",
                         # animate = animateOptions(
                         #   enter = animations$fading_entrances$fadeInLeftBig,
                         #   exit = animations$fading_exits$fadeOutRightBig
                         # )
                        ),
                        box(
                          width = 10,
                          title = "plot data",
                          height = 1000,
                          shinycssloaders::withSpinner(
                          plotOutput("traj"))
                        ),
                        plotOutput("test")
                      ),
                      conditionalPanel(
                        "!output.fileUploaded",
                        h1("You need to import file before use this functionality!")
                      )
                    ),
                    tabItem(
                      tabName = "descriptiveStatistics",
                      conditionalPanel(
                        "output.fileUploaded",
                        box(width = 12,
                            fluidRow(
                              column(
                                4,
                                selectizeInput(
                                  "choixGraphiqueStats",
                                  "choose a statistics",
                                  choices = c(
                                    "summary" = "summary",
                                    "Number of jumps" = "jump",
                                    "Time spent in each state" = "timeState",
                                    "duration of trajectories" =
                                      "duration"
                                  )
                                )
                              ),
                              column(
                                4,
                                radioButtons(
                                  "choixParaGroupeStatistics",
                                  "choose an option",
                                  choices = c("All", "By group variable" = "byGroup"),
                                  selected = "All"
                                )
                              ),
                              conditionalPanel(
                                "input.choixParaGroupeStatistics=='byGroup'",
                                column(4, uiOutput("groupVarStatistics"))
                              )
                            )),
                        uiOutput("statsRes")
                      ),
                      conditionalPanel(
                        "!output.fileUploaded",
                        h1("You need to import file before use this functionality!")
                      )
                    ),
                    tabItem(
                      tabName = "estimationOfMarkovChain",
                      conditionalPanel(
                        "output.fileUploaded",
                        box(
                          width = 12,
                          column(
                            6,
                            radioButtons(
                              "choixParaGroupeMarkov",
                              "choose an option",
                              choices = c("All", "By group variable" = "byGroup"),
                              selected = "All"
                            )
                          ),
                          conditionalPanel(
                            "input.choixParaGroupeMarkov=='byGroup'",
                            column(6, uiOutput("groupVarMarkov"))
                          )
                        )
                        ,
                        tabBox(
                          width = 12,
                          tabPanel(
                            "Transition graph",
                            conditionalPanel(
                              "input.choixParaGroupeMarkov=='byGroup'",
                              fluidRow(uiOutput("transGraphByGroup"))
                            ),
                            conditionalPanel(
                              "input.choixParaGroupeMarkov=='All'",
                              shinycssloaders::withSpinner(
                              plotOutput("transGraphAll"))
                            )
                          )
                          ,
                          tabPanel(
                            "Transition Matrix",
                            conditionalPanel(
                              "input.choixParaGroupeMarkov=='byGroup'",
                              fluidRow(uiOutput("transMatByGroup"))
                            ),
                            conditionalPanel(
                              "input.choixParaGroupeMarkov=='All'",
                              verbatimTextOutput("transMatAll")
                            )
                          ),
                          tabPanel(
                            "number of state changes",
                            conditionalPanel(
                              "input.choixParaGroupeMarkov=='byGroup'",
                              fluidRow(uiOutput("njumpMarkovByGroup"))
                            ),
                            conditionalPanel(
                              "input.choixParaGroupeMarkov=='All'",
                              verbatimTextOutput("njumpMarkovAll")
                            )
                          ),
                          tabPanel(
                            "probability to be in a state",
                            conditionalPanel(
                              "input.choixParaGroupeMarkov=='byGroup'",
                              shinycssloaders::withSpinner(
                              plotlyOutput("probaStateByGroup", height =
                                             "800px"))
                              
                            ),
                            conditionalPanel(
                              "input.choixParaGroupeMarkov=='All'",
                              shinycssloaders::withSpinner(plotlyOutput("probaStateAll", height =
                                             "800px"))
                            )
                          ),
                          tabPanel(
                            "parameters of exponential laws of sojourn time",
                            conditionalPanel(
                              "input.choixParaGroupeMarkov=='byGroup'",
                              fluidRow(uiOutput("expoLawMarkovByGroup"))
                            ),
                            conditionalPanel(
                              "input.choixParaGroupeMarkov=='All'",
                              verbatimTextOutput("expoLawMarkovAll")
                            )
                          )
                        )
                        
                        
                      ),
                      conditionalPanel(
                        "!output.fileUploaded",
                        h1("You need to import file before use this functionality!")
                      )
                      
                    ),
                    tabItem(
                      tabName = "factorialAnalysis",
                      conditionalPanel(
                        "output.fileUploaded",
                        column(width = 4,
                               tags$div(
                                 box(
                                   width = 12,
                                   
                                   box(
                                     width = 12,
                                     uiOutput("titleBoxFactorial"),
                                     textInput("tpsmax", "T:", placeholder = "no space between digits and decimal separateur must be the dot"),
                                     uiOutput("uiAbsorbedState"),
                                     textInput("nameNAstate","name of the none observed state",value="Not observable")
                                   ),
                                   box(
                                     width = 12,
                                     title = "optimal encoding function parameters",
                                     selectizeInput(
                                       "typeBasis",
                                       "select a basis of function",
                                       choice = c("spline" = "spline", "fourier" = "fourier"),
                                       selected = "spline"
                                     ),
                                     numericInput(
                                       "nbasis",
                                       "Number of basis",
                                       min = 1,
                                       value = 10,
                                       max = 20
                                     ),
                                     conditionalPanel(
                                       "input.typeBasis=='spline'",
                                       numericInput(
                                         "norder",
                                         "Degres of splines",
                                         min = 2,
                                         max = 10,
                                         value = 4
                                       )
                                     ),
                                     conditionalPanel(
                                       "output.fmcaUploaded",
                                       downloadButton("downloadResultsCFDA", "download factorial analysis results")
                                     ),
                                     actionButton("soumettre", "Submit")
                                     
                                   )
                                 ),
                                 conditionalPanel(
                                   "output.fmcaUploaded",
                                   box(
                                     width = 12,
                                     title = "summary of the dataset used for the factorial analysis",
                                     verbatimTextOutput("summaryCFDA")
                                   )
                                   
                                 )
                               )),
                        conditionalPanel(
                          "input.soumettre>0",
                          tabBox(
                            title = "factorial analysis results",
                            width = 8,
                            tabPanel("vizualize data",
                                     plotOutput("plotDataCFDA",height="1100px")),
                            tabPanel(
                              "eigen values",
                              wellPanel(shinycssloaders::withSpinner(plotlyOutput("valeurspropres"))),
                              wellPanel(fluidRow(
                                column(9, verbatimTextOutput("eigenvaluesTable")),
                                column(
                                  3,
                                  h4("Eigen values graph options"),
                                  checkboxInput("cumulative", "cumulative")
                                )
                              ))
                            ),
                            tabPanel(
                              "factorial plan",
                              h4("Factorial plan"),
                              wellPanel(style = "background: white;",
                                        fluidRow(
                                          column(10,
                                                 shinycssloaders::withSpinner(plotlyOutput("planfact"))),
                                          column(
                                            2,
                                            uiOutput("dim1"),
                                            uiOutput("dim2"),
                                            uiOutput("groupVarFactorialPlan"),
                                            checkboxInput("addCI", "add confidence interval")
                                          )
                                        )),
                              h4("Optimal encoding function"),
                              fluidRow(column(
                                6 ,
                                wellPanel(
                                  style = "background: white;",
                                  shinycssloaders::withSpinner(plotOutput("optimalEncoding1")),
                                  downloadButton("saveOptimalEncoding1")
                                )
                              ),
                              column(
                                6,
                                wellPanel(
                                  style = "background: white;",
                                  shinycssloaders::withSpinner(plotOutput("optimalEncoding2")),
                                  downloadButton("saveOptimalEncoding2")
                                )
                                
                              ))
                            ),
                            tabPanel(
                              "extrem individuals",
                              wellPanel(style = "background: white;",
                                        fluidRow(
                                          column(12,
                                                 shinycssloaders::withSpinner(plotlyOutput("planfactExtremeIndividuals"))),
                                          dropdown(
                                            uiOutput("extremComp1ui"),
                                            uiOutput("extremComp2ui"),
                                            uiOutput("dim1Extrem"),
                                            uiOutput("dim2Extrem"),
                                            style = "unite",
                                            icon = icon("gear"),
                                            status = "primary",
                                            width = "300px",
                                            animate = animateOptions(
                                              enter = animations$fading_entrances$fadeInLeftBig,
                                              exit = animations$fading_exits$fadeOutRightBig
                                            )
                                          ),
                                          
                                        )),
                              wellPanel(
                                style = "background: white;",
                                uiOutput("axe1"),
                                shinycssloaders::withSpinner(plotOutput("plotDataExtremAxe1")),
                                downloadButton("downloadPlotDataExtremAxe1"),
                                hr(),
                                uiOutput("axe2"),
                                shinycssloaders::withSpinner(plotOutput("plotDataExtremAxe2")),
                                downloadButton("downloadPlotDataExtremAxe2"),
                              )
                            )
                            
                          )
                        )
                        
                        
                        
                      ),
                      conditionalPanel(
                        "!output.fileUploaded",
                        h1("You need to import file before use this functionality!")
                      )
                      
                    ),
                    tabItem(
                      tabName = "clustering",
                      conditionalPanel(
                        "output.fmcaUploaded",
                        box(width = 8,
                            fluidRow(
                              column(
                                3,
                                radioButtons(
                                  inputId = "compsCAH",
                                  label = "Select one",
                                  choices = c(
                                    "Number of component" = 1,
                                    "percentage of variance" = 2
                                  ),
                                  selected = 2
                                )
                              ),
                              column(
                                3,
                                conditionalPanel("input.compsCAH==1",
                                                 uiOutput("nb_comp")),
                                conditionalPanel(
                                  "input.compsCAH==2",
                                  sliderInput(
                                    "percentageVariance",
                                    "select a percentage of variance",
                                    min = 5,
                                    max = 100,
                                    value = 90
                                  )
                                )
                              ),
                              column(
                                3,
                                selectizeInput(
                                  "method",
                                  "select a method",
                                  selected = "ward.D2",
                                  choice = c(
                                    "ward.D2" = "ward.D2",
                                    "ward.D" = "ward.D",
                                    "single" = "single",
                                    "complete" =
                                      "complete",
                                    "average" = "average",
                                    "centroid" = "centroid",
                                    "median" = "median",
                                    "mcquitty" = "mcquitty"
                                  )
                                )
                              ),
                              column(3, actionButton("clusteringSubmit", "submit",style=" margin-left: auto;margin-right: auto;"))
                            )),
                        conditionalPanel(
                          "input.clusteringSubmit>0",
                          box(
                            width = 4,
                            uiOutput("clus"),
                            downloadButton("downloadCAH", "download clusting results")
                          )
                        )
                        
                      ),
                      conditionalPanel(
                        "input.clusteringSubmit>0",
                        tabBox(
                          width = 12,
                          tabPanel(
                            width = 12,
                            title = "dendogram",
                            fluidRow(
                              column(11, shinycssloaders::withSpinner(plotOutput("dendogramme", height = "900px"))),
                              column(
                                1,
                                checkboxInput(inputId = "couper", label = "cut dendogram?"),
                                downloadButton("downloadDendogram")
                              )
                            )
                          ),
                          tabPanel(title = "Cluster",
                                   fluidRow(
                                     column(10, shinycssloaders::withSpinner(plotOutput("groupe", height = "900px"))),
                                     column(2,
                                     tags$div(
                                       column(12,downloadButton("downloadGroupPlot")),
                                       column(12,verbatimTextOutput("effecCluster"))
                                      )
                                     )
                                   )),
                          tabPanel(title = "descriptives statistics by cluster",
                                   fluidRow(
                                     column(4,
                                       tags$div(
                                         selectizeInput(
                                           "choixGraphiqueStatsCluster",
                                           "choose a statistics",
                                           choices =
                                             c(
                                               "Number of jump" = "jump",
                                               "Time spent in each state" = "timeState"
                                             )
                                         )
                                         ,
                                         conditionalPanel(
                                           "input.choixGraphiqueStatsCluster=='jump'",
                                           box(
                                             width = 12,
                                             title = "summary of number ob jumps",
                                             DTOutput("SummaryJumpByCluster")
                                           ),
                                           box(
                                             width = 12,
                                             title = "frenquencies table of number of jumps",
                                             DTOutput("freqJumpByCluster")
                                           ),
                                           box(
                                             width = 12,
                                             title = "proportion table of number of jumps",
                                             selectizeInput(
                                               "tableChoiceCluster",
                                               "choose a table",
                                               choices = c(
                                                 "proportions" = "prop",
                                                 "row profiles" = "row",
                                                 "column profiles" = "column"
                                               ),
                                               selected = "prop"
                                             ),
                                             DTOutput("tableJumpByCluster")
                                           )
                                         ),
                                         conditionalPanel(
                                           "input.choixGraphiqueStatsCluster!='jump'",
                                           uiOutput("timeStateByCluster")
                                           
                                         )
                                         
                                       )
                                     ),
                                     column(8,
                                       conditionalPanel(
                                         "input.choixGraphiqueStatsCluster=='jump'",
                                         wellPanel(style = "background: white;", shinycssloaders::withSpinner(plotlyOutput("nJumpGraphByCluster", height =
                                                                                                "1100px")))
                                       )
                                     )
                                     ,
                                     conditionalPanel(
                                       "input.choixGraphiqueStatsCluster!='jump'",
                                       column(8,
                                              selectizeInput(
                                                "selectTimeSpentClusterPlot",
                                                "choose a graph",
                                                choices = c(
                                                  "state among cluster" = "stateAmoungCluster",
                                                  "state within cluster" = "stateWithinCluster"
                                                )),
                                              wellPanel(
                                                style = "background: white;",
                                                conditionalPanel("input.selectTimeSpentClusterPlot=='stateWithinCluster'",
                                                 shinycssloaders::withSpinner(plotlyOutput("timeStateGraphByCluster", height = "1100px"))),
                                                conditionalPanel("input.selectTimeSpentClusterPlot!='stateWithinCluster'",
                                                uiOutput("timeStateGraphByAmongCluster"))
                                                
                                              ))
                                     )
                                   )),
                          tabPanel(title = "markov chain by cluster",
                                   fluidRow(
                                     column(
                                       12,
                                       selectizeInput(
                                         "choixStatsMarkovCluster",
                                         "choose a statistics",
                                         choices =
                                           c(
                                             "Transition matrix" = "transiMat",
                                             "Transition graph" = "transiGraph",
                                             "number of jump" =
                                               "jump",
                                             "exponential law" = "expoLaw"
                                           )
                                       )
                                     ),
                                     column(12,
                                            
                                            uiOutput("markovByCluster"))
                                   )),
                          tabPanel(
                            title = "description of cluster with group varriables",
                            fluidRow(
                              column(4, uiOutput("groupVarDescCluster")),
                              column(
                                4,
                                selectizeInput(
                                  "typeVarGroup",
                                  "select the type of the variable",
                                  choices = c(
                                    "numeric" = "as.numeric",
                                    "factor" =
                                      "as.factor",
                                    "integer" =
                                      "as.integer"
                                  )
                                )
                              ),
                              
                              conditionalPanel(
                                "input.typeVarGroup == 'as.integer' || input.typeVarGroup == 'as.factor' ",
                                box(
                                  width = 12,
                                  title = "frenquencies ",
                                  DTOutput("freqGroupVarFiniByCluster")
                                ),
                                box(
                                  width = 12,
                                  title = "proportion ",
                                  selectizeInput(
                                    "tableGroupVarFiniChoiceCluster",
                                    "choose a table",
                                    choices = c(
                                      "proportions" = "prop",
                                      "row profiles" = "row",
                                      "column profiles" = "column"
                                    ),
                                    selected = "prop"
                                  ),
                                  DTOutput("tableGroupVarFiniByCluster")
                                )
                              ),
                              conditionalPanel(
                                "input.typeVarGroup=='as.numeric'",
                                box(
                                  width = 12,
                                  title = "summary",
                                  DTOutput("numVarGroupCluster")
                                )
                                
                              )
                              
                            )
                          )
                          
                          
                        )
                      ),
                      conditionalPanel(
                        "!output.fmcaUploaded",
                        h1(
                          "You need to submit factortial analysis before use clustering analysis"
                        )
                      )
                    ),
                    tabItem(tabName = "help",
                            tabBox(
                              width = 12,
                              tabPanel("import data",
                                   p("This part allows you to import a csv file. You need to choose a file, the separator and the decimal. If dataset doesn't appear check the file format"),    
                                    
                                   p("After uploaded the file you can filter individuals by the number of jumps, the length of trajectories observed or choose the first n% individuals")  
                                       ),
                              tabPanel("visualize data",
                                       p("this part allows you to visualize the trajectories of the individuals selected in import part. Some graphics options are provided")
                                       ),
                              tabPanel("descriptive statistics",
                                       p("In this part you can see the statistics provide by cfda package (number of jumps, summary of data, duration of trajectories and time spent in each state). If your dataset contains 
                                         other variables that can be use as group variable you can choose them to compute all of this statistics by group variable.")
                                       ),
                              tabPanel("estimation of markov chain",
                                       p("in this part we assume than the data arise from a markov jump process and we compute all of the parameters of the model. You can also compute them by group variable")
                                       ),
                              tabPanel("Analysis",
                                       p("This part contains 2 analysis : Factorial analysis and clustering analysis. You have to compute factorial analysis before use clusterinf functionnality"),
                                       h3("Factorial Analysis"),
                                       p("You need to choose ending time and basis function"),
                                       h3("Clustering"),
                                       p("after compute factorial analysis you can make a clustering on principal component. You need to choose the method, the number of component or the percentage of variance"),
                                       p("After that the app compute the dendogram and other plots to help you to analyse the differents clusters")
                                       ),
                              tabPanel("simulate a mixture model")
                            ) 
                    ),
                    tabItem(
                      tabName = "simulateMarkov",
                      box(
                        width = 4,
                        
                        checkboxInput(inputId = "optionsSimulation", label = "show simulation options"),
                        conditionalPanel(
                          "input.optionsSimulation",
                          numericInput("nbComponent", "mixture component", value =
                                         2),
                          numericInput(
                            "nbStateMix",
                            "number of state",
                            min = 2,
                            max = 10,
                            value = 2
                          ),
                          numericInput(
                            "nbSimuMix",
                            "number of individuals simulated",
                            min = 1,
                            value = 100
                          ),
                          numericInput(
                            "TmaxMix",
                            "maximal duration of trajectories",
                            min = 1,
                            value = 10
                          ),
                          uiOutput("probabilityGp"),
                          box(width = 12, title = "transition matrix",
                              uiOutput("listMatrix")),
                          box(width = 12, title = "sejourn time",
                              uiOutput("listLambda"), ),
                          box(
                            width = 12,
                            title = "initial law",
                            uiOutput("listInitialLaw"),
                          ),
                          actionButton("SimulateMixtureModel", "Simulate mixture model"),
                          conditionalPanel(
                            "input.SimulateMixtureModel>0",
                            downloadButton("downloadDataMix", "Download")
                          )
                          
                        )
                        
                      ),
                      conditionalPanel(
                        "input.SimulateMixtureModel>0",
                        tabBox(
                          width = 8,
                          tabPanel("dataset",
                                   dataTableOutput("headSimulatedMix")),
                          tabPanel("Plot of data",
                                   shinycssloaders::withSpinner(plotOutput("trajSimulatedMix", height =
                                                "900px")))
                        )
                      )
                    )
                    
                  )
                ))
)
