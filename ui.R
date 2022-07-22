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
if (!require('scales'))
  install.packages("scales")
if (!require('stringr'))
  install.packages("stringr")
js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #d73925;font-weight: bold;
}
"'


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
      menuItem("Help", tabName = "help"),
      
        div(style="",tags$img(src = "inria_logo.png", width="100px"))
    )
  ),
  dashboardBody(
    tags$style(js),
                tags$head(
                  tags$style(".progress-bar{background-color:#d73925;}"),
                  tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #d73925;}')),
                  includeCSS(path = "www/style.css")
                ),
                fluidRow(
                  tabItems(
                    tabItem(
                      ##################
                      #1. Import data ##
                      ##################
                      tabName = "importData",
                      box(
                        width = 12,
                        title = "Import dataset and filter data",
                        status="danger",
                        solidHeader = TRUE, 
                        box(
                          width = 6,
                          status="danger",
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
                                           style = "color: red"),
                              no = tags$i(class = "fa fa-circle-o",
                                          style = "color: red")
                            )
                          ),
                          radioGroupButtons(
                            inputId = "dec",
                            label = "decimal",
                            choices = c("comma" = ",", "dot" = "."),
                            individual = TRUE,
                            checkIcon = list(
                              yes = tags$i(class = "fa fa-circle",
                                           style = "color: red"),
                              no = tags$i(class = "fa fa-circle-o",
                                          style = "color: red")
                            )
                          )
                        ),
                        
                        conditionalPanel(
                          "output.fileUploaded",
                          box( title="Data selection",status="danger",
                            width = 6,
                            checkboxInput("plotDataOptions", "data selection options", value =
                                            T),
                            conditionalPanel(
                              "input.plotDataOptions",
                              fluidRow(
                                column(
                                  4,
                                  hr(),
                                  h5("length of trajectories"),
                                  radioGroupButtons(
                                    inputId = "filterChoiceLength",
                                    "",
                                    choices = c("All available" = "1", "Filter" = "2"),
                                    selected = "1",
                                    status = "danger"
                                    
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
                                    status = "danger"
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
                                    status = "danger"
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
                        box(title = "data table", status="danger",
                            solidHeader = TRUE, width = 8, 
                            shinycssloaders::withSpinner(
                              DTOutput("head"),type = getOption("spinner.type", default = 6),
                              color = getOption("spinner.color", default = "#d73925"))
                            ),
                        box(
                          title = "summary",
                          width = 4,status="danger",
                          solidHeader = TRUE, 
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
                        box(width=2,title = "options",status = "danger", solidHeader = TRUE,
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
                         # status = "danger",
                         # width = "300px",
                         # animate = animateOptions(
                         #   enter = animations$fading_entrances$fadeInLeftBig,
                         #   exit = animations$fading_exits$fadeOutRightBig
                         # )
                        ),
                        box(
                          width = 10,
                          title = "plot of trajectories",status = "danger", solidHeader = TRUE,
                          height = 1000,
                          shinycssloaders::withSpinner(
                          plotOutput("traj"),type = getOption("spinner.type", default = 6),
                          color = getOption("spinner.color", default = "#d73925"))
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
                        tags$div(style="margin-left: auto;margin-right: auto;",
                        box(width = 12,title = "Descriptive statistics options",status = "danger", solidHeader = TRUE,
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
                              column(4,
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
                            ))),
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
                          width = 12,title="plots options",status="danger", solidHeader = TRUE,
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
                              plotOutput("transGraphAll"),type = getOption("spinner.type", default = 6),
                              color = getOption("spinner.color", default = "#d73925"))
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
                                             "800px"),type = getOption("spinner.type", default = 6),
                              color = getOption("spinner.color", default = "#d73925"))
                              
                            ),
                            conditionalPanel(
                              "input.choixParaGroupeMarkov=='All'",
                              shinycssloaders::withSpinner(plotlyOutput("probaStateAll", height =
                                             "800px"),type = getOption("spinner.type", default = 6),
                                             color = getOption("spinner.color", default = "#d73925"))
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
                                   title="factorial analysis parameters",
                                   status="danger",
                                   solidHeader = TRUE,
                                   box(
                                     width = 12,
                                     status="danger",
                                     uiOutput("titleBoxFactorial"),
                                     textInput("tpsmax", "T:", placeholder = "no space between digits and decimal separateur must be the dot"),
                                     uiOutput("uiAbsorbedState"),
                                     textInput("nameNAstate","name of the none observed state",value="Not observable")
                                   ),
                                   box(
                                     width = 12,
                                     status="danger",
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
                                   box(status="danger",
                                       solidHeader = TRUE,
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
                              box(width=12,
                                  status="danger",
                                  solidHeader = FALSE,
                                  shinycssloaders::withSpinner(plotlyOutput("valeurspropres"),type = getOption("spinner.type", default = 6),
                                                               color = getOption("spinner.color", default = "#d73925"))
                                  ),
                              fluidRow(
                                column(9, box(width=12,title="eigen value table",status="danger",solidHeader = TRUE,verbatimTextOutput("eigenvaluesTable"))),
                                column(3,
                                  h4("Eigen values graph options"),
                                  checkboxInput("cumulative", "cumulative")
                                )
                              )
                            ),
                            tabPanel(
                              "factorial plan",
                              fluidRow(box(width=12,title="Factorial plan",status="danger",solidHeader = TRUE,
                                        fluidRow(
                                          box(width=10,status="danger",solidHeader = FALSE,shinycssloaders::withSpinner(plotlyOutput("planfact"),type = getOption("spinner.type", default = 6),
                                                                                                                        color = getOption("spinner.color", default = "#d73925"))),
                                          box(width=2,status="danger",solidHeader = FALSE,
                                            uiOutput("dim1"),
                                            uiOutput("dim2"),
                                            uiOutput("groupVarFactorialPlan"),
                                            checkboxInput("addCI", "add confidence interval")
                                          )
                                        )),
                              box(width=12,title="Optimal encoding function",status="danger",solidHeader = TRUE,
                              fluidRow(box(width=6 ,status="danger",solidHeader = FALSE,
                                  shinycssloaders::withSpinner(plotlyOutput("optimalEncoding1"),type = getOption("spinner.type", default = 6),
                                                               color = getOption("spinner.color", default = "#d73925"))
                                  
                              ),
                              box(width=6,status="danger",solidHeader = FALSE,
                                  shinycssloaders::withSpinner(plotlyOutput("optimalEncoding2"),type = getOption("spinner.type", default = 6),
                                                               color = getOption("spinner.color", default = "#d73925"))
                                
                                
                              )))
                            )),
                            tabPanel(
                              "extrem individuals", 
                                fluidRow(
                                          box(width=12,title="Factorial plan with extrem individuals", status="danger",solidHeader = TRUE,
                                                 shinycssloaders::withSpinner(plotlyOutput("planfactExtremeIndividuals"),type = getOption("spinner.type", default = 6),
                                                                              color = getOption("spinner.color", default = "#d73925"))),
                                          dropdown(
                                            uiOutput("extremComp1ui"),
                                            uiOutput("extremComp2ui"),
                                            uiOutput("dim1Extrem"),
                                            uiOutput("dim2Extrem"),
                                            style = "unite",
                                            icon = icon("gear"),
                                            status = "danger",
                                            width = "300px",
                                            label="click to see graph options",
                                            animate = animateOptions(
                                              enter = animations$fading_entrances$fadeInLeftBig,
                                              exit = animations$fading_exits$fadeOutRightBig
                                            )
                                          ),
                                          
                                        
                              box(width=12,title="Plot of trajectories of extrem individuals",status="danger",solidHeader = TRUE,
                                  box(width=12,status="danger",solidHeader = FALSE,
                                uiOutput("axe1"),
                                shinycssloaders::withSpinner(plotOutput("plotDataExtremAxe1"),type = getOption("spinner.type", default = 6),
                                                             color = getOption("spinner.color", default = "#d73925")),
                                downloadButton("downloadPlotDataExtremAxe1")),
                                hr(),
                                box(width=12,status="danger",solidHeader = FALSE,
                                uiOutput("axe2"),
                                shinycssloaders::withSpinner(plotOutput("plotDataExtremAxe2"),type = getOption("spinner.type", default = 6),
                                                             color = getOption("spinner.color", default = "#d73925")),
                                downloadButton("downloadPlotDataExtremAxe2")),
                              )
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
                        box(width = 8,title="clustering options",status="danger",solidHeader = TRUE,
                            fluidRow(
                              column(3,
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
                              column(3,
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
                              column(3,
                                selectizeInput(
                                  "method",
                                  "select a method",
                                  selected = "ward.D2",
                                  choice = c(
                                    "ward.D2" = "ward.D2",
                                    "ward.D" = "ward.D",
                                    "single" = "single",
                                    "complete" ="complete",
                                    "average" = "average",
                                    "centroid" = "centroid",
                                    "median" = "median",
                                    "mcquitty" = "mcquitty"
                                  )
                                )
                              ),
                              column(3, div(style="margin-left: auto;margin-right: auto;",actionButton("clusteringSubmit", "submit")))
                            )),
                        conditionalPanel(
                          "input.clusteringSubmit>0",
                          box(
                            width = 4,title="number of clusters",status="danger",solidHeader = TRUE,
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
                              column(11, shinycssloaders::withSpinner(plotOutput("dendogramme", height = "900px"),type = getOption("spinner.type", default = 6),
                                                                      color = getOption("spinner.color", default = "#d73925"))),
                              column(1,
                                checkboxInput(inputId = "couper", label = "cut dendogram?"),
                                downloadButton("downloadDendogram")
                              )
                            )
                          ),
                          tabPanel(title = "Cluster",
                                   fluidRow(
                                     box(width=10, title="plot of cluster",status="danger", solidHeader=TRUE,shinycssloaders::withSpinner(plotOutput("groupe", height = "900px"),type = getOption("spinner.type", default = 6),
                                                                                                                                          color = getOption("spinner.color", default = "#d73925"))),
                                     box(width=2,title="cluster repartition",status="danger", solidHeader=TRUE,verbatimTextOutput("effecCluster")),
                                     downloadButton("downloadGroupPlot")
                                     )
                                     
                                   ),
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
                                             title = "summary of number ob jumps",status="danger", solidHeader=TRUE,
                                             DTOutput("SummaryJumpByCluster")
                                           ),
                                           box(
                                             width = 12,
                                             title = "frenquencies table of number of jumps",status="danger", solidHeader=TRUE,
                                             DTOutput("freqJumpByCluster")
                                           ),
                                           box(
                                             width = 12,
                                             title = "proportion table of number of jumps",status="danger", solidHeader=TRUE,
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
                                         box(width=12,status="danger", solidHeader=TRUE,title="Number of jumps by cluster", shinycssloaders::withSpinner(plotlyOutput("nJumpGraphByCluster", height =
                                                                                                "1100px"),type = getOption("spinner.type", default = 6),
                                                                                                color = getOption("spinner.color", default = "#d73925")))
                                       )
                                     )
                                     ,
                                     conditionalPanel(
                                       "input.choixGraphiqueStatsCluster!='jump'",
                                       column(8,align="center",
                                              selectizeInput(
                                                "selectTimeSpentClusterPlot",
                                                "choose a graph",
                                                choices = c(
                                                  "state among cluster" = "stateAmoungCluster",
                                                  "state within cluster" = "stateWithinCluster"
                                                )),
                                              box(width=12,status="danger", solidHeader=TRUE,
                                                title = "time spent in each state by cluster",
                                                conditionalPanel("input.selectTimeSpentClusterPlot=='stateWithinCluster'",
                                                 shinycssloaders::withSpinner(plotlyOutput("timeStateGraphByCluster", height = "1100px"),type = getOption("spinner.type", default = 6),
                                                                              color = getOption("spinner.color", default = "#d73925"))),
                                                conditionalPanel("input.selectTimeSpentClusterPlot!='stateWithinCluster'",
                                                uiOutput("timeStateGraphByAmongCluster"))
                                                
                                              ))
                                     )
                                   )),
                          tabPanel(title = "markov chain by cluster",
                                   fluidRow(
                                     column(12,
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
                                    "factor" ="as.factor",
                                    "integer" ="as.integer"
                                  )
                                )
                              ),
                              
                              conditionalPanel(
                                "input.typeVarGroup == 'as.integer' || input.typeVarGroup == 'as.factor' ",
                                box(
                                  width = 6, status="danger", solidHeader=TRUE,
                                  title = "frenquencies ",
                                  DTOutput("freqGroupVarFiniByCluster")
                                ),
                                box(
                                  width = 6,status="danger", solidHeader=TRUE,
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
                                  title = "summary",status="danger", solidHeader=TRUE,
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
                        title="simulation options",
                        status="danger", solidHeader=TRUE,
                          numericInput("nbComponent", "mixture component", value = 2),
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
                          box(width = 12, title = "transition matrix", status="danger",
                              uiOutput("listMatrix")),
                          box(width = 12, title = "sejourn time", status="danger",
                              uiOutput("listLambda")),
                          box(
                            width = 12,
                            title = "initial law", status="danger",
                            uiOutput("listInitialLaw")
                          ),
                          actionButton("SimulateMixtureModel", "Simulate mixture model")

                      ),
                      conditionalPanel(
                        "input.SimulateMixtureModel>0",
                        tabBox(
                          width = 8,
                          tabPanel("dataset",
                                   dataTableOutput("headSimulatedMix")),
                          tabPanel("Plot of data",
                                   shinycssloaders::withSpinner(plotOutput("trajSimulatedMix", height =
                                                "900px"),type = getOption("spinner.type", default = 6),
                                                color = getOption("spinner.color", default = "#d73925")))
                        )
                      )
                    )
                    
                  )
                )),skin = "red"
)
