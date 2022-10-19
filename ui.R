js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #d73925;font-weight: bold;
}
"'


ui <- dashboardPage(
  dashboardHeader(title = "CFDA"),
  dashboardSidebar(
    width = "300px",
    sidebarMenu(
      style = "font-size:20px;",
      menuItem("Import data", tabName = "importData"),
      menuItem("Visualize data", tabName = "visualizeData"),
      menuItem("Descriptive statistics", tabName = "descriptiveStatistics"),
      menuItem("Estimation of Markov chain", tabName = "estimationOfMarkovChain"),
      menuItem(
        "Analysis",
        tabName = "analysis",
        menuSubItem("Factorial analysis", tabName = "factorialAnalysis"),
        menuSubItem("Clustering", tabName = "clustering")
      ),
      menuItem("Simulate a mixture model", tabName = "simulateMarkov"),
      menuItem("Help", tabName = "help"),
      div(style = "margin-left: 10px;", tags$img(src = "inria_logo.png", width = "100px")),
      div(style = "margin-left: 10px;",
        h3("Design by:"),
        h4("- Whillem TONGO"),
        h4("- Cristian PREDA"),
        h4("- Vincent VANDEWALLE"),
        h4("- Quentin GRIMONPREZ")
      )
    )
  ),
  dashboardBody(
    tags$style(js),
    tags$head(
      tags$style(".progress-bar{background-color:#d73925;}"),
      tags$style(type = "text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; }
                #inline .form-group { display: table-row;}"),
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #d73925;}")),
      includeCSS(path = "www/style.css")
    ),
    fluidRow(
      tabItems(
        tabItem(
          ###################
          # 1. Import data ##
          ###################
          tabName = "importData",
          box(
            width = 12,
            title = "Import a data set and filter data",
            status = "danger",
            solidHeader = TRUE,
            box(
              width = 6,
              status = "danger",
              fileInput(
                "file1",
                "Import file (The file must have at least 3 columns named: id, time and state)",
                multiple = FALSE,
                accept = c("text/csv", ".csv", ".txt", "text/plain")
              ),
              radioGroupButtons(
                inputId = "sep",
                label = "Separator",
                choices = c("Semicolon" = ";",  "Tabulation" = "\t", "Space" = " ", "Comma" = ","),
                individual = TRUE,
                checkIcon = list(
                  yes = tags$i(class = "fa fa-circle", style = "color: red"),
                  no = tags$i(class = "fa fa-circle-o", style = "color: red")
                )
              ),
              radioGroupButtons(
                inputId = "dec",
                label = "Decimal",
                choices = c("Comma" = ",", "Dot" = "."),
                individual = TRUE,
                checkIcon = list(
                  yes = tags$i(class = "fa fa-circle", style = "color: red"),
                  no = tags$i(class = "fa fa-circle-o", style = "color: red")
                )
              )
            ),
            conditionalPanel(
              "output.fileUploaded",
              box(
                title = "Data selection", status = "danger",
                width = 6,
                checkboxInput("plotDataOptions", "Data selection options", value = TRUE),
                conditionalPanel(
                  "input.plotDataOptions",
                  fluidRow(
                    column(
                      4,
                      hr(),
                      h5("Length of trajectories"),
                      radioGroupButtons(
                        inputId = "filterChoiceLength",
                        "",
                        choices = c("All available" = "1", "Filter" = "2"),
                        selected = "1",
                        status = "danger"
                      ),
                      conditionalPanel(
                        "input.filterChoiceLength=='2'",
                        tagList(
                          numericInput("lower", "Individuals observed between time", value = NA),
                          numericInput("upper", "and time", value = NA)
                        )
                      )
                    ),
                    column(
                      4,
                      hr(),
                      h5("Number of jumps"),
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
                      h5("Sample percentage"),
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
                          max = 100,
                          value = 100,
                          step = 5
                        )
                      )
                    )
                  ),
                  actionButton("applyFilter", "Apply")
                )
              )
            )
          ),
          conditionalPanel(
            "output.fileUploaded",
            box(
              title = "Data table", status = "danger", solidHeader = TRUE, width = 8,
              shinycssloaders::withSpinner(
                DTOutput("head"),
                type = getOption("spinner.type", default = 6),
                color = getOption("spinner.color", default = "#d73925")
              )
            ),
            box(
              title = "Summary of data", width = 4, status = "danger", solidHeader = TRUE,
              verbatimTextOutput("Resume")
            )
          )
        ),
        tabItem(
          ######################
          # 2. visualize Data ##
          ######################
          tabName = "visualizeData",
          conditionalPanel(
            "output.fileUploaded",
            box(
              width = 2, title = "Options", status = "danger", solidHeader = TRUE,
              checkboxInput("addId", "Add id labels in the graph", FALSE),
              checkboxInput("addBorder", "Add border in the graph", FALSE),
              checkboxInput("sort", "Sort plot by the time spent in the first state"),
              textInput("plotDataTitle", "Title", "Trajectories of the Markov process"),
              radioButtons(
                "choixParaGroupeVisualize",
                "Choose an option",
                choices = c("All", "By group variable" = "byGroup"),
                selected = "All"
              ),
              conditionalPanel("input.choixParaGroupeVisualize=='byGroup'", uiOutput("groupVarVisualize")),
              actionButton("modPlotData", "Update plot"),
              downloadButton("downloadPlotTraj", "Download Plot")
            ),
            box(
              width = 10,
              title = "Plot of trajectories", status = "danger", solidHeader = TRUE,
              height = 1000,
              shinycssloaders::withSpinner(
                plotOutput("traj"),
                type = getOption("spinner.type", default = 6),
                color = getOption("spinner.color", default = "#d73925")
              )
            ),
            plotOutput("test")
          ),
          conditionalPanel("!output.fileUploaded", column(12, h1("You have to import file before using this feature!")))
        ),
        tabItem(
          ##############################
          # 3. Descriptive statistics ##
          ##############################
          tabName = "descriptiveStatistics",
          conditionalPanel(
            "output.fileUploaded",
            tags$div(
              style = "margin-left: auto;margin-right: auto;",
              box(
                width = 12, title = "Descriptive statistics options", status = "danger", solidHeader = TRUE,
                fluidRow(
                  column(
                    4,
                    selectizeInput(
                      "choixGraphiqueStats",
                      "Choose a statistic",
                      choices = c(
                        "Summary" = "summary",
                        "Number of jumps" = "jump",
                        "Time spent in each state" = "timeState",
                        "Duration of trajectories" = "duration"
                      )
                    )
                  ),
                  column(
                    4,
                    radioButtons(
                      "choixParaGroupeStatistics",
                      "Choose an option",
                      choices = c("All", "By group variable" = "byGroup"),
                      selected = "All"
                    )
                  ),
                  conditionalPanel(
                    "input.choixParaGroupeStatistics=='byGroup'",
                    column(4, uiOutput("groupVarStatistics"))
                  )
                )
              )
            ),
            conditionalPanel(
              ########
              # All ##
              ########
              "input.choixParaGroupeStatistics=='All'",
              conditionalPanel(
                "input.choixGraphiqueStats=='summary'",
                box(
                  width = 12,
                  title = "Summary of data set", status = "danger", solidHeader = TRUE,
                  verbatimTextOutput("summary")
                )
              ),
              conditionalPanel(
                "input.choixGraphiqueStats=='jump' ||input.choixGraphiqueStats=='duration' ",
                column(
                  4,
                  tags$div(
                    box(
                      width = 12,
                      title = "Summary of number of jumps", status = "danger", solidHeader = TRUE,
                      DTOutput("summaryStatsAll")
                    ),
                    conditionalPanel(
                      "input.choixGraphiqueStats=='jump'",
                      box(
                        width = 12,
                        title = "Frequencies and proportions", status = "danger", solidHeader = TRUE,
                        DTOutput("nJumpTable")
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                "input.choixGraphiqueStats=='timeState'",
                column(
                  4,
                  box(
                    width = 12, status = "danger", solidHeader = TRUE,
                    title = "Summary of time spent by state",
                    DTOutput("timeSpentAllTable")
                  )
                )
              ),
              conditionalPanel(
                "input.choixGraphiqueStats!='summary'",
                column(
                  8,
                  box(
                    width = 12,
                    status = "danger",
                    shinycssloaders::withSpinner(
                      plotlyOutput("plots", height = "1100px"),
                      type = getOption("spinner.type", default = 6),
                      color = getOption("spinner.color", default = "#d73925")
                    )
                  )
                )
              )
            ),
            conditionalPanel(
              ############
              # By group #
              ############
              "input.choixParaGroupeStatistics=='byGroup'",
              conditionalPanel(
                "input.choixGraphiqueStats=='summary'",
                box(
                  width = 12,
                  title = "Summary of data set by group", status = "danger", solidHeader = TRUE,
                  uiOutput("summaryGp")
                )
              ),
              conditionalPanel(
                "input.choixGraphiqueStats=='jump' ||input.choixGraphiqueStats=='duration'",
                column(
                  4,
                  tags$div(
                    box(
                      width = 12,
                      title = "Summary of statistic by group", status = "danger", solidHeader = TRUE,
                      DTOutput("summaryStatsByGroup")
                    ),
                    conditionalPanel(
                      "input.choixGraphiqueStats=='jump'",
                      box(
                        width = 12,
                        title = "Frequencies by group variable", status = "danger", solidHeader = TRUE,
                        DTOutput("nJumpTableGroupFreq")
                      ),
                      box(
                        width = 12,
                        title = "Proportions by group variable", status = "danger", solidHeader = TRUE,
                        selectizeInput(
                          "tableChoiceGroupDesc",
                          "Choose a table",
                          choices = c("Proportions" = "prop", "Row profiles" = "row", "Column profiles" = "column"),
                          selected = "prop"
                        ),
                        DTOutput("nJumpTableGroupTable")
                      )
                    )
                  )
                ),
                column(
                  8,
                  box(
                    width = 12,
                    status = "danger",
                    conditionalPanel(
                      "input.choixGraphiqueStats=='jump'",
                      shinycssloaders::withSpinner(
                        plotlyOutput("jumpGp", height = "1100px"),
                        type = getOption("spinner.type", default = 6),
                        color = getOption("spinner.color", default = "#d73925")
                      )
                    ),
                    conditionalPanel(
                      "input.choixGraphiqueStats=='duration'",
                      shinycssloaders::withSpinner(
                        plotlyOutput("durationGp", height = "1100px"),
                        type = getOption("spinner.type", default = 6),
                        color = getOption("spinner.color", default = "#d73925")
                      )
                    ),
                  )
                )
              ),
              conditionalPanel(
                "input.choixGraphiqueStats=='timeState'",
                column(4, box(width = 12, status = "danger", uiOutput("timeSpentTableGp"))),
                column(
                  8,
                  box(
                    width = 12, status = "danger",
                    shinycssloaders::withSpinner(
                      plotlyOutput("timeStateGp", height = "1100px"),
                      type = getOption("spinner.type", default = 6),
                      color = getOption("spinner.color", default = "#d73925")
                    )
                  )
                )
              )
            )
          ),
          conditionalPanel("!output.fileUploaded", column(12, h1("You have to import file before using this feature!")))
        ),
        ###############################
        # 4.  Markov chain estimation #
        ###############################
        tabItem(
          tabName = "estimationOfMarkovChain",
          conditionalPanel(
            "output.fileUploaded",
            box(
              width = 12, title = "Graphics options", status = "danger", solidHeader = TRUE,
              column(
                6,
                radioButtons(
                  "choixParaGroupeMarkov",
                  "Choose an option",
                  choices = c("All", "By group variable" = "byGroup"),
                  selected = "All"
                )
              ),
              conditionalPanel("input.choixParaGroupeMarkov=='byGroup'", column(6, uiOutput("groupVarMarkov")))
            ),
            tabBox(
              width = 12,
              tabPanel(
                ## Transition graph
                "Transition graph",
                fluidRow(
                  conditionalPanel("input.choixParaGroupeMarkov=='byGroup'", fluidRow(uiOutput("transGraphByGroup"))),
                  conditionalPanel(
                    "input.choixParaGroupeMarkov=='All'",
                    shinycssloaders::withSpinner(
                      plotOutput("transGraphAll"),
                      type = getOption("spinner.type", default = 6)
                    ),
                    color = getOption("spinner.color", default = "#d73925")
                  )
                )
              ),
              tabPanel(
                "Transition Matrix",
                conditionalPanel("input.choixParaGroupeMarkov=='byGroup'", fluidRow(uiOutput("transMatByGroup"))),
                conditionalPanel("input.choixParaGroupeMarkov=='All'", verbatimTextOutput("transMatAll"))
              ),
              tabPanel(
                "Number of state changes",
                conditionalPanel("input.choixParaGroupeMarkov=='byGroup'", fluidRow(uiOutput("njumpMarkovByGroup"))),
                conditionalPanel("input.choixParaGroupeMarkov=='All'", verbatimTextOutput("njumpMarkovAll"))
              ),
              tabPanel(
                "Probability to be in a state",
                conditionalPanel(
                  "input.choixParaGroupeMarkov=='byGroup'",
                  shinycssloaders::withSpinner(
                    plotlyOutput("probaStateByGroup", height = "800px"),
                    type = getOption("spinner.type", default = 6),
                    color = getOption("spinner.color", default = "#d73925")
                  )
                ),
                conditionalPanel(
                  "input.choixParaGroupeMarkov=='All'",
                  shinycssloaders::withSpinner(
                    plotlyOutput("probaStateAll", height = "800px"),
                    type = getOption("spinner.type", default = 6),
                    color = getOption("spinner.color", default = "#d73925")
                  )
                )
              ),
              tabPanel(
                "Parameters of exponential laws of sojourn time",
                conditionalPanel("input.choixParaGroupeMarkov=='byGroup'", fluidRow(uiOutput("expoLawMarkovByGroup"))),
                conditionalPanel("input.choixParaGroupeMarkov=='All'", verbatimTextOutput("expoLawMarkovAll"))
              )
            )
          ),
          conditionalPanel("!output.fileUploaded", column(12, h1("You have to import file before using this feature!")))
        ),
        tabItem(
          ############################
          # 5.1 factorial Analysis  ##
          ############################
          tabName = "factorialAnalysis",
          conditionalPanel(
            "output.fileUploaded",
            column(
              width = 4,
              tags$div(
                box(
                  width = 12,
                  title = "Factorial analysis parameters",
                  status = "danger",
                  solidHeader = TRUE,
                  box(
                    width = 12,
                    status = "danger",
                    uiOutput("titleBoxFactorial"),
                    tags$div(id = "inline", numericInput("tpsmax", "T:", value = NA)),
                    br(),
                    uiOutput("uiAbsorbedState"),
                    textInput("nameNAstate", "Name of the non observed state", value = "Not observable")
                  ),
                  box(
                    width = 12,
                    status = "danger",
                    title = "Optimal encoding function parameters",
                    selectizeInput(
                      "typeBasis",
                      "Select a basis of functions",
                      choice = c("Spline" = "spline", "Fourier" = "fourier"),
                      selected = "spline"
                    ),
                    numericInput("nbasis", "Number of basis", min = 1, value = 10, max = 20),
                    conditionalPanel(
                      "input.typeBasis=='spline'",
                      numericInput("norder", "Degres of splines", min = 2, max = 10, value = 4)
                    ),
                    conditionalPanel(
                      "output.fmcaUploaded",
                      downloadButton("downloadResultsCFDA", "Download factorial analysis results")
                    ),
                    br(),
                    actionButton("soumettre", "Submit")
                  )
                ),
                conditionalPanel(
                  "output.fmcaUploaded",
                  box(
                    status = "danger",
                    solidHeader = TRUE,
                    width = 12,
                    title = "Summary of the dataset used for the factorial analysis",
                    verbatimTextOutput("summaryCFDA")
                  )
                )
              )
            ),
            conditionalPanel(
              "input.soumettre>0",
              tabBox(
                title = "Factorial analysis results",
                width = 8,
                tabPanel("Visualize data", plotOutput("plotDataCFDA", height = "1100px")),
                tabPanel(
                  "Eigen values",
                  box(
                    width = 12,
                    status = "danger",
                    solidHeader = FALSE,
                    shinycssloaders::withSpinner(
                      plotlyOutput("valeurspropres"),
                      type = getOption("spinner.type", default = 6),
                      color = getOption("spinner.color", default = "#d73925")
                    )
                  ),
                  fluidRow(
                    column(
                      9,
                      box(
                        width = 12, title = "Eigen values table", status = "danger", solidHeader = TRUE,
                        verbatimTextOutput("eigenvaluesTable")
                      )
                    ),
                    column(3, h4("Eigen values graph options"), checkboxInput("cumulative", "Cumulative"))
                  )
                ),
                tabPanel(
                  "Factorial plan",
                  fluidRow(
                    box(
                      width = 12, title = "Factorial plan", status = "danger", solidHeader = TRUE,
                      fluidRow(
                        box(
                          width = 10, status = "danger", solidHeader = FALSE,
                          shinycssloaders::withSpinner(
                            plotlyOutput("planfact"),
                            type = getOption("spinner.type", default = 6),
                            color = getOption("spinner.color", default = "#d73925")
                          )
                        ),
                        box(
                          width = 2, status = "danger", solidHeader = FALSE,
                          uiOutput("dim1"),
                          uiOutput("dim2"),
                          uiOutput("groupVarFactorialPlan"),
                          checkboxInput("addCI", "Add confidence interval")
                        )
                      )
                    ),
                    box(
                      width = 12, title = "Optimal encoding function", status = "danger", solidHeader = TRUE,
                      fluidRow(
                        box(
                          width = 6, status = "danger", solidHeader = FALSE,
                          shinycssloaders::withSpinner(
                            plotlyOutput("optimalEncoding1"),
                            type = getOption("spinner.type", default = 6),
                            color = getOption("spinner.color", default = "#d73925")
                          )
                        ),
                        box(
                          width = 6, status = "danger", solidHeader = FALSE,
                          shinycssloaders::withSpinner(
                            plotlyOutput("optimalEncoding2"),
                            type = getOption("spinner.type", default = 6),
                            color = getOption("spinner.color", default = "#d73925")
                          )
                        )
                      )
                    )
                  )
                ),
                tabPanel(
                  "Extreme individuals",
                  fluidRow(
                    box(
                      width = 12, title = "Factorial plan with extreme individuals", status = "danger", solidHeader = TRUE,
                      shinycssloaders::withSpinner(
                        plotlyOutput("planfactExtremeIndividuals"),
                        type = getOption("spinner.type", default = 6),
                        color = getOption("spinner.color", default = "#d73925")
                      )
                    ),
                    column(12,
                      dropdown(
                        uiOutput("extremComp1ui"),
                        uiOutput("extremComp2ui"),
                        uiOutput("dim1Extrem"),
                        uiOutput("dim2Extrem"),
                        icon = icon("gear"),
                        status = "danger",
                        width = "300px",
                        label = "Click to see graph and options",
                        animate = FALSE
                      ),
                      br()
                    ),
                    box(
                      width = 12, title = "Plot of trajectories of extreme individuals", status = "danger", solidHeader = TRUE,
                      box(
                        width = 12, status = "danger", solidHeader = FALSE,
                        uiOutput("axe1"),
                        shinycssloaders::withSpinner(
                          plotOutput("plotDataExtremAxe1"),
                          type = getOption("spinner.type", default = 6),
                          color = getOption("spinner.color", default = "#d73925")
                        ),
                        downloadButton("downloadPlotDataExtremAxe1")
                      ),
                      hr(),
                      box(
                        width = 12, status = "danger", solidHeader = FALSE,
                        uiOutput("axe2"),
                        shinycssloaders::withSpinner(
                          plotOutput("plotDataExtremAxe2"),
                          type = getOption("spinner.type", default = 6),
                          color = getOption("spinner.color", default = "#d73925")
                        ),
                        downloadButton("downloadPlotDataExtremAxe2")
                      ),
                    )
                  )
                )
              )
            )
          ),
          conditionalPanel(
            "!output.fileUploaded",
            column(12, h1("You have to import file before using this feature!"))
          )
        ),
        tabItem(
          ########################
          # 5.2 Clustering      ##
          ########################
          tabName = "clustering",
          conditionalPanel(
            "output.fmcaUploaded",
            box(
              width = 8, title = "Clustering options", status = "danger", solidHeader = TRUE,
              fluidRow(
                column(
                  3,
                  radioButtons(
                    inputId = "compsCAH",
                    label = "Select one",
                    choices = c("Number of component" = 1, "Percentage of variance" = 2),
                    selected = 2
                  )
                ),
                column(
                  3,
                  conditionalPanel("input.compsCAH==1", uiOutput("nb_comp")),
                  conditionalPanel(
                    "input.compsCAH==2",
                    sliderInput("percentageVariance", "Select a percentage of variance", min = 5, max = 100, value = 90)
                  )
                ),
                column(
                  3,
                  selectizeInput(
                    "method",
                    "Select a method",
                    selected = "ward.D2",
                    choice = c(
                      "ward.D2" = "ward.D2",
                      "ward.D" = "ward.D",
                      "single" = "single",
                      "complete" = "complete",
                      "average" = "average",
                      "centroid" = "centroid",
                      "median" = "median",
                      "mcquitty" = "mcquitty"
                    )
                  )
                ),
                column(
                  3,
                  tags$div(style = "margin-left: auto;margin-right: auto;", actionButton("clusteringSubmit", "Submit"))
                )
              )
            ),
            conditionalPanel(
              "input.clusteringSubmit>0",
              box(
                width = 4, title = "Number of clusters", status = "danger", solidHeader = TRUE,
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
                title = "Dendrogram",
                fluidRow(
                  column(
                    11,
                    shinycssloaders::withSpinner(
                      plotOutput("dendrogramme", height = "900px"),
                      type = getOption("spinner.type", default = 6),
                      color = getOption("spinner.color", default = "#d73925")
                    )
                  ),
                  column(
                    1,
                    checkboxInput(inputId = "couper", label = "Cut dendrogram?"),
                    downloadButton("downloadDendogram")
                  )
                )
              ),
              tabPanel(
                title = "Cluster",
                fluidRow(
                  box(width = 10, title = "Plot of clusters", status = "danger", solidHeader = TRUE,
                  shinycssloaders::withSpinner(
                    plotOutput("groupe", height = "900px"),
                    type = getOption("spinner.type", default = 6),
                    color = getOption("spinner.color", default = "#d73925")
                    )
                  ),
                  box(
                    width = 2, title = "Clusters repartition", status = "danger", solidHeader = TRUE,
                    verbatimTextOutput("effecCluster")
                  ),
                  downloadButton("downloadGroupPlot")
                )
              ),
              tabPanel(
                title = "Descriptive statistics by cluster",
                fluidRow(
                  column(
                    4,
                    tags$div(
                      selectizeInput(
                        "choixGraphiqueStatsCluster",
                        "Choose a statistic",
                        choices = c("Number of jumps" = "jump", "Time spent in each state" = "timeState")
                      ),
                      conditionalPanel(
                        "input.choixGraphiqueStatsCluster=='jump'",
                        box(
                          width = 12,
                          title = "Summary of number of jumps", status = "danger", solidHeader = TRUE,
                          DTOutput("SummaryJumpByCluster")
                        ),
                        box(
                          width = 12,
                          title = "Frenquencies table of number of jumps", status = "danger", solidHeader = TRUE,
                          DTOutput("freqJumpByCluster")
                        ),
                        box(
                          width = 12,
                          title = "Proportions tables of number of jumps", status = "danger", solidHeader = TRUE,
                          selectizeInput(
                            "tableChoiceCluster",
                            "Choose a table",
                            choices = c("Proportions" = "prop", "Row profiles" = "row", "Column profiles" = "column"),
                            selected = "prop"
                          ),
                          DTOutput("tableJumpByCluster")
                        )
                      ),
                      conditionalPanel("input.choixGraphiqueStatsCluster!='jump'", uiOutput("timeStateByCluster"))
                    )
                  ),
                  column(
                    8,
                    conditionalPanel(
                      "input.choixGraphiqueStatsCluster=='jump'",
                      box(
                        width = 12, status = "danger", solidHeader = TRUE, title = "Number of jumps by cluster",
                        shinycssloaders::withSpinner(
                          plotlyOutput("nJumpGraphByCluster", height = "1100px"),
                          type = getOption("spinner.type", default = 6),
                          color = getOption("spinner.color", default = "#d73925")
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    "input.choixGraphiqueStatsCluster!='jump'",
                    column(8,
                      align = "center",
                      selectizeInput(
                        "selectTimeSpentClusterPlot",
                        "Choose a graph",
                        choices = c(
                          "State among cluster" = "stateAmoungCluster",
                          "State within cluster" = "stateWithinCluster"
                        )
                      ),
                      box(
                        width = 12, status = "danger", solidHeader = TRUE,
                        title = "Time spent in each state by cluster",
                        conditionalPanel(
                          "input.selectTimeSpentClusterPlot=='stateWithinCluster'",
                          shinycssloaders::withSpinner(
                            plotlyOutput("timeStateGraphByCluster", height = "1100px"),
                            type = getOption("spinner.type", default = 6),
                            color = getOption("spinner.color", default = "#d73925")
                          )
                        ),
                        conditionalPanel(
                          "input.selectTimeSpentClusterPlot!='stateWithinCluster'",
                          uiOutput("timeStateGraphByAmongCluster")
                        )
                      )
                    )
                  )
                )
              ),
              tabPanel(
                title = "Markov chain by cluster",
                fluidRow(
                  column(
                    12,
                    selectizeInput(
                      "choixStatsMarkovCluster",
                      "Choose a statistic",
                      choices = c(
                        "Transition matrix" = "transiMat",
                        "Transition graph" = "transiGraph",
                        "Number of jumps" = "jump",
                        "Exponential law" = "expoLaw"
                      )
                    )
                  ),
                  column(12, uiOutput("markovByCluster"))
                )
              ),
              tabPanel(
                title = "Description of clusters with group variables",
                fluidRow(
                  column(4, uiOutput("groupVarDescCluster")),
                  column(
                    4,
                    selectizeInput(
                      "typeVarGroup",
                      "Select the type of the variable",
                      choices = c("Numeric" = "as.numeric", "Factor" = "as.factor", "Integer" = "as.integer")
                    )
                  ),
                  conditionalPanel(
                    "input.typeVarGroup == 'as.integer' || input.typeVarGroup == 'as.factor' ",
                    box(
                      width = 6, status = "danger", solidHeader = TRUE,
                      title = "Frenquencies ",
                      DTOutput("freqGroupVarFiniByCluster")
                    ),
                    box(
                      width = 6, status = "danger", solidHeader = TRUE,
                      title = "Proportions",
                      selectizeInput(
                        "tableGroupVarFiniChoiceCluster",
                        "Choose a table",
                        choices = c("Proportions" = "prop", "Row profiles" = "row", "Column profiles" = "column"),
                        selected = "prop"
                      ),
                      DTOutput("tableGroupVarFiniByCluster")
                    )
                  ),
                  conditionalPanel(
                    "input.typeVarGroup=='as.numeric'",
                    box(
                      width = 12,
                      title = "Summary", status = "danger", solidHeader = TRUE,
                      DTOutput("numVarGroupCluster")
                    )
                  )
                )
              )
            )
          ),
          conditionalPanel(
            "!output.fmcaUploaded",
            column(12, h1("You have to perform a factorial analysis before performing a clustering analysis"))
          )
        ),
        tabItem(
          ########################################
          # 6. Simulate mixture model of Markov ##
          ########################################
          tabName = "simulateMarkov",
          box(
            width = 4,
            title = "Simulation options",
            status = "danger", solidHeader = TRUE,
            numericInput("nbComponent", "Mixture component", value = 2),
            numericInput("nbStateMix", "Number of states", min = 2, max = 10, value = 2),
            numericInput("nbSimuMix", "Number of individuals simulated", min = 1, value = 100),
            numericInput("TmaxMix", "Maximal duration of trajectories", min = 1, value = 10),
            uiOutput("probabilityGp"),
            box(
              width = 12, title = "Transition matrix", status = "danger",
              uiOutput("listMatrix")
            ),
            box(
              width = 12, title = "Sejourn time", status = "danger",
              uiOutput("listLambda")
            ),
            box(
              width = 12, title = "Initial law", status = "danger",
              uiOutput("listInitialLaw")
            ),
            actionButton("SimulateMixtureModel", "Simulate mixture model")
          ),
          conditionalPanel(
            "input.SimulateMixtureModel>0",
            tabBox(
              width = 8,
              tabPanel("dataset", dataTableOutput("headSimulatedMix")),
              tabPanel(
                "Plot of data",
                shinycssloaders::withSpinner(
                  plotOutput("trajSimulatedMix", height = "900px"),
                  type = getOption("spinner.type", default = 6),
                  color = getOption("spinner.color", default = "#d73925")
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "help",
          ############
          # 7 help  ##
          ############
          tabBox(
            width = 12,
            tabPanel(
              "Import data",
              p(
                "This part allows you to import a csv file. You have to choose a file, the separator and the decimal.
                If dataset doesn't appear check the file format."
              ),
              p(
                "After uploaded the file you can filter individuals by the number of jumps,
                the length of trajectories observed or choose the first n% individuals."
              )
            ),
            tabPanel(
              "Visualize data",
              p(
                "This part allows you to visualize the trajectories of the individuals selected in import part.
                Some graphics options are provided."
              )
            ),
            tabPanel(
              "Descriptive statistics",
              p(
                "In this part you can see the statistics provide by cfda package (number of jumps, summary of data,
                duration of trajectories and time spent in each state)."
              ),
              p(
                "If your dataset contains other variables that can be used as group variable you can choose them
                to compute all of this statistics by group variable."
              )
            ),
            tabPanel(
              "Estimation of markov chain",
              p(
                "In this part we assume than the data arise from a markov jump process and we compute all of the parameters
                of the model. You can also compute them by group variable."
              )
            ),
            tabPanel(
              "Analysis",
              p(
                "This part contains 2 analysis: Factorial analysis and clustering analysis.
                You have to perform a factorial analysis before using clustering functionality."
              ),
              h3("Factorial Analysis"),
              p(
                "You have to choose the ending time, the state(s) to extend, a NA state name and a basis of functions.
                The app computes the results and shows some graphics (factorial plan, eigen values, etc.)."
              ),
              h3("Clustering"),
              p(
                "After computing factorial analysis you can perform a clustering on principal component.
                You have to choose the method, the number of component or the percentage of variance."
              ),
              p(
                "After that the app displays the dendrogram and other plots to help you to analyse the different clusters."
              )
            ),
            tabPanel(
              "Simulate a mixture model",
              p("This part is used to simulate a mixture model of Markov."),
            )
          )
        )
      )
    )
  ),
  skin = "red"
)
