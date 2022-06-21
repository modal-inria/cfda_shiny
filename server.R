library(shiny)
library(cfda)
library("shinyMatrix")
library(tidyverse)
library(tractor.base)
library(dplyr)
library("aricode")
if (interactive()) {
  shinyServer(function(input, output) {
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
        need(is.numeric(data_import()[,"time"]), 'time must be numeric, please choose correct decimal symbol')
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
    
    ##Download data filter
    
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
        need(is.numeric(data_used()[,"time"]), 'time must be numeric, please choose correct decimal symbol')
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
        need(is.numeric(data_used()[,"time"]), 'time must be numeric, please choose correct decimal symbol')
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
        need(is.numeric(data_used()[,"time"]), 'time must be numeric, please choose correct decimal symbol')
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
    output$transitionMatData<-renderPrint({
      
    })
    
    ##Exponentiel law
    output$lambdaDataData<-renderPrint({
      
    })
    
    ##number of jump
    output$numberJumpDataTable<-renderPrint({
      
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
        data<-cut_data(data_used()[data_used()$id %in% idToKeep,c("id","time","state")],input$tpsmax)
        var<-which(!colnames(data_used()) %in% c("time","state"))
        choice=colnames(data_used())[var]
        unique(merge(data,data_used()[data_used()$id %in% idToKeep,c(var)],by="id"))
      })
    })
    
    output$summaryDataCFDA<-renderPrint({
      summary_cfd(data_CFDA()[,c("id","state","time")])
    })
    
    output$headDataCFDA<-renderDataTable({
      data_CFDA()
    })
    fmca<-reactive({
      input$soumettre
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
          fmca <- compute_optimal_encoding(data_CFDA()[,c("id","state","time")], basis)
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
      if(is.null(input$groupVariableFactorialPlan)){
        class<-NULL
      }else{
        if(input$groupVariableFactorialPlan!="NONE"){
          r<-unique(data_CFDA()[,c("id",input$groupVariableFactorialPlan)])
          class<-r[,input$groupVariableFactorialPlan] 
          class<-as.factor(class)
        }else{
          class<-NULL
        }
      }
      if(is.null(input$choix_dim1)){
        plotComponent(fmca(), comp = c(1, 2), addNames = FALSE)+ geom_point(aes(color=class)) + scale_fill_discrete(name = groupVariableFactorialPlan)
      }else{
      plotComponent(fmca(), comp = c(as.numeric(input$choix_dim1), as.numeric(input$choix_dim2)), addNames = FALSE)+geom_point(aes(color=class))+ labs(color = input$groupVariableFactorialPlan)
      }
    })
    
    
    output$groupVarFactorialPlan<-renderUI({
      var<-which(!colnames(data_used()) %in% c("id","time","state"))
      choice=colnames(data_used())[var]
      selectizeInput("groupVariableFactorialPlan","select a group variable",choice=c("NONE",choice))
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
      if(is.null(input$choix_dim1)){
        plot(fmca(),harm=1,addCI=input$addCI)
      }else{
        plot(fmca(),harm=as.numeric(input$choix_dim1),addCI=input$addCI)
      }

    })
    
    output$optimalEncoding1<-renderPlot({
      optimalEncodingPlot1()
    })
    
    optimalEncodingPlot2<-reactive({
      if(is.null(input$choix_dim2)){
        plot(fmca(),harm=2,addCI=input$addCI)
      }else{
      plot(fmca(),harm=as.numeric(input$choix_dim2),addCI=input$addCI)
      }
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
      if(is.null(input$choix_dim1)){
        minpc1 <- names(which(fmca()$pc[,1] <= quantile(fmca()$pc[,1], input$extremComp1/100)))
        maxpc1 <- names(which(fmca()$pc[,1] >= quantile(fmca()$pc[,1], 1-(input$extremComp1/100))))
        minpc2 <- names(which(fmca()$pc[,2] <= quantile(fmca()$pc[,2], input$extremComp2/100)))
        maxpc2 <- names(which(fmca()$pc[,2] >= quantile(fmca()$pc[,2], 1-(input$extremComp2/100))))
      }else{
        minpc1 <- names(which(fmca()$pc[,as.numeric(input$choix_dim1)] <= quantile(fmca()$pc[,as.numeric(input$choix_dim1)], input$extremComp1/100)))
        maxpc1 <- names(which(fmca()$pc[,as.numeric(input$choix_dim1)] >= quantile(fmca()$pc[,as.numeric(input$choix_dim1)], 1-(input$extremComp1/100))))
        minpc2 <- names(which(fmca()$pc[,as.numeric(input$choix_dim2)] <= quantile(fmca()$pc[,as.numeric(input$choix_dim2)], input$extremComp2/100)))
        maxpc2 <- names(which(fmca()$pc[,as.numeric(input$choix_dim2)] >= quantile(fmca()$pc[,as.numeric(input$choix_dim2)], 1-(input$extremComp2/100))))
      }
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
      plotData(data_CFDA()[,c("id","state","time")],group=group,addBorder = F,addId=F)+labs(title=paste("Extreme individuals on component",input$choix_dim1))
    
    })
    
    output$plotDataExtremAxe1<-renderPlot({
      plotDataExtremAxe1()
    })
    
    plotDataExtremAxe2<-reactive({
      ids <- unique(data_CFDA()$id)
      group <- factor(rep(NA, length(ids)), levels = c("lowest component values","highest component values"))
      group[ids %in% extremIndividuals()$minpc2] = "lowest component values"
      group[ids %in% extremIndividuals()$maxpc2] = "highest component values"
      plotData(data_CFDA()[,c("id","state","time")],group=group,addBorder = F,addId=F)+labs(title=paste("Extreme individuals on component",input$choix_dim2)) 
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
    
    ##Stats de base par rpport au groupe 
    results_CFDA<-reactive({
      factorialAnalys<-list(fmca=fmca(),encoding=get_encoding(fmca()))
      list(factorialAnalys=factorialAnalys,dataSetUsed=data_used()[data_used()$id %in% data_CFDA()$id,],dataSetCut=data_CFDA())
    })
    
    ##Importer les resultats des cluster
    ##Groupe 
    
    output$downloadResultsCFDA <- downloadHandler(
      filename = function() {
        paste("resFactorialAnalysis", ".rds", sep = "")
      },
      content = function(file) {
        saveRDS(results_CFDA(),file)
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
    
    class<-reactive({
      class <- cutree(hc(), k = input$nbclust)
  
    })
    groupe<-reactive({
      plotData(data_CFDA()[,c("id","state","time")], group = class(), addId = FALSE, addBorder = FALSE, sort = TRUE)
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
    
    results_CAH<-reactive({
      dataCAH<-unique(merge(data_CFDA(),cbind.data.frame(id=names(class),class()),by="id"))
      dataByGroup<-list()
      for(i in 1:input$nbclust){
        data<-data_CFDA()[data_CFDA()$id %in% names(class),]
        dataByGroup<-append(dataByGroup,list(data)) 
      }
      names(dataByGroup)=paste("Group",1:input$nbclust,sep="")
      list(dataset=dataCAH,dataByGroup=dataByGroup,cluster=list(class()))
    })
    
    output$downloadResultsCAH <- downloadHandler(
      filename = function() {
        paste("resClustering", ".rds", sep = "")
      },
      content = function(file) {
        saveRDS(results_CAH(),file)
      }
    )
  })
}