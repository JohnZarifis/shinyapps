


server <- function(input, output, session) { 
  
  # # For storing which rows have been excluded
  #  vals <- reactiveValues (
  #   keeprows = rep(TRUE, nrow(Sampling))
  # )
   values <- reactiveValues(
    ## keeprows = rep(TRUE, nrow(Sampling))
     
   )
   
   
   
   observeEvent({  
     input$Specie
     input$Grading
     input$StockingMonth
     input$AvgWeight
     input$Region
     #input$GraphColor
   }, 
   {
     values$plotData <- 
       subset(Sampling, SPECIE %in% c(input$Specie) 
                         & GRADING %in% c(input$Grading)
                         & ORIGINMONTH %in% c(input$StockingMonth)
                         & REGION %in% c(input$Region)
                         & AvgWeight  <=  as.numeric(input$AvgWeight)
                       
                        , drop = FALSE
                        
                        )
     
     #keeprows = rep(TRUE, nrow(values$plotData))
     # filter(Sampling, SPECIE %in% c(input$Specie) & GRADING %in% c(input$Grading)
     #        & ORIGINMONTH %in% c(input$StockingMonth)
     #        & AvgWeight  <=  as.numeric(input$AvgWeight), drop = FALSE)
     # 
      
   } 
   
   
   )
  
  
  # plotData <- reactive({
  #   
  #   
  #   subset(Sampling, SPECIE %in% c(input$Specie) 
  #           & GRADING %in% c(input$Grading)
  #           & ORIGINMONTH %in% c(input$StockingMonth)
  #           & AvgWeight  <=  as.numeric(input$AvgWeight)
  #          
  #          , drop = FALSE
  #          
  #          )
  #   
  #   })
  
  
   #Toggle points that are clicked
   observeEvent(input$Yplot_click, {
     
      t <- values$plotData
     
     #res <- nearPoints(t, input$Yplot_click, allRows = TRUE)
     atomic <- nearPoints(t, input$Yplot_click, allRows = FALSE)
     
       
      ##values$keeprows <- xor(values$keeprows, res$selected_)
      #values$plotData %>% mutate(ToGraph = ifelse(uniqueID == atomic$uniqueID,'Excluded', ToGraph))
     values$plotData <- within(t, ToGraph[uniqueID %in% c(atomic$uniqueID)] <- ifelse(atomic$ToGraph =='Included','Excluded','Included'))
      
      #View(atomic)

   })
   
   output$info <- renderPrint({
     #nearPoints(values$plotData, input$Yplot_click, allRows = FALSE)
     #View(t)
     curveVals()
   })
    
  
  output$Yplot <- renderPlot({
    
    #kp <- values$plotData
    #xd <- values$exclude
    
    dt <- values$plotData
    
    #View(dt)
    
    # Plot the kept and excluded points as two separate data sets
    keep    <- filter(dt,ToGraph =='Included')   ## <-  dt[ values$keeprows, , drop = FALSE]
    exclude <- filter(dt,ToGraph =='Excluded') ##<-  dt[!values$keeprows, , drop = FALSE]
    #keep <- filter(kp,!(uniqueID %in% xd$uniqueID ))
    #View(exclude)
    #View(keep)
    
     p <- ggplot() #+ geom_point(data = keep,aes_string("Age","AvgWeight",color = ifelse(input$GraphColor != 'Nothing',  input$GraphColor,1) )) #+
       #color = ifelse(input$GraphColor != 'Nothing',parse(text= shQuote(input$GraphColor)),"black" )) +
        if(input$Smoother){
       p <- p +  geom_smooth(data = keep,aes(Age,AvgWeight)) #+ theme_hc() + scale_colour_hc()
          }
         if(input$GraphColor != 'Nothing'){
        #   #test <- levels(as.factor(input$GraphColor))
        #  
          
           p <-  p +  geom_point(data = keep,aes_string("Age","AvgWeight",color = input$GraphColor  ))
          # p <- p + theme(legend.position="none")
         } else {
           p <-  p +geom_point(data = keep,aes(Age,AvgWeight))
           
         }
          p <- p + geom_point(data = exclude, shape = 21, fill = NA, color = "red", alpha = 0.8,aes(Age,AvgWeight)) +
       labs(x = "Age (Days)", y = 'Average Weight(g)') + xlim(0,max(dt$Age)+10)+ ylim(0,max(dt$AvgWeight) + 10)
    
      # if(input$GraphColor != 'Nothing'){
      #   #test <- levels(as.factor(input$GraphColor))
      #  
      #   p <- p + aes_string(colour = input$GraphColor)
      # }
   
     if(length(input$StockingMonth) == 1){
       dtw <- winterDays(as.integer(input$StockingMonth))
       print(dtw)
       p  <- p + geom_rect(data = dtw, aes(xmin =startDate, xmax = endDate, ymin = 0 , ymax = Inf),fill='cyan', alpha=0.2)
     }
     p 
       
    #ggplot(dt, aes(Age,AvgWeight)) + geom_point(aes(colour = ORIGINYEAR )) + geom_smooth() +
     # labs(x = "Age in Days", y = 'Average Weight in g.') + xlim(0,max(dt$Age)+10)+ ylim(0,max(dt$AvgWeight) + 10)
    
  })
  
  
  output$Outliers <- DT::renderDataTable({
    dt <- values$plotData
    exclude <- filter(dt,ToGraph =='Excluded')
    if(!is.null(exclude)){
      
      exclude[,c('REGION','TRANSACTIONDATE','AvgWeight','Age','GPD','LotStartAvgWeight','PopulationDate','HATCHERY','LOT','GRADING','ORIGINMONTH','TypeOfMeasure')]
    }
  }, extensions = c('Buttons','Scroller'),
  filter = 'bottom',
  server = FALSE,   # False in order to see all rows of data, not only visible.
  options = list(
    pageLength = "ALL",
    #lengthMenu = c(5, 10, 15, 20),
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    rownames = FALSE,
    deferRender = TRUE,
    scrollY = 200,
    scroller = TRUE )
  
  )
  
  curveVals <- eventReactive(input$modelButton, {
    
    #dt   <- values$plotData
    #keep <- filter(dt,ToGraph =='Included')
    keep <- values$plotData %>% filter(ToGraph =='Included')
    #View(keep)
    #keep$tAge <- keep$Age
    m0 <- lms(AvgWeight,Age, data= keep, trans.x=TRUE, k=2)
    
    #m0$family
    #m0$power
    
    keep$tAge <- (keep$Age)^(m0$power)
    keep<<- keep
    #data(keep)
    #values$gamlssMod <- gamlss(AvgWeight~pb(TAge) ,sigma.fo=~pb(Age),data = keep, method=mixed(20,50), n.cyc=50 ,trans.x=TRUE)
    
    values$gamlssMod <- gamlss(AvgWeight~pb(Age)
                                ,sigma.formula =~pb(Age)
                                ,nu.formula = ~pb(Age)
                                ,family = m0$family #BCPEo
                                ,trace = FALSE
                                ,data = keep)


    # #a <-    gamlss(AvgWeight~pb(Age) ,sigma.fo=~pb(Age),data=SamplingTsipoyra , family=BCT)
    # 
    # # with(keep
    # #      ,centiles(pred,Age)
    # # )
    # 
    #newx<-seq(1, as.integer(input$AvgWeight),1)
    newx <- unique(keep$Age)
    mat <- centiles.pred(values$gamlssMod, xname="Age", xvalues=newx )
    #View(mat)
    final <- inner_join(keep,mat)
    #View(final)
    return(final)
    
  })
  
  output$CurvesInt <-  renderPlotly ({ #    renderPlot({
    

    curveVals<- curveVals()
    selectedLOT <- filter(curveVals,curveVals$LOT %in% input$lot)

    g <- ggplot(data = curveVals) +
      #geom_point(aes(Age,AvgWeight),
      #           colour = ifelse( curveVals$AvgWeight > curveVals$C98 |curveVals$AvgWeight < curveVals$C2 ,"red","blue"),
      #           shape = ifelse( curveVals$LOT %in% input$lot ,1,16)) +
      geom_line(aes(Age,C98),colour = "red",size=1, stat="identity",linetype="dashed") +
      geom_line(aes(Age,C90),colour = "orange",size=1, stat="identity",linetype="dashed") +
      geom_line(aes(Age,C75),colour = "orange",size=1, stat="identity",linetype="dashed") +
      geom_line(aes(Age,C50),colour = "dimgray",size=1.1, stat="identity") +
      geom_line(aes(Age,C25),colour = "orange",size=1, stat="identity",linetype="dashed") +
      geom_line(aes(Age,C10),colour = "orange",size=1, stat="identity",linetype="dashed") +
      geom_line(aes(Age,C2),colour = "red",size=1, stat="identity",linetype="dashed") +
      labs(x = "Age (Days)", y = 'Average Weight(g)') 
      if(nrow(selectedLOT)>0){
       g<- g + geom_point(data = selectedLOT,aes(Age,AvgWeight,color= selectedLOT$LOT, text = paste('Lot: ', selectedLOT$LOT ,'\nCage: ', selectedLOT$CAGENAME)),
                   shape = 1  ) + scale_color_discrete(name  ="Lot Name")
        
      }
     
    g<- g + ggtitle(paste('Growth Curves for: ',input$Specie,' Grading: ',paste(input$Grading,collapse = ','), ' Stocking Month: '
                          , paste(input$StockingMonth, collapse = ',') ))
    
    
      if(input$ShowSmooth){
        g <- g +  geom_smooth(aes(Age,AvgWeight)) #+ theme_hc() + scale_colour_hc()
      }
       if(input$ShowPoints){
      g <- g +  geom_point(aes(Age,AvgWeight),
                           colour = ifelse( curveVals$AvgWeight > curveVals$C98 |curveVals$AvgWeight < curveVals$C2 ,"red","darkcyan"),
                           shape = ifelse( curveVals$LOT %in% input$lot ,1,16))
       }
      #if(input$ShowInteractive == 'Interactive'){
        g<- ggplotly(g)
      #}else{
       # g
      #}
      

    
    
  })
  
  
  
  
  output$Curves <-  renderPlot({
    
    
    curveVals<- curveVals()
    selectedLOT <- filter(curveVals,curveVals$LOT %in% input$lot)
    
    g <- ggplot(data = curveVals) +
      #geom_point(aes(Age,AvgWeight),
      #           colour = ifelse( curveVals$AvgWeight > curveVals$C98 |curveVals$AvgWeight < curveVals$C2 ,"red","blue"),
      #           shape = ifelse( curveVals$LOT %in% input$lot ,1,16)) +
      geom_line(aes(Age,C98),colour = "red",size=1, stat="identity",linetype="dashed") +
      geom_line(aes(Age,C90),colour = "orange",size=1, stat="identity",linetype="dashed") +
      geom_line(aes(Age,C75),colour = "orange",size=1, stat="identity",linetype="dashed") +
      geom_line(aes(Age,C50),colour = "dimgray",size=1.1, stat="identity") +
      geom_line(aes(Age,C25),colour = "orange",size=1, stat="identity",linetype="dashed") +
      geom_line(aes(Age,C10),colour = "orange",size=1, stat="identity",linetype="dashed") +
      geom_line(aes(Age,C2),colour = "red",size=1, stat="identity",linetype="dashed") +
      labs(x = "Age (Days)", y = 'Average Weight(g)') 
    if(nrow(selectedLOT)>0){
      g<- g + geom_point(data = selectedLOT,aes(Age,AvgWeight, color= selectedLOT$LOT),
                         shape = 1  ) + scale_color_discrete(name  ="Lot Name")
      # + geom_text_repel(data = selectedLOT,aes(Age,AvgWeight), label =selectedLOT$LOT)
      
    }
    
    g<- g + ggtitle(paste('Growth Curves for: ',input$Specie,' Grading: ',paste(input$Grading,collapse = ','), ' Stocking Month: '
                          , paste(input$StockingMonth, collapse = ',') ))
    
    
    if(input$ShowSmooth){
      g <- g +  geom_smooth(aes(Age,AvgWeight)) #+ theme_hc() + scale_colour_hc()
    }
    if(input$ShowPoints){
      g <- g +  geom_point(aes(Age,AvgWeight),
                           colour = ifelse( curveVals$AvgWeight > curveVals$C98 |curveVals$AvgWeight < curveVals$C2 ,"red","darkcyan"),
                           shape = ifelse( curveVals$LOT %in% input$lot ,1,16))
    }
     g
   
    
    
  })
  
  
  output$lotName <- renderUI({
    
    #curveVals<- curveVals()
    
    dt   <- values$plotData
    curveVals <- filter(dt,ToGraph =='Included')
    
    selectizeInput(
      'lot', label = 'Choose Lot', choices = c(unique(as.character( curveVals$LOT ))),
        selected = NULL #c(unique(as.character(keep$Lot )))
      , multiple = TRUE
      
    )
    
  })
  
  
  output$lots <- renderUI({
    
   
    
    dt   <- values$plotData
    
    
    selectizeInput(
      'lots', label = 'Choose Lot', choices = c(unique(as.character( dt$LOT ))),
      selected = NULL #c(unique(as.character(keep$Lot )))
      , multiple = TRUE
      
    )
    
  })
  
  
  
  
  # output$testFit <- renderPrint({
  #   #nearPoints(values$plotData, input$Yplot_click, allRows = FALSE)
  #   #View(t)
  #   #curveVals()
  #   SamplingRegr <- keep %>% dplyr::select(Age,AvgWeight,ORIGINMONTH ,ORIGINYEAR,LotStartAvgWeight,REGION)
  #   SamplingRegr$REGION <- as.factor(SamplingRegr$REGION)
  #   regr.task = makeRegrTask(id = "bh", data = SamplingRegr, target = "AvgWeight")
  #   regr.lrn = makeLearner("regr.bartMachine")
  #   mod = train(regr.lrn, regr.task)
  #   task.pred = predict(mod, task = regr.task)
  #   testOutput <- cbind(keep,task.pred$data)
  #   
  #   
  #   
  # })
  # 


  
  
    }
    
    
    



