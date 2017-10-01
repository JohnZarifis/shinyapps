shinyServer(function(input, output, session){
  values <- reactiveValues()
  # SingleFCRTable <- reactive({
  # FCR <- filter(FCR,TemplateName == input$Template & Temp %in% c( as.numeric(input$Temp[1]):as.numeric(input$Temp[2]) ))  
  # #View(FCR2)
  # #return(FCR2)
  # 
  # 
  # })
  
  observe({
    if (input$tabs == "Stop") {
      session$sendCustomMessage(type = "closeWindow", message = "message")
      stopApp()
    }
  })
  
  observeEvent({  
    input$Template
    input$Temp
       }, 
      { values$FCR <- 
        filter(FCR,TemplateName == input$Template & Temp %in% c( as.numeric(input$Temp[1]):as.numeric(input$Temp[2]) ))
       } )
  
  
  output$highchart <- renderHighchart({
    
    validate(
      need(input$Template %in%  FCR$TemplateName
           
           ,"Please Choose a Table.")
    )  
    
    FCR <- values$FCR#SingleFCRTable()
    # IF input false ....
    #FCR <- filter(FCR,TemplateName == input$Template & Temp %in% c( as.numeric(input$Temp[1]):as.numeric(input$Temp[2]) ))  
    
    Lhc <- hchart( FCR, input$TypeOfChart,hcaes(x = Temp,y = Factor, group = WeightCategory),draggableY = TRUE 
                   
    )
    if (input$ToolTip)
    Lhc <- Lhc %>% hc_tooltip( backgroundColor = "#FCFFC5", shared = TRUE, borderWidth = 1 )
    Lhc %>% 
      hc_exporting(
        enabled = TRUE
      ) %>%
      
     # hc_tooltip(shared = FALSE,
     #            formatter = JS("function () { return '<b>' + this.series.name + ', age ' + this.point.category + '</b><br/>' + 'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}")
      #) %>%
     hc_plotOptions(
      series = list(
        point = list(
          events = list(
            drop = JS("function(){
                        //console.log(this.series.data)
                        //window.data = _.map(this.series.data, function(e) { return e.Factor })
                       window.data = this.series.data.map(function(e) { 
                                                                        return {
                                                                     WeightCategory : e.WeightCategory,
                                                                     FromWeight : e.FromWeight,
                                                                     ToWeight : e.ToWeight,
                                                                     Temp : e.Temp,
                                                                     Factor : e.y,
                                                                     TemplateName : e.TemplateName
                                                                     
                                                                         } })
                       ////test = window.data()
                       ////console.log(test.Factor)
                       //console.log(window.data)
                       ////console.log(data)
                       ////console.log(this.y)
                       
                      Shiny.onInputChange('inputname', data);
  }"))
          )))
    # if(input$ToolTip){
    #   Lhc %>% hc_tooltip( backgroundColor = "#FCFFC5",
    #               shared = TRUE, borderWidth = 1 )
    # } else{
    #   Lhc
    # }
    
    
  })
  
  
  # observer for debugging reasons
  # observe({
  #   cat(str(input$inputname))
  #   
  #   dataset <- input$inputname
  #   #View(dataset)
  #   #dataset2<- data.frame(as.list(dataset))
  #   #dataset2<- data.frame('Factor'=dataset[Factor], 'Temp'=dataset[Temp], 'FromWeight' = dataset[FromWeight])
  #   if(!is.null(dataset)){
  #   dataset2 <- data.frame(keyName=names(dataset), value=dataset , row.names = NULL, stringsAsFactors = FALSE ) 
  #   #View(dataset2)
  #   dataset2$row <- rep(seq_len(nrow(dataset2)/6), each=6)
  #   dataset3 <- spread(dataset2,keyName,value)
  #   dataset3$Factor = as.numeric(dataset3$Factor)
  #   dataset3$FromWeight = as.numeric(dataset3$FromWeight)
  #   dataset3$Temp = as.integer(dataset3$Temp)
  #   dataset3$ToWeight = as.numeric(dataset3$ToWeight)
  #   
  #   View(dataset3)
  #   print(str(dataset3))
  #   FCR <<- filter(FCR #,TemplateName != unique(as.character(dataset3$TemplateName) )
  #                 , FromWeight != unique(dataset3$FromWeight)# && as.character(TemplateName) != unique(as.character(dataset3$TemplateName) )
  #                 
  #                 ) #"ΤΣΙΠΟΥΡΑ 2017")   # unique(dataset3$TemplateName) )
  #   # View(FCR)
  #   FCR <- rbind(FCR,select(dataset3,-row))
  #   print(str(FCR))
  #   #View(FCR)
  #   ## FCR <- FCR[   apo to fcr afairo tis palies vazo tis nees
  #   
  #   }
  #   
  #   View(FCR)
  #   
  #   
  #   #View(dataset2)
  #   #print(dataset2)
  #   #print(dataset[Temp])
  #  })
  
  output$table <- DT::renderDataTable({
    #FCR <- values$FCR#SingleFCRTable()
    #values <- reactiveValues(values$FCR2 = FCR)
    #values$FCR2 <- FCR
    #values$FCR2 <- SingleFCRTable$FCR2
    #FCR2 <- SingleFCRTable()
    dataset <- input$inputname
    if(!is.null(dataset)){
      
      dataset2 <- data.frame(keyName=names(dataset), value=dataset , row.names = NULL, stringsAsFactors = FALSE ) 
      #View(dataset2)
      dataset2$rowno <- rep(seq_len(nrow(dataset2)/6), each=6)
      dataset3 <- spread(dataset2,keyName,value)
      dataset3$Factor =  round(as.numeric(dataset3$Factor), digits = 3) 
      dataset3$FromWeight = as.numeric(dataset3$FromWeight)
      dataset3$Temp = as.integer(dataset3$Temp)
      dataset3$ToWeight = as.numeric(dataset3$ToWeight)
      
      #View(dataset3)
      #print(str(dataset3))
      FCR2 <- filter(values$FCR #,TemplateName != unique(as.character(dataset3$TemplateName) )
                     , FromWeight != unique(dataset3$FromWeight) #&& as.character(TemplateName) != unique(as.character(dataset3$TemplateName) )
      
                    
      ) #"ΤΣΙΠΟΥΡΑ 2017")   # unique(dataset3$TemplateName) )
      #View(values$FCR2)
      values$FCR <- rbind(FCR2,select(dataset3,-rowno))
      #print(str(values$FCR))
      if(input$DataFormat == "Raw Data"){
        values$FCR
      }
      else{
        fcrTable <- select(values$FCR,Temp,FromWeight,Factor)
        fcrTable <- spread(fcrTable,Temp,Factor)
        
      }
      
      #View(fcrTable)
      ## FCR <- FCR[   apo to fcr afairo tis palies vazo tis nees
      
    }
    
    #View(FCR)
    #FCR 
    
    
    #)
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
    scrollY = 400,
    scroller = TRUE

    
    
    
    )
    
  )
  
  output$tableFCR <- DT::renderDataTable({
    
    FCRTable 
  })
  

  
  
})