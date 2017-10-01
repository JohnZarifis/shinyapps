library(shiny)
library(shinydashboard)
library(RODBC)
library(ggplot2)
library(plotly)
library(mlr)
library(dplyr)
library(gamlss)
library(shinycssloaders)
library(shinyBS)
library(Cairo)
#library(ggiraph)

dbhandle <- odbcDriverConnect("driver={SQL Server Native Client 11.0};server= 10.1.1.120;Database=AquaGrowth3;Uid=sa;Pwd=1q2w3e!Q@W#E")

Sampling <- sqlQuery(dbhandle, "SELECT 
                     P.Designation AS REGION
                     ,D.[CageLotID]
                     ,convert(varchar(50),D.[CageLotID]) + convert(varchar(8),CLTranTransDate,112) AS uniqueID
                     ,[CLTranTransDate] AS TRANSACTIONDATE
                     ,convert(varchar(8),CLTranTransDate,112) AS DATEKEY
                     ,[TransactionMAB] AS AvgWeight
                     ,G.PopulationDate
                     ,DATEDIFF(day,G.PopulationDate,CLTranTransDate) as Age
                     ,YEAR(PopulationDate) ORIGINYEAR
                     ,MONTH(PopulationDate) ORIGINMONTH
                     ,S.Designation as SPECIE
                     ,L.Designation as LOT
                     ,R.Designation AS GRADING
                     ,H.Designation as HATCHERY
                     ,L.MAB AS LotStartAvgWeight
                     ,'Sampling' as TypeOfMeasure
                     ,'Included' as ToGraph
                     FROM [Aquagrowth3].[dbo].[CLTransDetails] D
                     inner join [Aquagrowth3].[dbo].CageLot G
                     on G.CageLotID = D.CageLotID
                     inner join [Aquagrowth3].[dbo].LOT L
                     on L.LotID = G.LotID
                     inner join [Aquagrowth3].[dbo].Species S
                     on S.SpeciesID = L.SpeciesID
                     inner join [Aquagrowth3].[dbo].GradingCategory R
                     on R.GradingID = G.CageLotGradingID
                     INNER JOIN [Aquagrowth3].[dbo].[HATCHERY] H
                     ON H.HatcheryID = L.HatcheryID
                     INNER JOIN [Aquagrowth3].[dbo].[CAGE] C
                     ON C.CAGEID = G.CAGEID
                     INNER JOIN [Aquagrowth3].[dbo].[Site] ST
                     ON ST.SiteID = C.SiteID
                     INNER JOIN [Aquagrowth3].[dbo].[SiteGroup] P
                     ON P.SiteGroupID = ST.SiteGroupID
                     where CLTranTransKindID = 4 
                     and IsVirtualSampling = 0 
                     and PopulationDate > '2013-12-31'
                     and YEAR(L.PurchaseDate) > 2013",stringsAsFactors = FALSE)
odbcClose(dbhandle)


Sampling$Prefatening <- ifelse(Sampling$LotStartAvgWeight < 5 , 'Not PreFatened','Prefatened')
Sampling$GPD <-  Sampling$AvgWeight/Sampling$Age



label.help <- function(label,id){
  HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}










ui <- fluidPage(
  actionButton("go", "Go"),
  numericInput("n", "n", 50)
  #plotOutput("plot")
  ,verbatimTextOutput("info")
)

server <- function(input, output) {
  
  
  values <- reactiveValues(
    ## keeprows = rep(TRUE, nrow(Sampling))
    
  )
  
  
   randomVals <- eventReactive(input$go, {
  #   
     values$keep <- filter(Sampling,SPECIE =='ΛΑΒΡΑΚΙ' & ORIGINMONTH == 5 & AvgWeight <= 500)
     
     #pred <- gamlss(AvgWeight~pb(Age) ,sigma.fo=~pb(Age),data=keep, method=mixed(20,50), n.cyc=50 ,trans.x=TRUE)
  #   
  #   # #a <-    gamlss(AvgWeight~pb(Age) ,sigma.fo=~pb(Age),data=SamplingTsipoyra , family=BCT)
  #   # 
  #   # # with(keep
  #   # #      ,centiles(pred,Age)
  #   # # )
  #   #
     newx<-seq(1,500,1)
  # 
     
  #    mat <- centiles.pred(pred
  #                      , xname="Age", xvalues=newx )
  # #   #print(mat)
  #    inner_join(keep,mat)
  #    
  #runif(50)
     
     #Sampling%>%filter(ToGraph =='Included') 
     # testt<- keep %>% 
     #   gamlss(AvgWeight~pb(Age) ,sigma.fo=~pb(Age),data= ., method=mixed(20,50), n.cyc=50 ,trans.x=TRUE)
     # 
     keep <- values$keep
     values$testt <- gamlss(AvgWeight~pb(Age) ,sigma.fo=~pb(Age),data= keep, method=mixed(20,50), n.cyc=50 ,trans.x=TRUE)
     
     
       
       #t<- centiles(testt,xvar=keep$Age)
       
       mat <- centiles.pred(#values$
                              testt,xname="Age", xvalues=newx)
      
       ##########################################
       ## bring the data and fit the model
       
       #a<-gamlss(y~pb(x),sigma.fo=~pb(x), data=abdom, family=BCT)
       ## plot the centiles
       #centiles(a,xvar=abdom$x)
       ##-----------------------------------------------------------------------------
       ## first use of  centiles.pred()
       ## to calculate the centiles at new x values
       ##-----------------------------------------------------------------------------
       #newx<-seq(12,40,2)
       #mat <- centiles.pred(a, xname="x", xvalues=newx )
       #mat
      y<- inner_join(keep,mat)
       
       
       
       
       
       
     
     
  })
  
  # output$plot <- renderPlot({
  #   
  #   hist(randomVals())
  # })
  output$info <- renderPrint({
    #nearPoints(values$plotData, input$Yplot_click, allRows = FALSE)
    #View(t)
    randomVals()
  })
}

shinyApp(ui, server)

