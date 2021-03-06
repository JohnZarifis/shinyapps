library(shiny)
library(shinydashboard)
library(RODBC)
library(dplyr)
library(ggplot2)
library(plotly)
library(mlr)
library(gamlss)
library(shinycssloaders)
library(shinyBS)
library(Cairo)
library(ggthemes)
library(ggrepel)
#library(ggiraph)

#dbhandle <- odbcDriverConnect("driver={SQL Server Native Client 11.0};server= 10.1.1.120;Database=AquaGrowth3;Uid=sa;Pwd=1q2w3e!Q@W#E")
           # odbcDriverConnect('Driver=FreeTDS;TDS_Version=7.0;Server=<server>;Port=<port>;Database=<db>;Uid=<uid>;Pwd=<pw>;Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;')
dbhandle <- odbcDriverConnect('driver=FreeTDS;TDS_Version=7.3;Server=10.1.1.120;Port=1433;Database=AquaGrowth3;Uid=sa;Pwd=1q2w3e!Q@W#E')


Sampling <- sqlQuery(dbhandle, "SELECT    ROW_NUMBER() OVER ( PARTITION BY D.CageLotID order by D.CageLotID,CLTranTransDate) AS ROWNUM,
                     P.Designation AS REGION
                     ,D.[CageLotID]
                     ,convert(varchar(50),D.[CageLotID]) + convert(varchar(8),CLTranTransDate,112) AS uniqueID
                     ,c.designation as CAGENAME
                     ,c.designation + ' | ' + L.Designation as CAGELOT
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
                     and YEAR(L.PurchaseDate) > 2013


                     UNION ALL

                     SELECT
                     ROW_NUMBER() OVER ( PARTITION BY D.CageLotID order by D.CageLotID,CLTranTransDate) AS ROWNUM,
                     P.Designation AS REGION
                     ,D.[CageLotID]
                     ,convert(varchar(50),D.[CageLotID]) + convert(varchar(8),CLTranTransDate,112) AS uniqueID
                     ,c.designation as CAGENAME
                     ,c.designation + ' | ' + L.Designation as CAGELOT
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
                     ,'Harvest' as TypeOfMeasure
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
                     where CLTranTransKindID = 8 and IsSampling = 1
                     and IsSampling = 1
                     and PopulationDate > '2013-12-31'
                     and YEAR(L.PurchaseDate) > 2013",stringsAsFactors = FALSE)
odbcClose(dbhandle)



#  dbhandle <- odbcDriverConnect("driver={SQL Server Native Client 11.0};server= WIN8-PATRA;Database=AquaGrowthFilo;Uid=sa;Pwd=1q2w3e!Q@W#E")
#  
#  Sampling <- sqlQuery(dbhandle, "SELECT    ROW_NUMBER() OVER ( PARTITION BY D.CageLotID order by D.CageLotID,CLTranTransDate) AS ROWNUM,
# P.Designation AS REGION
# ,D.[CageLotID]
# ,convert(varchar(50),D.[CageLotID]) + convert(varchar(8),CLTranTransDate,112) AS uniqueID
# ,c.designation as CAGENAME
# ,c.designation + ' | ' + L.Designation as CAGELOT
# ,[CLTranTransDate] AS TRANSACTIONDATE
# ,convert(varchar(8),CLTranTransDate,112) AS DATEKEY
# ,[TransactionMAB] AS AvgWeight
# ,G.PopulationDate
# ,DATEDIFF(day,G.PopulationDate,CLTranTransDate) as Age
# ,YEAR(PopulationDate) ORIGINYEAR
# ,MONTH(PopulationDate) ORIGINMONTH
# ,S.Designation as SPECIE
# ,L.Designation as LOT
# ,R.Designation AS GRADING
# ,H.Designation as HATCHERY
# ,L.MAB AS LotStartAvgWeight
# ,'Sampling' as TypeOfMeasure
# ,'Included' as ToGraph
# FROM [AquaGrowthFilo].[dbo].[CLTransDetails] D
# inner join [AquaGrowthFilo].[dbo].CageLot G
# on G.CageLotID = D.CageLotID
# inner join [AquaGrowthFilo].[dbo].LOT L
# on L.LotID = G.LotID
# inner join [AquaGrowthFilo].[dbo].Species S
# on S.SpeciesID = L.SpeciesID
# inner join [AquaGrowthFilo].[dbo].GradingCategory R
# on R.GradingID = G.CageLotGradingID
# INNER JOIN [AquaGrowthFilo].[dbo].[HATCHERY] H
# ON H.HatcheryID = L.HatcheryID
# INNER JOIN [AquaGrowthFilo].[dbo].[CAGE] C
# ON C.CAGEID = G.CAGEID
# INNER JOIN [AquaGrowthFilo].[dbo].[Site] ST
# ON ST.SiteID = C.SiteID
# INNER JOIN [AquaGrowthFilo].[dbo].[SiteGroup] P
# ON P.SiteGroupID = ST.SiteGroupID
# where CLTranTransKindID = 4 
# and IsVirtualSampling = 0 
# and PopulationDate > '2013-12-31'
# and YEAR(L.PurchaseDate) > 2013
# and LEFT(L.Designation,2) NOT IN ('12','13')
# 
# 
# UNION ALL
# 
# SELECT 
# ROW_NUMBER() OVER ( PARTITION BY D.CageLotID order by D.CageLotID,CLTranTransDate) AS ROWNUM,
# P.Designation AS REGION
# ,D.[CageLotID]
# ,convert(varchar(50),D.[CageLotID]) + convert(varchar(8),CLTranTransDate,112) AS uniqueID
# ,c.designation as CAGENAME
# ,c.designation + ' | ' + L.Designation as CAGELOT
# ,[CLTranTransDate] AS TRANSACTIONDATE
# ,convert(varchar(8),CLTranTransDate,112) AS DATEKEY
# ,[TransactionMAB] AS AvgWeight
# ,G.PopulationDate
# ,DATEDIFF(day,G.PopulationDate,CLTranTransDate) as Age
# ,YEAR(PopulationDate) ORIGINYEAR
# ,MONTH(PopulationDate) ORIGINMONTH
# ,S.Designation as SPECIE
# ,L.Designation as LOT
# ,R.Designation AS GRADING
# ,H.Designation as HATCHERY
# ,L.MAB AS LotStartAvgWeight
# ,'Harvest' as TypeOfMeasure
# ,'Included' as ToGraph
# FROM [AquaGrowthFilo].[dbo].[CLTransDetails] D
# inner join [AquaGrowthFilo].[dbo].CageLot G
# on G.CageLotID = D.CageLotID
# inner join [AquaGrowthFilo].[dbo].LOT L
# on L.LotID = G.LotID
# inner join [AquaGrowthFilo].[dbo].Species S
# on S.SpeciesID = L.SpeciesID
# inner join [AquaGrowthFilo].[dbo].GradingCategory R
# on R.GradingID = G.CageLotGradingID
# INNER JOIN [AquaGrowthFilo].[dbo].[HATCHERY] H
# ON H.HatcheryID = L.HatcheryID
# INNER JOIN [AquaGrowthFilo].[dbo].[CAGE] C
# ON C.CAGEID = G.CAGEID
# INNER JOIN [AquaGrowthFilo].[dbo].[Site] ST
# ON ST.SiteID = C.SiteID
# INNER JOIN [AquaGrowthFilo].[dbo].[SiteGroup] P
# ON P.SiteGroupID = ST.SiteGroupID
# where CLTranTransKindID = 8 and IsSampling = 1
# and IsSampling = 1
# and PopulationDate > '2013-12-31'
# and YEAR(L.PurchaseDate) > 2013
# and LEFT(L.Designation,2) NOT IN ('12','13')",stringsAsFactors = FALSE)
# 
#  odbcClose(dbhandle)




#saveRDS(Sampling, "./DemoData/Sampling.rds")
# 

#Sampling <- readRDS("./DemoData/Sampling.rds")



Sampling$Prefatening <- ifelse(Sampling$LotStartAvgWeight < 5 , 'Not PreFatened','Prefatened')
Sampling$GPD <- round(Sampling$AvgWeight/Sampling$Age, digits = 2)

#Sampling <- Sampling %>% mutate(FirstHarvest = ifelse(TypeOfMeasure == 'Harvest' & ROWNUM == 1, 'FirstHarvest','Rest' ))

Sampling <- Sampling %>%filter((TypeOfMeasure == 'Harvest' & ROWNUM == 1)|TypeOfMeasure == 'Sampling')

Sampling$tAge <- Sampling$Age



label.help <- function(label,id){
  HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}


winterDays <- function(stockingMonth) {
  switch(stockingMonth,
         data.frame(startDate = c(0,360,720),endDate = c(90,450,810) ),
         data.frame(startDate = c(0,330,690),endDate = c(60,420,790)),
         data.frame(startDate = c(0,300,660),endDate = c(30,390,760)),
         data.frame(startDate = c(270,630,990),endDate = c(360,720,1080)),
         data.frame(startDate = c(240,600,960),endDate = c(330,690,1050)),
         data.frame(startDate = c(210,570,930),endDate = c(300,660,1020)),
         data.frame(startDate = c(180,540,900),endDate = c(270,630,990)),
         data.frame(startDate = c(150,510,870),endDate = c(240,600,960)),
         data.frame(startDate = c(120,480,840),endDate = c(210,570,930)),
         data.frame(startDate = c(90,450,810),endDate = c(180,540,900)),
         data.frame(startDate = c(60,420,780),endDate = c(150,510,870)),
         data.frame(startDate = c(30,390,750),endDate = c(120,480,840))
         
         
  )
  
  
}


helpTxt <-
  'Centile estimation includes methods for estimating the age-related distribution of
human growth. The standard estimation of centile curves usually involves two continuous variables:
1. the response variable, that is, the variable we are interested in and for which we
are trying to find the centile curves, e.g. weight, BMI, head circumference.
2. the explanatory variable, e.g. age.
The 100p centile of a continuous random variable Y is the value yp such that
Prob(Y ≤ yp) = p. Then yp = FY−1(p), and yp is the inverse cdf of Y applied
to p. In this chapter we consider the conditional centile of Y given explanatory
variable X = x, i.e. yp(x) = FY−j1x(p). By varying x, a 100p centile curve of yp(x)
against x is obtained. Centile curves can be obtained for different values of p. The
World Health Organization uses the values 100p=(3,15,50,85,97) in its charts and
100p=(1,3,5,15,25,50,75,85,95,97,99) in its tables; see WHO [2006, 2007, 2009]. Centile estimation can be extended to more than one explanatory continuous variable,
e.g. age and height; see for example Cole et al. [2009] and Quanjer et al. [2012a] and
Exercise 4 of this chapter. For explanatory categorical variables such as gender, the
usual practice is to produce separate charts for each level of the categorical variable.'






#regr.task

#regr.lrn = makeLearner("regr.gbm", par.vals = list(n.trees = 500, interaction.depth = 3))
#regr.lrn = makeLearner("regr.glmnet")
#regr.lrn = makeLearner("regr.svm")


#regr.lrn = makeLearner("regr.nnet",par.vals = list(size=5))


#mod




#n = getTaskSize(bh.task)
#train.set = seq(1, n, by = 2)
#test.set = seq(2, n, by = 2)
#lrn = makeLearner("regr.gbm", n.trees = 100)
#mod = train(lrn, bh.task, subset = train.set)


#task.pred




# ggplot(testOutput,aes(AvgWeight,response)) + geom_point() +
#   geom_abline(slope = 1)
# 
# ggplot(testOutput,aes(Age,AvgWeight)) + geom_point() +
# geom_point(aes(Age,response), color = "red" )
#   geom_abline(slope = 1)
#   
  


# plotLearnerPrediction(regr.lrn,features = "Age",  task = regr.task)
# 
# performance(task.pred)
# 
# rdesc = makeResampleDesc("Subsample", iters = 5)
# 
# r = resample(regr.lrn, regr.task, rdesc, measures = list(mse, medse,mae, timetrain), extract = getFeatureImportance)
# 
# View(r$pred$data)
# 
# 
# 
# r = resample("regr.rpart", regr.task, rdesc, show.info = FALSE, extract = getFeatureImportance)
# #> Resampling: cross-validation
# #> Measures:             mse
# r$extract














