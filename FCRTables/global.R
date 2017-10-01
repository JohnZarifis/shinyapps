library("shiny")
library("shinydashboard")
library("highcharter")
library("dplyr")
library("markdown")

library("tidyr")
library("ggplot2")
library("DT")
library("RODBC")

# remove saved data
rm(list=ls())

jscode <- "Shiny.addCustomMessageHandler('closeWindow', function(m) {window.close();});"


#dbhandle <- odbcDriverConnect("driver={SQL Server Native Client 11.0};server= 10.1.1.120;Database=Aquagrowth3;Uid=sa;Pwd=1q2w3e!Q@W#E")
dbhandle <- odbcDriverConnect('driver=FreeTDS;TDS_Version=7.3;Server=10.1.1.120;Port=1433;Database=AquaGrowth3;Uid=sa;Pwd=1q2w3e!Q@W#E')




FCR <- sqlQuery(dbhandle, "WITH FCRTable ( FromWeight, ToWeight, Temp, Factor , TemplateName)
                as
                (
                SELECT
                FromWeight
                ,isNull((SELECT TOP 1 FromWeight FROM [FCRValue] V1
                WHERE V1.FromWeight > V.FromWeight and V1.FCRTemplateID = V.FCRTemplateID
                ORDER BY FromWeight),10000) AS ToWeight
                ,[Temp]
                ,[Factor]
                ,T.TemplateName
                FROM [FCRValue] V
                inner join [FCRTemplate] T
                on V.FCRTemplateID = T.FCRTemplateID
                where
                Factor > 0

                )
                Select  cast(FromWeight as varchar(10)) + '-'+ cast(ToWeight as varchar(10)) AS WeightCategory, FromWeight, ToWeight, Temp, Factor , TemplateName
                FROM FCRTable order by TemplateName, FromWeight, Temp")
odbcClose(dbhandle)

#saveRDS(FCR, "./DemoData/FCR.rds")


#FCR <- readRDS("./DemoData/FCR.rds")

