#jscode <- "Shiny.addCustomMessageHandler('closeWindow', function(m) {window.close();});"
sidebar <- dashboardSidebar(
  tags$head(tags$script(HTML(jscode))),
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Plot", tabName="plot", icon=icon("line-chart"), selected=TRUE),
              # menuItem("Table", tabName = "table", icon=icon("table")),
              # menuItem("Codes",  icon = icon("file-text-o"),
              #          menuSubItem("Mlxtran", tabName = "pkmodel", icon = icon("angle-right")),
              #          menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
              #          menuSubItem("server.R", tabName = "server", icon = icon("angle-right"))
              # ),
              # menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board")),
              menuItem("Close App", tabName = "Stop",  icon = icon("power-off")),
              menuItem("About", tabName = "about", icon = icon("question"))
              
  ),
  hr(),
  conditionalPanel("input.tabs=='plot'",
                   fluidRow(
                     column(1),
                     column(11,
                            selectInput("Template", label = "FCR Table:",
                                        choices = FCR$TemplateName)
                            ,sliderInput("Temp", label = "Temperature:",
                                        min = min(FCR$Temp), max = max(FCR$Temp), value = c(10,28), step = 1)
                            ,selectInput("TypeOfChart", label = "Type of Chart:",
                                        choices = c("line","scatter"))
                            ,checkboxInput("ToolTip", "ToolTip", TRUE)
                            ,selectInput("DataFormat", label = "Format of Data:",
                                        choices = c("Pivoted","Raw Data"))
                     )
                   )
  )
  ,conditionalPanel("input.tabs == 'table'",
                   fluidRow(
                     column(1),
                     column(11,
                            
                            selectInput("DataFormat", label = "Format of Data:",
                                         choices = c("Raw Data","Pivoted"))
                            
                     )
                   )
  )
)








############################################





# dashboardPage(
#   skin = "black",
#   dashboardHeader(title = "FCRTable", disable = FALSE),
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Plot of FCR Table", tabName = "plot", icon = icon("bar-chart")),
#       selectInput("Template", label = "FCR Table:",
#                   choices = FCR$TemplateName),
#       
#       
#       sliderInput("Temp", label = "Temperature:",
#                   min = min(FCR$Temp), max = max(FCR$Temp), value = c(10,28), step = 1)
#     )
#     #,
#     #div(includeMarkdown("hcterinfo.md"), style = "padding:10px")
#   ),
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = "plot",
              fluidRow(
                box(  width = NULL, highchartOutput("highchart",height = "550px"), collapsible = TRUE,
                      title = " FCR Plot", status = "primary", solidHeader = TRUE),
              br()
              ,box(  width = NULL, dataTableOutput("table",height = "550px"), collapsible = FALSE,
                    title = " FCR Table", status = "primary", solidHeader = TRUE),
              br(),br()
                #box(width = 6, highchartOutput("highchart")),
                #box(width = 6, dataTableOutput("table"))
                
              )
      )
      ,tabItem(tabName = "table",
              fluidRow(
                
                
            
                
              )
      )
    )
  )
  
#  )



dashboardPage(
  dashboardHeader(title = "FCR Table Smoother"),
  sidebar,
  body

  )
