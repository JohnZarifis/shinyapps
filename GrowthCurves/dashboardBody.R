body <- dashboardBody(
        tabItems(
  tabItem(tabName = "data",
          box(width = 2,title = 'Sampling Data',solidHeader = T,status = 'primary',
              
              selectInput('Specie', label='Choose Specie', 
                          choices= c( unique(as.character(Sampling$SPECIE ))), selected="ΤΣΙΠΟΥΡΑ", multiple=FALSE),
              selectizeInput(
                'Grading', label = 'Choose Grading', choices = c( unique(as.character(Sampling$GRADING ))),
                 selected = NULL, multiple = TRUE
                
              ),
              selectizeInput(
                'StockingMonth', label = 'Choose Stocking Month', choices = c(sort(unique(Sampling$ORIGINMONTH ))),
                selected = 5, multiple = TRUE
                
              ),
              sliderInput('AvgWeight',label = label.help('Average Weight','lbl_AvgWeight'),min = 0 ,max = max(Sampling$AvgWeight),step = 10,value = 420),
              bsTooltip(id = "lbl_AvgWeight", title = "Average Weight Range to model", 
                        placement = "right", trigger = "hover"),
              selectizeInput(
                'Region', label = 'Choose Region', choices = c(unique(as.character(Sampling$REGION ))),
                selected = c(unique(as.character(Sampling$REGION ))), multiple = TRUE
                
              )
             
              
              ,selectInput(
                'GraphColor', label = label.help('Color By','lbl_GraphColor'), choices = c('Nothing'='Nothing','StockingYear'='ORIGINYEAR'
                                                                                           ,'Hatchery' = 'HATCHERY','Prefatening'= 'Prefatening'
                                                                                           ,'Grading' = 'GRADING','Region'='REGION','StockingMonth'='ORIGINMONTH'
                                                                                           ,'LOT' = 'LOT','Type of Measure' = 'TypeOfMeasure'
                                                                                           ),
                selected = 'Nothing', multiple = FALSE
                
              )
              ,bsTooltip(id = "lbl_GraphColor", title = "Chooses a categorial variable to Disect Dataset", 
                         placement = "right", trigger = "hover")
              
              ,hr()
              ,checkboxInput('Smoother', label = 'Show Smoothing', value = FALSE)
              ,uiOutput('lots')
              
              
          ),
          box(width=10,
              
              #withSpinner(
                plotOutput('Yplot',height=460,click = "Yplot_click")
                #)
              ,DT::dataTableOutput('Outliers')
              
              #,verbatimTextOutput("info")
          )
  ),
  tabItem(tabName = "model",
          box(width = 2,title = 'Model Options',solidHeader = T,status = 'primary',
              actionButton("modelButton", "Create Growth Curves")
               
              ,uiOutput('lotName')
              ,checkboxInput('ShowSmooth', label = 'Show Smoothing', value = FALSE)
              ,checkboxInput('ShowPoints', label = 'Show All Data', value = FALSE)
              ,selectInput(
                'ShowInteractive', label = 'Choose Graph Type', choices = c('Interactive','Static'),
                selected = 'Static', multiple = FALSE
                
              )
              #,checkboxInput('ShowInteractive', label = 'Interactive Plot', value = FALSE)
              )
          ,box(width=10,
               conditionalPanel( condition = "input.ShowInteractive == 'Interactive'",
                                 withSpinner(
                                   
                                   plotlyOutput('CurvesInt',height = "600px")
                                   )               
               )
             
               , conditionalPanel( condition = "input.ShowInteractive == 'Static'",
                                 withSpinner(

                                   plotOutput('Curves',height = "600px")
                                 ))

               
               
               #,verbatimTextOutput("info")
               
               
               #verbatimTextOutput("info")
          )
  ),tabItem(tabName = "about",
            h4('Centile estimation includes methods for estimating the age-related distribution of
               human growth. The standard estimation of centile curves usually involves two continuous variables:')
                 
                 
            ,h5('1. the response variable, that is, the variable we are interested in and for which we
               are trying to find the centile curves, e.g. weight, BMI, head circumference.')
               
               
             ,h5('2. the explanatory variable, e.g. age.')
               
               
             ,h5(
               'We consider the conditional centile of Y given explanatory
               variable X = x.
               
               Centile curves can be obtained for different values of p. The
               World Health Organization uses the values 100p=(3,15,50,85,97) in its charts and
               100p=(1,3,5,15,25,50,75,85,95,97,99) in its tables. 
               
               Centile estimation can be extended to more than one explanatory continuous variable,
               e.g. age and height; see for example Cole et al. [2009] and Quanjer et al. [2012a] 
               For explanatory categorical variables such as gender, the
               usual practice is to produce separate charts for each level of the categorical variable.')
            )
  
  
  
  )
  
  
)
