sidebar <- dashboardSidebar(
   img(src='logo.jpg',class ='img-responsive')
  ,sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem(" Step 1: Data",  tabName = "data",   icon = icon("dashboard")),
      menuItem(" Step 2: Model", tabName = "model", icon = icon("bar-chart-o")),
      menuItem(" About", tabName = "about", icon = icon("book"))
  )
  ,hr()
  
  
  
)
