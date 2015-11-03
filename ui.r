library(shiny)
shinyUI(fluidPage(
  titlePanel("Cheetah Orion Performance Report"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file"),
      selectInput(
        "y_input", 
        label = h5("Select Adset"),
        ""
      ),
      dateRangeInput('date_range',
                     'Date range input:'),
      selectInput("kpi", label = h5("Select Goal KPI"), 
                         choices = list("CPI" = 1, "CPC" = 2), 
                  selected = 1)
    ),
    mainPanel(
      uiOutput("tb")
      # use below code if you want the tabset programming in the main panel. If so, then tabset will appear when the app loads for the first time.
      #       tabsetPanel(tabPanel("Summary", verbatimTextOutput("sum")),
      #                   tabPanel("Data", tableOutput("table")))
    )
    
  )
))