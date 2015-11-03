library(shiny)
# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)
library(rCharts)
library(dplyr)
library(stringr)

# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input, output, session){
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.csv(file1$datapath, header = T)
  })
  data_Date<-reactive({
    data_Date<-data()
    data_Date$Date<-as.Date(as.character(data_Date$Date), "%Y%m%d")
    data_Date <- transform(data_Date, Date = as.character(Date))
    data_Date
  })
  #to update the panel according to the file's adset
  observe({
    updateSelectInput(
      session,
      "y_input",
      choices=as.character(unique(data()$Ad.Set)))
  })
  
  observe({
    updateDateRangeInput(session, "date_range",
                         start = min(data_Date()$Date),
                         end = max(data_Date()$Date),
                         min = min(data_Date()$Date),
                         max = max(data_Date()$Date))
  })
  
  yVarName<-reactive({
    input$y_input
  })
  
  #####General
  ##To plot out the table summary of this campaign
  # create a summary dataframe for the table to use
  data_summary_table<-reactive({
    data3 <- data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data3$Date<-as.Date(as.character(data3$Date), "%Y%m%d")
    data3$Spent<-as.numeric(substring(as.character(data3$Spent),2))
    data3$Impressions<- as.numeric(data3$Impressions)
    data3$Campaign<- as.character(data3$Campaign)
    data3 <- data3 %>% 
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Campaign) %>% 
      summarize(Total_Impressions=sum(Impressions),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_CPI=Total_Spent/Total_Conversions, 
             Total_CPC=Total_Spent/Total_Clicks, 
             Total_CTR=Total_Clicks/Total_Impressions,
             Total_CVR=Total_Conversions/Total_Clicks)
    data3
  })
  
  #output the table in the ui
  output$summary_table <- renderDataTable({
    data_summary_table()
    
  })
  
  ## To create basic summary plots
  #create a data frame for the plots to use
  data_adset<-reactive({
    data2<-data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data2$Date<-as.Date(as.character(data2$Date), "%Y%m%d")
    data2$Spent<-as.numeric(substring(as.character(data2$Spent),2))
    data2$Impressions<- as.numeric(data2$Impressions)
    data2<-data2 %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Date) %>% 
      summarize(Total_Impressions=sum(Impressions),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_CPI=Total_Spent/Total_Conversions,
             Total_CPC=Total_Spent/Total_Clicks)
    data2<-transform(data2, Date = as.character(Date))
    data2
  })
  
  #use the dataframe to plot out the basic plots
  output$h1 <- renderChart2({
    if(is.null(data())){return ()}
    
    h1 <- Highcharts$new()
    h1$xAxis(categories = data_adset()$Date)
    h1$yAxis(list(list(title = list(text = 'Conversions')), 
             list(title = list(text = 'CPI'), opposite = TRUE)))
    h1$series(name = 'Conversions', type = 'column', color = '#4572A7',
             data = data_adset()$Total_Conversions)
    h1$series(name = 'CPI', type = 'spline', color = '#89A54E',
             data = data_adset()$Total_CPI,
             yAxis = 1)
    h1$title(text =("Conversion/CPI vs Date"))
    #h1$subtitle(text =("可以看出Conversion/CPI隨時間的變化"))
    return(h1)
  })
  
  output$h2 <- renderChart2({
    if(is.null(data())){return ()}
    
    h2 <- Highcharts$new()
    h2$xAxis(categories = data_adset()$Date)
    h2$yAxis(list(list(title = list(text = 'Clicks')), 
                  list(title = list(text = 'CPC'), opposite = TRUE)))
    h2$series(name = 'Clicks', type = 'column', color = '#4572A7',
              data = data_adset()$Total_Clicks)
    h2$series(name = 'CPC', type = 'spline', color = '#AA4643',
              data = data_adset()$Total_CPC,
              yAxis = 1)
    h2$title(text =("Clicks/CPC vs Date"))
    #h2$subtitle(text =("可以看出Clicks/CPC隨時間的變化"))
    return(h2)
  })
  
  output$h3 <- renderChart2({
    if(is.null(data())){return ()}
    
    h3 <- Highcharts$new()
    h3$xAxis(categories = data_adset()$Date)
    h3$yAxis(list(list(title = list(text = 'CPI')), 
                  list(title = list(text = 'CPC'), opposite = TRUE)))
    h3$series(name = 'CPI', type = 'spline', color = '#89A54E',
              data = data_adset()$Total_CPI)
    h3$series(name = 'CPC', type = 'spline', color = '#AA4643',
              data = data_adset()$Total_CPC,
              yAxis = 1)
    h3$title(text =("CPI/CPC vs Date"))
    return(h3)
  })
  
  ##to plot the age install/clicks bar line chart
  data_age_kpi<- reactive({
    data18<-data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data18$Date<-as.Date(as.character(data18$Date), "%Y%m%d")
    data18<- data18 %>%
              filter(Date <= max_date) %>% 
              filter(Date >= min_date) %>%
              group_by(Age) %>% 
              summarize(Total_Impressions=sum(Impressions),
                        Total_Clicks=sum(Clicks),
                        Total_Conversions=sum(Conversions),
                        Total_Spent=sum(Spent)) %>%
              mutate(Total_Cpi=Total_Spent/Total_Conversions, 
                     Total_Cvr=Total_Conversions/Total_Clicks,
                     Total_Ctr=Total_Clicks/Total_Impressions)
    data18
  })
  output$h8 <- renderChart2({
    h8 <- Highcharts$new()
    h8$xAxis(categories = data_age_kpi()$Age)
    h8$yAxis(list(list(title = list(text = 'Conversions')), 
                  list(title = list(text = 'CVR'), opposite = TRUE)))
    h8$series(name = 'Conversions', type = 'column', color = '#4572A7',
              data = data_age_kpi()$Total_Conversions)
    h8$series(name = 'CVR', type = 'spline', color = '#89A54E',
              data = data_age_kpi()$Total_Cvr,
              yAxis = 1)
    h8$title(text =("Conversions/CVR vs Age"))
    return(h8)
  })
  output$h9 <- renderChart2({
    h9 <- Highcharts$new()
    h9$xAxis(categories = data_age_kpi()$Age)
    h9$yAxis(list(list(title = list(text = 'Clicks')), 
                  list(title = list(text = 'CTR'), opposite = TRUE)))
    h9$series(name = 'Clicks', type = 'column', color = '#4572A7',
              data = data_age_kpi()$Total_Conversions)
    h9$series(name = 'CTR', type = 'spline', color = '#AA4643',
              data = data_age_kpi()$Total_Cvr,
              yAxis = 1)
    h9$title(text =("Clicks/CTR vs Age"))
    return(h9)
  })
  
  ##to plot the creative install/clicks bar line chart
  data_creative_kpi<- reactive({
    data19<-data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data19$Date<-as.Date(as.character(data19$Date), "%Y%m%d")
    data19<-cbind(data19,str_split_fixed(data19$Ad.Set, "-", 5))
    data19<-cbind(data19,str_split_fixed(data19$Ad, "_", 2)[,1])
    colnames(data19)[16:21]<-c("ad_type","category","initial_bid","potential_reach","app_name","creative")
    data19<- data19 %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(creative) %>% 
      summarize(Total_Impressions=sum(Impressions),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions, 
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Impressions)
    data19
  })
  output$h10 <- renderChart2({
    h10 <- Highcharts$new()
    h10$xAxis(categories = data_creative_kpi()$creative)
    h10$yAxis(list(list(title = list(text = 'Conversions')), 
                  list(title = list(text = 'CVR'), opposite = TRUE)))
    h10$series(name = 'Conversions', type = 'column', color = '#4572A7',
              data = data_creative_kpi()$Total_Conversions)
    h10$series(name = 'CVR', type = 'spline', color = '#89A54E',
              data = data_creative_kpi()$Total_Cvr,
              yAxis = 1)
    h10$title(text =("Conversions/CVR vs Creative"))
    return(h10)
  })
  output$h11 <- renderChart2({
    h11 <- Highcharts$new()
    h11$xAxis(categories = data_creative_kpi()$creative)
    h11$yAxis(list(list(title = list(text = 'Clicks')), 
                  list(title = list(text = 'CTR'), opposite = TRUE)))
    h11$series(name = 'Clicks', type = 'column', color = '#4572A7',
              data = data_creative_kpi()$Total_Clicks)
    h11$series(name = 'CTR', type = 'spline', color = '#AA4643',
              data = data_creative_kpi()$Total_Ctr,
              yAxis = 1)
    h11$title(text =("Clicks/CTR vs Creative"))
    return(h11)
  })
  
  ##to plot the adtype install/clicks bar line chart
  data_adtype_kpi<- reactive({
    data20<-data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data20$Date<-as.Date(as.character(data20$Date), "%Y%m%d")
    data20<-cbind(data20,str_split_fixed(data20$Ad.Set, "-", 5))
    data20<-cbind(data20,str_split_fixed(data20$Ad, "_", 2)[,1])
    colnames(data20)[16:21]<-c("ad_type","category","initial_bid","potential_reach","app_name","creative")
    data20<- data20 %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(ad_type) %>% 
      summarize(Total_Impressions=sum(Impressions),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions, 
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Impressions)
    data20
  })
  output$h12 <- renderChart2({
    h12 <- Highcharts$new()
    h12$xAxis(categories = data_adtype_kpi()$ad_type)
    h12$yAxis(list(list(title = list(text = 'Conversions')), 
                   list(title = list(text = 'CVR'), opposite = TRUE)))
    h12$series(name = 'Conversions', type = 'column', color = '#4572A7',
               data = data_adtype_kpi()$Total_Conversions)
    h12$series(name = 'CVR', type = 'spline', color = '#89A54E',
               data = data_adtype_kpi()$Total_Cvr,
               yAxis = 1)
    h12$title(text =("Conversions/CVR vs Adtype"))
    return(h12)
  })
  output$h13 <- renderChart2({
    h13 <- Highcharts$new()
    h13$xAxis(categories = data_adtype_kpi()$ad_type)
    h13$yAxis(list(list(title = list(text = 'Clicks')), 
                   list(title = list(text = 'CTR'), opposite = TRUE)))
    h13$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_adtype_kpi()$Total_Clicks)
    h13$series(name = 'CTR', type = 'spline', color = '#AA4643',
               data = data_adtype_kpi()$Total_Ctr,
               yAxis = 1)
    h13$title(text =("Clicks/CTR vs Adtype"))
    return(h13)
  })
  
  ##to plot the gender install/clicks bar line chart
  data_gender_kpi<- reactive({
    data21<-data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data21$Date<-as.Date(as.character(data21$Date), "%Y%m%d")
    data21<- data21 %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Gender) %>% 
      summarize(Total_Impressions=sum(Impressions),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions, 
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Impressions)
    data21
  })
  output$h14 <- renderChart2({
    h14 <- Highcharts$new()
    h14$xAxis(categories = data_gender_kpi()$Gender)
    h14$yAxis(list(list(title = list(text = 'Conversions')), 
                   list(title = list(text = 'CVR'), opposite = TRUE)))
    h14$series(name = 'Conversions', type = 'column', color = '#4572A7',
               data = data_gender_kpi()$Total_Conversions)
    h14$series(name = 'CVR', type = 'spline', color = '#89A54E',
               data = data_gender_kpi()$Total_Cvr,
               yAxis = 1)
    h14$title(text =("Conversions/CVR vs Gender"))
    return(h14)
  })
  output$h15 <- renderChart2({
    h15 <- Highcharts$new()
    h15$xAxis(categories = data_gender_kpi()$Gender)
    h15$yAxis(list(list(title = list(text = 'Clicks')), 
                   list(title = list(text = 'CTR'), opposite = TRUE)))
    h15$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_gender_kpi()$Total_Clicks)
    h15$series(name = 'CTR', type = 'spline', color = '#AA4643',
               data = data_gender_kpi()$Total_Ctr,
               yAxis = 1)
    h15$title(text =("Clicks/CTR vs Gender"))
    return(h15)
  })
  
  
  #####Category
  ##To plot the category basic table
  # create a summary dataframe for the table to use
  data_category_table<-reactive({
    data9 <- data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data9$Date<-as.Date(as.character(data9$Date), "%Y%m%d")
    data9$Spent<-as.numeric(substring(as.character(data9$Spent),2))
    data9$Impressions<- as.numeric(data9$Impressions)
    data9$Campaign<- as.character(data9$Campaign)
    data9<-cbind(data9,str_split_fixed(data9$Ad.Set, "-", 5))
    data9<-cbind(data9,str_split_fixed(data9$Ad, "_", 2)[,1])
    colnames(data9)[16:21]<-c("ad_type","category","initial_bid","potential_reach","app_name","creative")
    data9 <- data9 %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(category) %>% 
      summarize(Total_Impressions=sum(Impressions),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_CPI=Total_Spent/Total_Conversions, 
             Total_CPC=Total_Spent/Total_Clicks, 
             Total_CTR=Total_Clicks/Total_Impressions,
             Total_CVR=Total_Conversions/Total_Clicks)
    data9
  })
  
  #output the table in the ui
  output$category_table <- renderDataTable({
    data_category_table()
    
  })
  
  
  ##To plot the category vs date
  data_category_date<-reactive({
    data8<- data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data8$Date<-as.Date(as.character(data8$Date), "%Y%m%d")
    data8<-cbind(data8,str_split_fixed(data8$Ad.Set, "-", 5))
    data8<-cbind(data8,str_split_fixed(data8$Ad, "_", 2)[,1])
    colnames(data8)[16:21]<-c("ad_type","category","initial_bid","potential_reach","app_name","creative")
    data8<-data8 %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(category, Date) %>% 
      summarize(Total_Conversions=sum(Conversions),
                Total_Clicks=sum(Clicks))
    data8 <- transform(data8, Date = as.character(Date))
    data8
  })
  
  #use the dataframe to plot out the basic plots
  output$h4 <- renderChart2({
    if(is.null(data())){return ()}
    
    h4 <- Highcharts$new()
    h4 <- hPlot(x = "Date", y = "Total_Conversions",
                data = data_category_date(), type = "line", group = "category")
    h4$title(text =("Conversions vs Date (by Category)"))
    return(h4)
  })
  
  output$h22 <- renderChart2({
    if(is.null(data())){return ()}
    
    h22 <- Highcharts$new()
    h22 <- hPlot(x = "Date", y = "Total_Clicks",
                data = data_category_date(), type = "line", group = "category")
    h22$title(text =("Clicks vs Date (by Category)"))
    return(h22)
  })
  
  ##to plot the category pie chart
  data_category <- reactive({
    data10<- data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data10$Date<-as.Date(as.character(data10$Date), "%Y%m%d")
    data10<-cbind(data10,str_split_fixed(data10$Ad.Set, "-", 5))
    data10<-cbind(data10,str_split_fixed(data10$Ad, "_", 2)[,1])
    colnames(data10)[16:21]<-c("ad_type","category","initial_bid","potential_reach","app_name","creative")
    data10<- data10 %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(category) %>% 
      summarize(Total_Conversions=sum(Conversions),
                Total_Clicks=sum(Clicks))
    data10
  })
  
  output$n5 <- renderChart2({
    n5 = nPlot(x = "category", y = "Total_Conversions", data = data_category(), type = "pieChart")
    n5$chart(tooltipContent = "#! function(key, y, e, graph){return '<h3>' + 'Category: ' + key + '</h3>' + '<p>'+ 'Value ' + y + '<br>' + ' % of value: ' + e.point.PERCENT} !#")
    n5$set(width = 800, height = 400) # mk changed width to 800 and height to 500
    n5$addParams(dom="n5")
    n5$set(title = "Category Conversion pie chart")
    return(n5)
  })
  output$n11 <- renderChart2({
    n11 = nPlot(x = "category", y = "Total_Clicks", data = data_category(), type = "pieChart")
    n11$chart(tooltipContent = "#! function(key, y, e, graph){return '<h3>' + 'Category: ' + key + '</h3>' + '<p>'+ 'Value ' + y + '<br>' + ' % of value: ' + e.point.PERCENT} !#")
    n11$set(width = 800, height = 400) # mk changed width to 800 and height to 500
    n11$addParams(dom="n11")
    n11$set(title = "Category Clicks pie chart")
    return(n11)
  })
  
  
  
  #####Adset
  ##To group the data by the selected adset
  #to generate the data frame
  data_selected_adset_table <-reactive({
    data13 <- data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data13$Date<-as.Date(as.character(data13$Date), "%Y%m%d")
    selected_adset<- input$y_input
    data13$Spent<-as.numeric(substring(as.character(data13$Spent),2))
    data13$Impressions<- as.numeric(data13$Impressions)
    data13$Campaign<- as.character(data13$Campaign)
    data13 <- data13 %>% 
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Ad.Set) %>% 
      summarize(Total_Impressions=sum(Impressions),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_CPI=Total_Spent/Total_Conversions, 
             Total_CPC=Total_Spent/Total_Clicks, 
             Total_CTR=Total_Clicks/Total_Impressions,
             Total_CVR=Total_Conversions/Total_Clicks)
    data13
    })
  
  #output the table in the ui
  output$adset_table <- renderDataTable({
    data_selected_adset_table()
  })
  
  ## To create basic summary plots
  #create a data frame for the plots to use
  data_selected_adset<-reactive({
    data14<-data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    selected_adset<- input$y_input
    data14$Date<-as.Date(as.character(data14$Date), "%Y%m%d")
    data14$Spent<-as.numeric(substring(as.character(data14$Spent),2))
    data14$Impressions<- as.numeric(data14$Impressions)
    data14<-data14 %>% 
            filter(Ad.Set==selected_adset) %>%
            filter(Date <= max_date) %>% 
            filter(Date >= min_date) %>%
            group_by(Date) %>% 
            summarize(Total_Impressions=sum(Impressions),
                      Total_Clicks=sum(Clicks),
                      Total_Conversions=sum(Conversions),
                      Total_Spent=sum(Spent)) %>%
            mutate(Total_CPI=Total_Spent/Total_Conversions,
                   Total_CPC=Total_Spent/Total_Clicks)
    data14$Total_CPI <- ifelse(data14$Total_CPI == "Inf", 
                               data14$Total_Spent, data14$Total_CPI)
    data14<-transform(data14, Date = as.character(Date))
    data14
  })
  
  #use the dataframe to plot out the basic plots
  output$h5 <- renderChart2({
    if(is.null(data())){return ()}
    h5 <- Highcharts$new()
    h5$xAxis(categories = data_selected_adset()$Date)
    h5$yAxis(list(list(title = list(text = 'Conversions')), 
                  list(title = list(text = 'CPI'), opposite = TRUE)))
    h5$series(name = 'Conversions', type = 'column', color = '#4572A7',
              data = data_selected_adset()$Total_Conversions)
    h5$series(name = 'CPI', type = 'spline', color = '#89A54E',
              data = data_selected_adset()$Total_CPI,
              yAxis = 1)
    h5$title(text =("Conversion/CPI vs Date(Selected Adset)"))
    return(h5)
  })
  
  output$h6 <- renderChart2({
    if(is.null(data())){return ()}
    h6 <- Highcharts$new()
    h6$xAxis(categories = data_selected_adset()$Date)
    h6$yAxis(list(list(title = list(text = 'Clicks')), 
                  list(title = list(text = 'CPC'), opposite = TRUE)))
    h6$series(name = 'Clicks', type = 'column', color = '#4572A7',
              data = data_selected_adset()$Total_Clicks)
    h6$series(name = 'CPC', type = 'spline', color = '#AA4643',
              data = data_selected_adset()$Total_CPC,
              yAxis = 1)
    h6$title(text =("Clicks/CPC vs Date(Selected Adset)"))
    return(h6)
  })
  
  output$h7 <- renderChart2({
    if(is.null(data())){return ()}
    h7 <- Highcharts$new()
    h7$xAxis(categories = data_selected_adset()$Date)
    h7$yAxis(list(list(title = list(text = 'CPI')), 
                  list(title = list(text = 'CPC'), opposite = TRUE)))
    h7$series(name = 'CPI', type = 'spline', color = '#89A54E',
              data = data_selected_adset()$Total_CPI)
    h7$series(name = 'CPC', type = 'spline', color = '#AA4643',
              data = data_selected_adset()$Total_CPC,
              yAxis = 1)
    h7$title(text =("CPI/CPC vs Date(Selected Adset)"))
    return(h7)
  })
  ##to plot the selected adset age install/clicks bar line chart
  data_selected_adset_age_kpi<- reactive({
    data22<-data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data22$Date<-as.Date(as.character(data22$Date), "%Y%m%d")
    selected_adset<- input$y_input
    data22<- data22 %>%
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Age) %>% 
      summarize(Total_Impressions=sum(Impressions),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions, 
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Impressions)
    data22
  })
  output$h16 <- renderChart2({
    h16 <- Highcharts$new()
    h16$xAxis(categories = data_selected_adset_age_kpi()$Age)
    h16$yAxis(list(list(title = list(text = 'Conversions')), 
                  list(title = list(text = 'CVR'), opposite = TRUE)))
    h16$series(name = 'Conversions', type = 'column', color = '#4572A7',
              data = data_selected_adset_age_kpi()$Total_Conversions)
    h16$series(name = 'CVR', type = 'spline', color = '#89A54E',
              data = data_selected_adset_age_kpi()$Total_Cvr,
              yAxis = 1)
    h16$title(text =("Conversions/CVR vs Age (Selected Adset)"))
    return(h16)
  })
  output$h17 <- renderChart2({
    h17 <- Highcharts$new()
    h17$xAxis(categories = data_selected_adset_age_kpi()$Age)
    h17$yAxis(list(list(title = list(text = 'Clicks')), 
                  list(title = list(text = 'CTR'), opposite = TRUE)))
    h17$series(name = 'Clicks', type = 'column', color = '#4572A7',
              data = data_selected_adset_age_kpi()$Total_Conversions)
    h17$series(name = 'CTR', type = 'spline', color = '#AA4643',
              data = data_selected_adset_age_kpi()$Total_Cvr,
              yAxis = 1)
    h17$title(text =("Clicks/CTR vs Age (Selected Adset)"))
    return(h17)
  })
  
  ##to plot the selected adset creative install/clicks bar line chart
  data_selected_adset_creative_kpi<- reactive({
    data23<-data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    selected_adset<- input$y_input
    data23$Date<-as.Date(as.character(data23$Date), "%Y%m%d")
    data23<-cbind(data23,str_split_fixed(data23$Ad.Set, "-", 5))
    data23<-cbind(data23,str_split_fixed(data23$Ad, "_", 2)[,1])
    colnames(data23)[16:21]<-c("ad_type","category","initial_bid","potential_reach","app_name","creative")
    data23<- data23 %>%
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(creative) %>% 
      summarize(Total_Impressions=sum(Impressions),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions, 
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Impressions)
    data23
  })
  output$h18 <- renderChart2({
    h18 <- Highcharts$new()
    h18$xAxis(categories = data_selected_adset_creative_kpi()$creative)
    h18$yAxis(list(list(title = list(text = 'Conversions')), 
                   list(title = list(text = 'CVR'), opposite = TRUE)))
    h18$series(name = 'Conversions', type = 'column', color = '#4572A7',
               data = data_selected_adset_creative_kpi()$Total_Conversions)
    h18$series(name = 'CVR', type = 'spline', color = '#89A54E',
               data = data_selected_adset_creative_kpi()$Total_Cvr,
               yAxis = 1)
    h18$title(text =("Conversions/CVR vs Creative (Selected Adset)"))
    return(h18)
  })
  output$h19 <- renderChart2({
    h19 <- Highcharts$new()
    h19$xAxis(categories = data_selected_adset_creative_kpi()$creative)
    h19$yAxis(list(list(title = list(text = 'Clicks')), 
                   list(title = list(text = 'CTR'), opposite = TRUE)))
    h19$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_selected_adset_creative_kpi()$Total_Clicks)
    h19$series(name = 'CTR', type = 'spline', color = '#AA4643',
               data = data_selected_adset_creative_kpi()$Total_Ctr,
               yAxis = 1)
    h19$title(text =("Clicks/CTR vs Creative (Selected Adset)"))
    return(h19)
  })
  ##to plot the selected adset gender install/clicks bar line chart
  data_selected_adset_gender_kpi<- reactive({
    data24<-data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    selected_adset<- input$y_input
    data24$Date<-as.Date(as.character(data24$Date), "%Y%m%d")
    data24<- data24 %>%
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Gender) %>% 
      summarize(Total_Impressions=sum(Impressions),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions, 
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Impressions)
    data24
  })
  output$h20 <- renderChart2({
    h20 <- Highcharts$new()
    h20$xAxis(categories = data_selected_adset_gender_kpi()$Gender)
    h20$yAxis(list(list(title = list(text = 'Conversions')), 
                   list(title = list(text = 'CVR'), opposite = TRUE)))
    h20$series(name = 'Conversions', type = 'column', color = '#4572A7',
               data = data_selected_adset_gender_kpi()$Total_Conversions)
    h20$series(name = 'CVR', type = 'spline', color = '#89A54E',
               data = data_selected_adset_gender_kpi()$Total_Cvr,
               yAxis = 1)
    h20$title(text =("Conversions/CVR vs Gender (Selected Adset)"))
    return(h20)
  })
  output$h21 <- renderChart2({
    h21 <- Highcharts$new()
    h21$xAxis(categories = data_selected_adset_gender_kpi()$Gender)
    h21$yAxis(list(list(title = list(text = 'Clicks')), 
                   list(title = list(text = 'CTR'), opposite = TRUE)))
    h21$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_selected_adset_gender_kpi()$Total_Clicks)
    h21$series(name = 'CTR', type = 'spline', color = '#AA4643',
               data = data_selected_adset_gender_kpi()$Total_Ctr,
               yAxis = 1)
    h21$title(text =("Clicks/CTR vs Gender (Selected Adset)"))
    return(h21)
  })
  
  ##to plot the selected adset age pie chart
  data_selected_adset_age <- reactive({
    data15<- data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data15$Date<-as.Date(as.character(data15$Date), "%Y%m%d")
    selected_adset<- input$y_input
    data15<- data15 %>% 
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Age) %>% 
      summarize(Total_Conversions=sum(Conversions),
                Total_Clicks=sum(Clicks))
    data15
  })
  output$n8 <- renderChart2({
    if(is.null(data())){return ()}
    n8 = nPlot(x = "Age", y = "Total_Conversions", data = data_selected_adset_age(), type = "pieChart")
    n8$chart(tooltipContent = "#! function(key, y, e, graph){return '<h3>' + 'Category: ' + key + '</h3>' + '<p>'+ 'Value ' + y + '<br>' + ' % of value: ' + e.point.PERCENT} !#")
    n8$set(width = 800, height = 400) # mk changed width to 800 and height to 500
    n8$addParams(dom="n8")
    return(n8)
  })
  output$n12 <- renderChart2({
    if(is.null(data())){return ()}
    n12 = nPlot(x = "Age", y = "Total_Clicks", data = data_selected_adset_age(), type = "pieChart")
    n12$chart(tooltipContent = "#! function(key, y, e, graph){return '<h3>' + 'Category: ' + key + '</h3>' + '<p>'+ 'Value ' + y + '<br>' + ' % of value: ' + e.point.PERCENT} !#")
    n12$set(width = 800, height = 400) # mk changed width to 800 and height to 500
    n12$addParams(dom="n12")
    return(n12)
  })
  
  ##to plot the selected adset creative pie chart
  data_selected_adset_creative <- reactive({
    data16<- data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data16$Date<-as.Date(as.character(data16$Date), "%Y%m%d")
    selected_adset<- input$y_input
    data16<-cbind(data16,str_split_fixed(data16$Ad.Set, "-", 5))
    data16<-cbind(data16,str_split_fixed(data16$Ad, "_", 2)[,1])
    colnames(data16)[16:21]<-c("ad_type","category","initial_bid","potential_reach","app_name","creative")
    data16<- data16 %>% 
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(creative) %>% 
      summarize(Total_Conversions=sum(Conversions),
                Total_Clicks=sum(Clicks))
    data16
  })
  
  output$n9 <- renderChart2({
    n9 = nPlot(x = "creative", y = "Total_Conversions", data = data_selected_adset_creative(), type = "pieChart")
    n9$chart(tooltipContent = "#! function(key, y, e, graph){return '<h3>' + 'Category: ' + key + '</h3>' + '<p>'+ 'Value ' + y + '<br>' + ' % of value: ' + e.point.PERCENT} !#")
    n9$set(width = 800, height = 400) # mk changed width to 800 and height to 500
    n9$addParams(dom="n9")
    return(n9)
  })
  output$n13 <- renderChart2({
    n13 = nPlot(x = "creative", y = "Total_Clicks", data = data_selected_adset_creative(), type = "pieChart")
    n13$chart(tooltipContent = "#! function(key, y, e, graph){return '<h3>' + 'Category: ' + key + '</h3>' + '<p>'+ 'Value ' + y + '<br>' + ' % of value: ' + e.point.PERCENT} !#")
    n13$set(width = 800, height = 400) # mk changed width to 800 and height to 500
    n13$addParams(dom="n13")
    return(n13)
  })
  
  ##to plot the selected adset gender pie chart
  data_selected_adset_gender <- reactive({
    data17<- data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data17$Date<-as.Date(as.character(data17$Date), "%Y%m%d")
    selected_adset<- input$y_input
    data17<- data17 %>% 
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Gender) %>% 
      summarize(Total_Conversions=sum(Conversions),
                Total_Clicks=sum(Clicks))
    data17
  })
  
  output$n10 <- renderChart2({
    n10 = nPlot(x = "Gender", y = "Total_Conversions", data = data_selected_adset_gender(), type = "pieChart")
    n10$chart(tooltipContent = "#! function(key, y, e, graph){return '<h3>' + 'Category: ' + key + '</h3>' + '<p>'+ 'Value ' + y + '<br>' + ' % of value: ' + e.point.PERCENT} !#")
    n10$set(width = 800, height = 400) # mk changed width to 800 and height to 500
    n10$addParams(dom="n10")
    return(n10)
  })
  
  output$n14 <- renderChart2({
    n14 = nPlot(x = "Gender", y = "Total_Clicks", data = data_selected_adset_gender(), type = "pieChart")
    n14$chart(tooltipContent = "#! function(key, y, e, graph){return '<h3>' + 'Category: ' + key + '</h3>' + '<p>'+ 'Value ' + y + '<br>' + ' % of value: ' + e.point.PERCENT} !#")
    n14$set(width = 800, height = 400) # mk changed width to 800 and height to 500
    n14$addParams(dom="n14")
    return(n14)
  })
  
  #####Creative
  ##To group the data by creative
  #to generate the data frame
  data_creative_table <-reactive({
    data25 <- data()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data25$Date<-as.Date(as.character(data25$Date), "%Y%m%d")
    data25$Spent<-as.numeric(substring(as.character(data25$Spent),2))
    data25$Impressions<- as.numeric(data25$Impressions)
    data25$Campaign<- as.character(data25$Campaign)
    data25<-cbind(data25,str_split_fixed(data25$Ad.Set, "-", 5))
    data25<-cbind(data25,str_split_fixed(data25$Ad, "_", 2)[,1])
    colnames(data25)[16:21]<-c("ad_type","category","initial_bid","potential_reach","app_name","creative")
    data25 <- data25 %>% 
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(creative) %>% 
      summarize(Total_Impressions=sum(Impressions),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_CPI=Total_Spent/Total_Conversions, 
             Total_CPC=Total_Spent/Total_Clicks, 
             Total_CTR=Total_Clicks/Total_Impressions,
             Total_CVR=Total_Conversions/Total_Clicks)
    data25
  })
  
  #output the table in the ui
  output$creative_table <- renderDataTable({
    data_creative_table()
  })
  
  output$tb <- renderUI({
    if(is.null(data()))
      h5("Powered by Johnny Chiu",  heigth=200, width=200)
    else
      tabsetPanel(tabPanel("Adset",
                           dataTableOutput(outputId="adset_table"),
                           showOutput("h5", "Highcharts"),
                           showOutput("h6", "Highcharts"),
                           showOutput("h7", "Highcharts"),
                           showOutput(paste(if(input$kpi=="1"){"h16"}else{"h17"}), "Highcharts"),
                           showOutput(paste(if(input$kpi=="1"){"h18"}else{"h19"}), "Highcharts"),
                           showOutput(paste(if(input$kpi=="1"){"h20"}else{"h21"}), "Highcharts"),
                           showOutput(paste(if(input$kpi=="1"){"n8"}else{"n12"}), "nvd3"),
                           showOutput(paste(if(input$kpi=="1"){"n9"}else{"n13"}), "nvd3"),
                           showOutput(paste(if(input$kpi=="1"){"n10"}else{"n14"}), "nvd3")),
                  tabPanel("General", 
                           dataTableOutput(outputId="summary_table"),
                           showOutput("h1", "Highcharts"),
                           showOutput("h2", "Highcharts"),
                           showOutput("h3", "Highcharts"),
                           showOutput(paste(if(input$kpi=="1"){"h8"}else{"h9"}), "Highcharts"),
                           showOutput(paste(if(input$kpi=="1"){"h10"}else{"h11"}), "Highcharts"),
                           showOutput(paste(if(input$kpi=="1"){"h12"}else{"h13"}), "Highcharts"),
                           showOutput(paste(if(input$kpi=="1"){"h14"}else{"h15"}), "Highcharts")), 
                  tabPanel("Category",
                           dataTableOutput(outputId="category_table"),
                           showOutput(paste(if(input$kpi=="1"){"h4"}else{"h22"}), "Highcharts"),
                           showOutput(paste(if(input$kpi=="1"){"n5"}else{"n11"}), "nvd3")),
                  tabPanel("Creative",
                           dataTableOutput(outputId="creative_table"))
                  )
  })
  
})