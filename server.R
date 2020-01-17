library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(googleVis)
library(shinydashboard)

shinyServer(function(input, output, session) {

  observe({
    var_option2 <- var_option[var_option!= input$var1]
    updateSelectizeInput(
      session, "var2",
      choices = var_option2,
      selected = var_option2[1])
  })
  
  react_state_selected <- reactive({
    my_data %>% filter(.,State == input$State)
  })
  
  
  react_top3 <- reactive({
    data_2 %>% 
      gather(., key="Type", value="Number",c(Count,proportion)) %>% filter(., Type==input$map) %>% 
      arrange(.,desc(Number)) %>% top_n(.,3) %>% select(.,StateName)
  })
  
  react_topCities <- reactive({
    data_city %>% filter(.,State==input$State) %>% 
      arrange(., desc(Count)) %>% top_n(.,3) %>% select(.,City)
  })
  
  data_4 <- reactive({
    my_data %>%
      filter(.,State == input$State) %>% 
      filter(.,Severity %in% input$Severity) %>%
      group_by(.,year, month, State, day_night) %>% 
      summarise(.,count=n())%>% inner_join(.,population,by="State") %>% select(.,-StateName) %>% 
      mutate(.,proportion=count/Population*100) %>% 
      group_by(., month,day_night) %>% summarise(.,Count=round(mean(count)), Proportion=round(mean(proportion),3)) 
  })
  
  react_plotVar_selected <- reactive({
    my_data %>% 
      filter(., State== input$State) %>%
      group_by(.,my_data[,input$plot_var]) %>% summarise(., Count=n()) %>%
      `colnames<-`(c(input$plot_var, "Count"))
  })

  output$top1 <- renderUI({ 
    top1 <- first(react_top3()$StateName)
    # HTML('&nbsp;') to add 1 whitespace and HTML('&emsp;') to add 1 tab space
    HTML(paste(
      p(HTML("NO.1:"),HTML('&nbsp;'),top1)
    )
    )
  })
  
  output$top2 <- renderUI({ 
    top2 <- react_top3()[2,]
    # HTML('&nbsp;') to add 1 whitespace and HTML('&emsp;') to add 1 tab space
    HTML(paste(
      p(HTML("NO.2:"),HTML('&nbsp;'),top2)
    )
    )
  })
  
  output$top3 <- renderUI({ 
    top3 <- react_top3()[3,]
    # HTML('&nbsp;') to add 1 whitespace and HTML('&emsp;') to add 1 tab space
    HTML(paste(
      p(HTML("NO.3:"),HTML('&nbsp;'),top3)
    )
    )
  })

  output$map_desc <- renderUI({ 
    # HTML('&nbsp;') to add 1 whitespace and HTML('&emsp;') to add 1 tab space
    HTML(paste(
      p(HTML('&nbsp;'),HTML('&nbsp;'),"Hover over the map to see more detail.")
    )
    )
  })
  output$bar_desc <- renderUI({ 
    HTML(paste(
      p(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
        "Blue bars provide the national average for comparison.")
    )
    )
  })
  output$plot_desc <- renderUI({ 
    HTML(
      p("Discover the correlation between the count of accident and different variables")
    )
  })
  
  output$plot <- renderPlot({
    ggplot(react_state_selected(),
           aes_string(x=input$plot_var, y="Count")) +
      geom_point(na.rm=T, color='#E86E6E')
  })

  # output$plot2 <- renderGvis({
  #   gvisScatterChart(react_plotVar_selected()[,c(input$bar,"Count"), drop=FALSE],
  #                    options = list(title=input$bar,
  #                                   width = "500",height = "500",#colors: "['#E86E6E']",
  #                                   legend='none')
  #                    )
  # })
  
  output$leaflet <- renderLeaflet({
    leaflet(react_state_selected()) %>% 
      #addPolygons(data=colState, stroke=F) %>% 
      addCircles(~Start_Lng, ~Start_Lat) %>% 
      addProviderTiles('Esri.WorldStreetMap')
  })
  
  output$bar <- renderPlot({
    ggplot(rbind(react_state_selected() %>% 
                   group_by(.,year, month, State) %>% 
                   summarise(.,count=n())%>% inner_join(.,population,by="State") %>% select(.,-StateName) %>% 
                   mutate(.,proportion=count/Population*100) %>% 
                   group_by(., month) %>% summarise(.,Count=round(mean(count)), Proportion=round(mean(proportion),3)) %>% 
                   mutate(., type=input$State),data_USA)
           , aes_string(x="month", y=input$bar,fill="type")) +
      geom_col(position="dodge", width = 0.5) +
      xlab("") +
      ylab(input$bar)
  })
  
  output$bar2 <- renderPlot({
    ggplot(data_4(),
           aes_string(x="month", y=input$bar,fill="day_night")) +
      geom_col(width = 0.5) +
      xlab("") +
      ylab(input$bar)
  })
  
  output$heatmap <- renderPlot({
    ggplot(data=react_state_selected()) + 
      geom_bin2d(aes_string(input$var1,input$var2),na.rm =T) +
      scale_fill_gradient(low="#E8B5B5", high="#E80000") 
      # theme(legend.position = "bottom")
  })
  
  # output$pie <- renderGvis({
  #   gvisPieChart(data1 %>% select(.,month,count.avg.month), 
  #                options=list(width=500, height=550, title="Average Monthly Car Accidents (in %)"))
  # })
  
  # output$gvis3 <- renderGvis({
  #   gvisColumnChart(
  #     data2,
  #     options = list(
  #       legend='none',width = 1200,height = 600,
  #       axisTitlesPosition = "none",title = "Average Accidents Per Year"
  #     )
  #   )
  # })
  
  
  output$USmap <- renderGvis({
    gvisGeoChart(
      data_2,"State",input$map,
      options = list(region = "US",displayMode = "regions",
                     resolution = "provinces",colors="['red']",width = 1000,height = 600))
  })
  
  # output$USmap2 <- renderGvis({
  #   
  #   library(googleVis)
  #   test <- gvisGeoChart(data_County_geocode,"County","Count",
  #                        options = list(region = "US-AL",displayMode = "regions",
  #                                       resolution = "metros",colors="['red']",width = 1000,height = 600))
  #   plot(test)
  # })  
  

  # output$visibility <- renderGvis({
  #   gvisColumnChart(
  #     data_visibility,
  #     options = list(title="Count of Accidents vs Visibility(in miles)",
  #       width = "1000",height = "600",
  #       hAxes="[{viewWindowMode:'explicit',viewWindow:{min:0, max:10}}]",
  #       legend='none',bar="{groupWidth:'100%'}"
  #     )
  #   )
  # })
  
  
  
})