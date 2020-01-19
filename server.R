library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(googleVis)
library(shinydashboard)

shinyServer(function(input, output, session) {
  
  url_kaggle <- a("Kaggle", href="https://www.kaggle.com/sobhanmoosavi/us-accidents")
  url_population <- a("Wikipedia", href="https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population")
  url_insurance <- a("insure.com", href="https://www.insure.com/car-insurance/car-insurance-rates.html")

  output$url_kaggle <- renderUI({
    tagList("Source:", url_kaggle)
  })
  
  output$url_population <- renderUI({
    tagList("Source:", url_population)
  })
  
  output$url_insurance <- renderUI({
    link <- tagList("Source:",url_insurance)
  })
  
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
  
  react_topCounties <- reactive({
    data_county %>% filter(.,State==input$State) %>% 
      arrange(., desc(Count)) %>% top_n(.,3) 
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
  
  # react_plotVar_selected <- reactive({
  #   my_data %>%
  #     filter(., State == input$State) %>%
  #     group_by(.,my_data[,"humidity"]) %>%
  #     summarise(., Count=n()) %>% 
  #     `colnames<-`(c("humidity", "Count"))
  # })
  
  react_dt <- reactive({
    my_data %>% filter(.,State==input$State2)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(react_dt(), rownames=FALSE) %>% 
      DT::formatStyle("State",background="#E88E8E",fontWeight='bold')
  })
  
  output$table_population <- DT::renderDataTable({
    DT::datatable(population, rownames=FALSE) %>% 
      DT::formatStyle("StateName",background="#E88E8E",fontWeight='bold')
  })
  
  output$table_insurance <- DT::renderDataTable({
    DT::datatable(insurance, rownames=FALSE) %>% 
      DT::formatStyle("StateName",background="#E88E8E",fontWeight='bold')
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

  output$top1county <- renderUI({ 
    top1 <- react_topCounties()[1,"County"]
    cnt <- react_topCounties()[1,"Count"]
    # HTML('&nbsp;') to add 1 whitespace and HTML('&emsp;') to add 1 tab space
    HTML(paste(
      p(HTML("NO.1:"),HTML('&nbsp;'),top1, HTML("<br />"),
        HTML("Count:"), HTML('&nbsp;'), cnt)
    )
    )
  })
  
  output$top2county <- renderUI({ 
    top2 <- react_topCounties()[2,"County"]
    cnt <- react_topCounties()[2,"Count"]
    # HTML('&nbsp;') to add 1 whitespace and HTML('&emsp;') to add 1 tab space
    HTML(paste(
      p(HTML("NO.2:"),HTML('&nbsp;'),top2, HTML("<br />"),
        HTML("Count:"), HTML('&nbsp;'), cnt)
    )
    )
  })
  
  output$top3county <- renderUI({ 
    top3 <- react_topCounties()[3,"County"]
    cnt <- react_topCounties()[3,"Count"]
    # HTML('&nbsp;') to add 1 whitespace and HTML('&emsp;') to add 1 tab space
    HTML(paste(
      p(HTML("NO.3:"),HTML('&nbsp;'),top3, HTML("<br />"),
        HTML("Count:"), HTML('&nbsp;'), cnt)
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
  
  output$heatmap_desc <- renderUI({ 
    # HTML('&nbsp;') to add 1 whitespace and HTML('&emsp;') to add 1 tab space
    HTML(paste(
      p(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
        HTML('&nbsp;'),"Explore the relationship between different weather variables and the number of accidents.")
    )
    )
  })
  
  output$insurance_desc <- renderUI({ 
    # HTML('&nbsp;') to add 1 whitespace and HTML('&emsp;') to add 1 tab space
    HTML(paste(
      p(HTML('&nbsp;'),HTML('&nbsp;'),
        HTML('&nbsp;'),"Hover over the line chart to look at numbers for a specific state.")
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
  # output$plot_desc <- renderUI({ 
  #   HTML(
  #     p("Discover the correlation between the count of accident and different variables")
  #   )
  # })
  
  # output$plot <- renderPlot({
  #   ggplot(react_plotVar_selected() %>% mutate_all(., function(x) {as.integer(x)}),
  #          aes_string("humidity","Count")) +
  #     geom_point(na.rm=T, color='#E86E6E') +
  #     ylab("") + ggtitle("Count")
  # })

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
      addCircles(~Start_Lng, ~Start_Lat, color="#E82A2A") %>% 
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
      xlab("") + ylab("") + ggtitle(input$bar)
  })
  
  output$bar2 <- renderPlot({
    ggplot(data_4(),
           aes_string(x="month", y=input$bar,fill="day_night")) +
      geom_col(width = 0.5) +
      xlab("") + ylab("") + ggtitle(input$bar)
  })
  
  output$heatmap <- renderPlot({
    ggplot(data=react_state_selected()) + 
      geom_bin2d(aes_string(input$var1,input$var2),na.rm =T) +
      scale_fill_gradient(low="#E8B5B5", high="#E80000") + xlab(input$var1) + ylab("") + 
      ggtitle(input$var2)
      # theme(legend.position = "bottom")
  })
  
  output$insurance <- renderGvis({
    gvisLineChart(
      df[, c("type", "Accidents", "Insurance")],
      "type",c("Accidents", "Insurance"),
      options = list(
        width = "1000px",
        height = "600px",
        series = "[{targetAxisIndex: 0},{targetAxisIndex:1}]",
        hAxe = "{title:'States'}",
        vAxes = "[{title:'Car Accident Per Capita (in %)'}, {title:'Insurance ($)'}]"
      )
    )
  })

  
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
  
})