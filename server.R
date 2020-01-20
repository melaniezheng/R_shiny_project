library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(googleVis)
library(shinydashboard)

shinyServer(function(input, output, session) {
  
  url_kaggle <- a("Kaggle", href="https://www.kaggle.com/sobhanmoosavi/us-accidents")
  url_population <- a("www.census.gov", href="https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html")
  url_insurance <- a("www.insure.com", href="https://www.insure.com/car-insurance/car-insurance-rates.html")

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
  
  react_map <- reactive({
    data_byStateYear %>% filter(between(year, input$year[1], input$year[2])) %>%
      group_by(.,State, StateName) %>% 
      summarise(.,Count=mean(Count), proportion=mean(proportion), Population=mean(Population))
  })
  
  react_state_selected <- reactive({
    my_data %>% filter(.,State == input$State)
  })
  
  react_state_year_selected <- reactive({
    react_state_selected() %>% filter(between(year, input$year2[1], input$year2[2]))
  })
  
  
  react_top3 <- reactive({
    react_map() %>% select(., -Population) %>% 
      gather(., key="Type", value="Number",c(Count,proportion)) %>% filter(., Type==input$map) %>% 
      arrange(.,desc(Number)) %>% top_n(.,3) %>% select(.,StateName)
  })
  
  react_topCounties <- reactive({
    react_state_year_selected() %>% 
      group_by(., State, County) %>% summarise(.,Count=n()) %>% 
      arrange(., desc(Count)) %>% top_n(.,3) 
  })
  
  day_night <- reactive({
    my_data %>%
      filter(.,State == input$State) %>% 
      filter(.,Severity %in% input$Severity) %>%
      group_by(.,year, month, State, day_night) %>% 
      summarise(.,count=n())%>% inner_join(.,population,by="State") %>% select(.,-StateName) %>% 
      mutate(.,proportion=count/Population*100) %>% 
      group_by(., month,day_night) %>% summarise(.,Count=round(mean(count)), Proportion=round(mean(proportion),3)) 
  })
  
  
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
    top2 <- react_top3()[2,2]
    # HTML('&nbsp;') to add 1 whitespace and HTML('&emsp;') to add 1 tab space
    HTML(paste(
      p(HTML("NO.2:"),HTML('&nbsp;'),top2)
    )
    )
  })
  
  output$top3 <- renderUI({ 
    top3 <- react_top3()[3,2]
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
      p(HTML('&nbsp;'),"Hover over the map to see more detail.")
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
    leaflet(react_state_year_selected()) %>% 
      #addPolygons(data=colState, stroke=F) %>% 
      addCircles(~Start_Lng, ~Start_Lat, color="#E82A2A") %>% 
      addProviderTiles('Esri.WorldStreetMap')
  })
  
  output$bar <- renderPlot({
    ggplot(rbind(react_state_selected() %>% filter(.,Severity %in% input$Severity) %>%
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
    ggplot(day_night(),
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
  
  output$density <- renderPlot({
    ggplot(data=react_state_selected() %>% gather(., key="key", value="value", c(input$var1,input$var2))) + 
      geom_density(aes(value, color=key), na.rm=T) +
      facet_wrap(~key, scales = "free")+
      labs(fill = "Weather Variables")+
      theme(legend.position="right")
  
  })
  
  ggplot(my_data %>% filter(., State=="AL")) +
    geom_density(aes_string("windspeed"), na.rm=T,color="#E87D7D")
  
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
      react_map(),"State",input$map,
      options = list(region = "US",displayMode = "regions",
                     resolution = "provinces",colors="['red']",width = 1000,height = 600))
  })
  
  # output$pie <- renderGvis({
  #   gvisPieChart(data1 %>% select(.,month,count.avg.month), 
  #                options=list(width=500, height=550, title="Average Monthly Car Accidents (in %)"))
  # })
  
  
})