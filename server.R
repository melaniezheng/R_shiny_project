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
  
  # 'overview' map
  react_map <- reactive({
    data_byStateYear %>% filter(between(year, input$year[1], input$year[2])) %>%
      group_by(.,State, StateName) %>% 
      summarise(.,Count=round(sum(Count)), proportion=sum(proportion), Population=mean(Population))
  })

  react_state_selected <- reactive({
    my_data %>% filter(.,State == input$State) %>% filter(.,Severity %in% input$Severity) 
  })
  
  react_state_year_selected <- reactive({
    react_state_selected() %>% mutate(., Date=as.Date(Date)) %>% 
      filter(between(Date, input$daterange[1], input$daterange[2]))
  })

  # top 3 states in 'overview' page
  react_top3 <- reactive({
    react_map() %>%  ungroup() %>% select(., -Population) %>% 
      gather(., key="Type", value="Number",c(Count,proportion)) %>% filter(., Type==input$map) %>% 
      arrange(.,desc(Number)) %>% top_n(.,3) %>% select(.,StateName)
  })
  
  # top 3 counties in 'explore' page
  react_topCounties <- reactive({
    react_state_year_selected() %>% 
      group_by(., State, County) %>% summarise(.,Count=n()) %>% 
      arrange(., desc(Count)) %>% top_n(.,3) 
  })
  
  reactive_bar_df <- reactive({
    my_data %>%
      filter(.,State == input$State) %>% 
      filter(.,Severity %in% input$Severity) %>% 
      filter(.,day_night %in% c('Day','Night')) %>% 
      group_by(.,year, month, State, day_night) %>% 
      summarise(.,count=n())%>% inner_join(.,population_raw,by=c("year","State")) %>%
      mutate(.,proportion=count/Population*1000) 
  })
  
  react_dt <- reactive({
    my_data %>% filter(.,State==input$State2)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(react_dt(), rownames=FALSE) %>% 
      DT::formatStyle("State",background="#E88E8E",fontWeight='bold')
  })
  
  output$table_population <- DT::renderDataTable({
    DT::datatable(population_raw %>% mutate(.,Population=format(round(as.numeric(Population)), big.mark=",")), rownames=FALSE) %>% 
      DT::formatStyle("StateName",background="#E88E8E",fontWeight='bold')
  })
  
  output$table_insurance <- DT::renderDataTable({
    DT::datatable(insurance %>% mutate(.,Insurance=paste0("$", formatC(as.numeric(Insurance), big.mark=","))), rownames=FALSE) %>% 
      DT::formatStyle("StateName",background="#E88E8E",fontWeight='bold')
  })

  output$top1 <- renderUI({ 
    top1 <- first(react_top3()$StateName)
    HTML(paste(
      p(HTML("NO.1:"),HTML('&nbsp;'),top1)
    )
    )
  })
  
  output$top2 <- renderUI({ 
    top2 <- react_top3()[2,"StateName"]
    HTML(paste(
      p(HTML("NO.2:"),HTML('&nbsp;'),top2)
    )
    )
  })
  
  output$top3 <- renderUI({ 
    top3 <- react_top3()[3,"StateName"]
    HTML(paste(
      p(HTML("NO.3:"),HTML('&nbsp;'),top3)
    )
    )
  })

  output$top1county <- renderUI({ 
    top1 <- react_topCounties()[1,"County"]
    cnt <- react_topCounties()[1,"Count"]
    HTML(paste(
      p(HTML("NO.1:"),HTML('&nbsp;'),top1, HTML("<br />"),
        HTML("Count:"), HTML('&nbsp;'), cnt)
    )
    )
  })
  
  output$top2county <- renderUI({ 
    top2 <- react_topCounties()[2,"County"]
    cnt <- react_topCounties()[2,"Count"]
    HTML(paste(
      p(HTML("NO.2:"),HTML('&nbsp;'),top2, HTML("<br />"),
        HTML("Count:"), HTML('&nbsp;'), cnt)
    )
    )
  })
  
  output$top3county <- renderUI({ 
    top3 <- react_topCounties()[3,"County"]
    cnt <- react_topCounties()[3,"Count"]
    HTML(paste(
      p(HTML("NO.3:"),HTML('&nbsp;'),top3, HTML("<br />"),
        HTML("Count:"), HTML('&nbsp;'), cnt)
    )
    )
  })
  
  output$map_desc <- renderUI({ 
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

  # leaflet map
  output$leaflet <- renderLeaflet({
    leaflet(react_state_year_selected()) %>% 
      addCircles(~Start_Lng, ~Start_Lat, color="#E82A2A") %>% 
      addProviderTiles('Esri.WorldStreetMap')
  })
  
  # monthly average bar chart
  output$bar <- renderPlot({
    ggplot(rbind(
      react_state_selected() %>%
        group_by(., year, month, State) %>%
        summarise(., count = n()) %>% inner_join(., population_raw, by =c("year", "State")) %>%
        mutate(., proportion = round(count/Population*1000,2)) %>%
        group_by(., year, month) %>% summarise(avg = mean(count), avg_prop =mean(proportion)) %>%
        group_by(., month) %>% summarise(., Count = round(mean(avg)), Proportion =round(mean(avg_prop),2)) %>%
        mutate(., type = input$State),
      my_data %>% filter(., Severity %in% input$Severity) %>%
        group_by(., year, month, State) %>% summarise(., count = n()) %>%
        inner_join(., population_raw, by = c("year", "State")) %>%
        mutate(., proportion = round(count/Population*1000,2)) %>%
        group_by(., year, month) %>% summarise(avg = mean(count), avg_prop =mean(proportion)) %>%
        group_by(., month) %>% summarise(., Count = round(mean(avg)), Proportion =round(mean(avg_prop),2)) %>%
        mutate(., type = "USA")
    ) ,aes_string(x="month", y=input$bar, fill="type")) +
      geom_col(position="dodge", width = 0.5) +
      xlab("") + ylab("") + ggtitle(input$bar)
  })
  
  # day_night bar chart
  output$bar2 <- renderPlot({
    ggplot(reactive_bar_df() %>% group_by(., month,day_night) %>% 
             summarise(.,Count=round(mean(count)), Proportion=round(mean(proportion),2)) ,
           aes_string(x="month", y=input$bar,fill="day_night")) +
      geom_col(width = 0.5) +
      xlab("") + ylab("") + ggtitle(input$bar)
  })
  
  # weather var heatmap
  output$heatmap <- renderPlot({
    ggplot(data=react_state_year_selected()) + 
      geom_bin2d(aes_string(input$var1,input$var2),na.rm =T) +
      scale_fill_gradient(low="#E8B5B5", high="#E80000") + xlab(input$var1) + ylab(input$var2)
  })
  
  # explore tab 
  output$density <- renderPlot({
    ggplot(data=react_state_year_selected() %>% gather(., key="key", value="value", c(input$var1,input$var2)),aes(value)) + 
      geom_histogram(aes(fill=key),na.rm=T, color="#EAE9E9",stat='bin', binwidth = 5) +
      facet_wrap(~key, scales = "free")+
      labs(fill = "Weather Variables")+
      theme(legend.position="right")
  
  })
  
  # insurance line chart
  output$insurance <- renderGvis({
    gvisLineChart(
      df[, c("type", "Accidents", "Insurance")],
      "type",c("Accidents", "Insurance"),
      options = list(
        width="1150px", height="400px",
        series = "[{targetAxisIndex: 0},{targetAxisIndex:1}]",
        hAxe = "{title:'States'}",
        vAxes = "[{title:'Car Accident Per Capita (in %)'}, {title:'Insurance ($)'}]"
      )
    )
  })

  # overview US map
  output$USmap <- renderGvis({
    gvisGeoChart(
      react_map(),"State",input$map,
      options = list(region = "US",displayMode = "regions",
                     resolution = "provinces",colors="['red']",width = 1000,height = 600))
  })
  
  # time series chart
  output$timeseries <- renderGvis({
    gvisAnnotationChart(as.data.frame(react_state_year_selected() %>% group_by(., Date, State) %>% summarise(.,Count=n()) %>% 
                                        mutate(., Count=as.numeric(Count), Day=as.character(weekdays(as.Date(Date))),
                                               Is.Sunday=ifelse(Day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),"", "Sunday"))) %>% 
                          filter(., State==input$State), 
                        datevar = "Date", numvar="Count",annotationvar = "Is.Sunday",#idvar="Day",
                        options=list(displayAnnotations=TRUE,
                                     legendPosition='newRow',
                                     width=1150, height=400))
  })

  
})
