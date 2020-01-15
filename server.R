library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(googleVis)

shinyServer(function(input, output, session) {

  observe({
    var_option2 <- var_option[var_option!= input$var1]
    updateSelectizeInput(
      session, "var2",
      choices = var_option2,
      selected = var_option2[1])
  })
  
  react_state_selected <- reactive({
    my_data %>%
      filter(.,State == input$State)
  })
  
  output$bar <- renderPlot({
        ggplot(react_state_selected() %>% 
                 group_by(.,year, month) %>% summarise(.,count_per_month=n()) %>%
                 group_by(.,month) %>% summarise(., Average=round(mean(count_per_month)))
                 , aes(x=month, y=Average)) +
        geom_col(position="dodge", fill = "#FF6666", width = 0.4) +
        xlab("Month") + ylab("") +
        ggtitle("Number of Accidents")
  })
  output$bar2 <- renderPlot({
    ggplot(rbind(react_state_selected() %>% 
             group_by(.,year, month, State) %>% 
             summarise(.,count=n())%>% inner_join(.,population,by="State") %>% select(.,-StateName) %>% 
             mutate(.,proportion=count/Population*100) %>% 
             group_by(., month) %>% summarise(.,Count=round(mean(count)), Proportion=round(mean(proportion),3)) %>% 
             mutate(., type=input$State),data_USA)
           , aes_string(x="month", y=input$bar,fill="type")) +
      geom_col(position="dodge", width = 0.5) +
      xlab("Month") +
      ylab("")+
      ggtitle("Number of Accidents")
  })
  
  output$heatmap <- renderPlot({
    ggplot(data=react_state_selected()) + 
      geom_bin2d(aes_string(input$var1,input$var2),na.rm =T) +
      scale_fill_gradient(low="#FCC8C5", high="#D10C00") 
      # theme(legend.position = "bottom")
  })
  
  output$gvis2 <- renderGvis({
    gvisPieChart(data1 %>% select(.,month,count.avg.month), 
                 options=list(width=500, height=550, title="Average Monthly Car Accidents (in %)"))
  })
  
  output$gvis3 <- renderGvis({
    gvisColumnChart(
      data2,
      options = list(
        legend='none',
        width = 1200,
        height = 600,
        axisTitlesPosition = "none",
        title = "Average Accidents Per Year"
      )
    )
  })
  
  
  output$USmap <- renderGvis({
    gvisGeoChart(
      data_2,"State",input$map,
      options = list(region = "US",displayMode = "regions",
                     resolution = "provinces",colors="['red']",width = 1000,height = 600))
  })

  output$visibility <- renderGvis({
    gvisColumnChart(
      data_visibility,
      options = list(
        title="Count of Accidents vs Visibility(in miles)",
        width = "1000",
        height = "600",
        hAxes="[{viewWindowMode:'explicit',
        viewWindow:{min:0, max:10}}]",
        legend='none',
        bar="{groupWidth:'100%'}"
      )
    )
  })
  
  output$precipitation <- renderPlot({
    ggplot(data_byZipcode, aes(avg.precipitation, accident.count)) + geom_line(stat="identity")
  })
  
  
})