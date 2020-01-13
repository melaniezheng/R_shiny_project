library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(googleVis)

shinyServer(function(input, output) {
  output$linkedin <- renderUI({
    tags$a(imageOutput("linkedin.png"),href="https://www.linkedin.com/in/melanie-zheng-56ab7856/")
  })
  
  react_data_State <- reactive({
    my_data %>%
      filter(.,State == input$State) %>% 
      group_by(.,year, month) %>% summarise(.,count_per_month=n()) %>% 
      group_by(.,month) %>% summarise(., Average=round(mean(count_per_month)))
  })
  
  output$plot1 <- renderPlot({
        ggplot(data = react_data_State(), aes(x=month, y=Average)) +
        geom_col(position="dodge", fill = "#FF6666", width = 0.4) +
        xlab("Month") +
        ylab("")+
        ggtitle("Number of Accidents")
  })
  
  react_data_State <- reactive({
    my_data %>%
      filter(.,State == input$State) %>% 
      group_by(.,year, month) %>% summarise(.,count_per_month=n()) %>% 
      group_by(.,month) %>% summarise(., Average=round(mean(count_per_month)))
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
      data2,
      "State",
      "Count",
      options = list(
        title= "Hover over each state to see more details",
        region = "US",
        displayMode = "regions",
        resolution = "provinces",
        colors="['red']",
        width = 1000,
        height = 600
      )
    )
  })

  output$USmap1 <- renderGvis({
    gvisGeoChart(
      data_2,
      "State",
      "proportion",
      options = list(
        title= "Hover over each state to see more details",
        region = "US",
        displayMode = "regions",
        resolution = "provinces",
        colors="['red']",
        width = 1000,
        height = 600
      )
    )
  })
  
  output$plot4 <- renderGvis({
    gvisColumnChart(
      data3,
      options = list(
        legend='none',
        width = 1300,
        height = 600,
        axisTitlesPosition = "none",
        title = "Accident Location"
      )
    )
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