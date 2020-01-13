library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(googleVis)

shinyServer(function(input, output) {
  output$plot1 <- renderGvis({
    gvisColumnChart(
      my_data %>% 
        filter(.,State=="CA") %>% 
        group_by(.,year, month) %>% 
        summarise(.,count_per_month=n()) %>% 
        left_join(.,my_data %>% group_by(.,year) %>% summarise(., count_per_year=n()),by="year") %>% 
        mutate(.,perc.mon.accident=count_per_month/count_per_year) %>% 
        group_by(.,month) %>% 
        summarise(., mon.average=round(mean(count_per_month))),
      options = list(
        legend='none',
        width = 1100,
        height = 600,
        axisTitlesPosition = "none",
        Title = "Average Monthly Car Accident (2016-2019)"
      )
    )
  })
  output$plot2 <- renderGvis({
    gvisPieChart(data1, options=list(width=400, height=400))
  })
  output$plot3 <- renderGvis({
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
  
})