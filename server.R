library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

function(input, output, session) {
  
  observe({
    roomtype <- unique(bnb %>%
                     filter(bnb$neighbourhood == input$neighbourhood) %>%
                     .$room_type)
    updateSelectizeInput(
      session, "room_type",
      choices = room_type,
      selected = room_type[1])
  })
  
  bnb_price_reviews <- reactive({
    bnb %>%
      filter(neighbourhood == input$neighbourhood & room_type == input$room_type) %>%
      group_by(neighbourhood, room_type) %>%
      summarise(n = n(),
                px = mean(price),
                reviews_count  = mean(number_of_reviews))
  })

  output$px <- renderPlot(
    bnb_price_reviews () %>%
      gather(key = room_type, value = px, neighbourhood, room_type) %>%
      ggplot(aes(x = neighbourhood, y = px, fill = room_type)) +
      geom_col(position = "dodge") +
      ggtitle("Average Price")
  )

  output$review <- renderPlot(
    bnb_price_reviews() %>%
      ggplot(aes(x = neighbourhood, y = n)) +
      geom_col(fill = "lightblue") +
      ggtitle("Number of listings")
  )
}