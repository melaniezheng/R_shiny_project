library(shiny)

fluidPage(
  titlePanel("Airbnb in NYC 2019"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "neighbourhood",
                     label = "Neighbourhood",
                     choices = unique(bnb[, 'neighbourhood'])),
      selectizeInput(inputId = "room_type",
                     label = "Room Type",
                     choices = unique(bnb[, 'room_type']))
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("px")),
        column(6, plotOutput("review"))
      )
    )
  )
)
