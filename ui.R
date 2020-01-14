library(shiny)
library(googleVis)
library(shinythemes)
library(shinydashboardPlus)

fluidPage(
  theme=shinytheme("cosmo"),
  tags$head(
    tags$style(HTML(".navbar .navbar-nav {float: left}"), 
               type="text/css", "body {padding-top: 70px;}"),
    #tags$script(HTML("var header = $('.navbar > .navbar-nav');
    #header.append('<div style=\"float:right\"><a href=\"https://www.linkedin.com/in/melanie-zheng/\"><img src=\"linkedin.png\" alt=\"alt\" style=\"position:relative;top:-40px;right:-1100px;\"> </a>`</div>');
    #console.log(header)")
    #),
    ),
  navbarPage(
    title = "US Car Accidents",
    #title = div(img(src = "linkedin.png", height = "100px", style = "position: relative; top: -40px; right: -1100px;")),
    id= "nav",
    position="fixed-top",
  tabPanel("Overview", 
           fluidRow(column(8, 
                           h2(strong("The Map View")),
                           h4("The data is gathered from US car accidents from Februrary 2016 to March 2019."),
                           htmlOutput("USmap")),
                    column(4, 
                           radioButtons("map", label = h3(strong("Display map by: ")),
                                        choices = list("Count of Accidents" = "Count",
                                                       "Accidents/Population (in %)" = "proportion"),
                                        selected = "Count"),
                           h5("Hover over the map to see more detail."))
                    )), 
  tabPanel("Explore", 
           fluidRow(
             column(3,
                    h3("Which month does your state have the most accidents?"),
                    selectizeInput(inputId = "State",label = "Select a state",choices = unique(data_byState[, 'State']))),
             column(9,
                    h3("Maybe add the national average for comparison.."),
                    plotOutput("plot1"))
               ),
           fluidRow(
             column(3,
                    h3("options...")),
             column(3,
                    radioButtons("day_night", label = h3(strong("Day/Night")),
                                 choices = list("Day" = "day","Night" = "night"),
                                 selected = "day"),
                    ),
             column(3,
                    selectizeInput(inputId = "humidity",label = h3(strong("Humidity Level:")),
                                   choices = unique((my_data[, 'humidity'])))),  #need to remove NA and add order
             column(3,
                    h3(strong("Another option")))),
           fluidRow(
             column(3, 
                    h3("Top 3 Counties with highest car accident count"),
                    h3("Monthly Average per State vs Visibility(mi) Average"),
                    h3("Monthly Average per State vs Wind Speed(mph) Average"),
                    h3("Monthly Average per State vs per Wind Direction"),
                    h3("Monthly Average per State vs Precipitation(in) Average"),
                    h3("Monthly Average per State vs Humidity(%) Average"),
                    h3("Monthly Average per State vs Pressure(in) Average"),
                    h3("Monthly Average per State vs Temperature Average")),
             column(9, 
                    h3("add a heat map - relationship count of accidents in different humidity/temperature"),
                    plotOutput("precipitation")))
           ),
  navbarMenu(
    "Interesting Findings",
    tabPanel("Participation/Humidity",
             "... to fill in ..."
             ),
    tabPanel("Visibility",
             htmlOutput("visibility")
             ),
    tabPanel("Wind Speed/DIrection",
             "... to fill in ..."
             )
  ),
  navbarMenu(
    "More",
    tabPanel("Data Source", 
             "... to fill in ..."
             ),
    tabPanel("About me", 
             "... to fill in ..."
             )
  )
  #mainPanel(fluidRow(
    #column(12,
           #"Monthly Accident Data",
          # fluidRow(
           #  column(9,
            #        htmlOutput("plot1")),
           #  column(3,
          #          htmlOutput("plot2"))
         #  ))
 # ))
))
