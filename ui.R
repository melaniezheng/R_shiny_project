library(shiny)
library(googleVis)
library(shinythemes)

fluidPage(
  theme=shinytheme("cosmo"),
  navbarPage(
    title="Car Accident Analysis",
  tabPanel("Overview", 
           fluidRow(column(8, htmlOutput("USmap")),
                    column(4, 
                           h1("US Car Accident Analysis"),
                           h3("The data is gathered from US car accidents from Februrary 2016 to March 2019."),
                           htmlOutput("plot2"))
           )), 
  tabPanel("by State", 
           fluidRow(
             column(3, 
                    h3("Select a state."),
                    h3("Selectize Input."),
                    h3("Monthly Average per State vs Visibility(mi) Average"),
                    h3("Monthly Average per State vs Wind Speed(mph) Average"),
                    h3("Monthly Average per State vs per Wind Direction"),
                    h3("Monthly Average per State vs Precipitation(in) Average"),
                    h3("Monthly Average per State vs Humidity(%) Average"),
                    h3("Monthly Average per State vs Pressure(in) Average"),
                    h3("Monthly Average per State vs Temperature Average")),
             column(9, htmlOutput("plot1"))
           )
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
             ),
    tabPanel("...",
             htmlOutput("plot4")
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
