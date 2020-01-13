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
                           h3("The data is gathered from US car accidents from Februrary 2016 to March 2019."))
           )), 
  tabPanel("by State", 
           htmlOutput("plot1")
           ),
  navbarMenu(
    "Interesting Findings",
    tabPanel("Wind Speed"
             ),
    tabPanel("Visibility",
             htmlOutput("visibility")
             ),
    tabPanel("..."
             )
  ),
  navbarMenu(
    "More",
    tabPanel("Data Source", "to fill in"
             ),
    tabPanel("About me", htmlOutput("plot4")
             ),
    tabPanel("...")
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
