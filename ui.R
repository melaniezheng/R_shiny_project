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
           fluidRow(column(7, 
                           h2(strong("The Map View")),
                           h4("The data is gathered from US car accidents from Februrary 2016 to March 2019."),
                           htmlOutput("USmap")),
                    column(4,
                           h3("  "),
                           radioButtons("map", label = h4(strong("Choose from:")),
                                        choices = list("Count of Accidents" = "Count",
                                                       "Accidents/Population (in %)" = "proportion"),
                                        selected = "Count"),
                           h5("Hover over the map to see more detail.")))
  ), 
  tabPanel("Explore", 
           fluidRow(
             column(2,
                    wellPanel(
                    selectizeInput(inputId = "State",label = h4(strong("Choose a state:")),
                                   choices = unique(data_byState[, 'State'])),
                    radioButtons("day_night", label = h5(strong("Day/Night")),
                                 choices = list("Day" = "day","Night" = "night"),
                                 selected = "day")),
                    selectizeInput(inputId = "humidity2",label = h4(strong("Humidity Level:")),
                                   choices = unique(my_data %>% filter(!is.na(humidity)) %>% select(.,humidity2)))
                    ),
             column(10,
                    h5("Maybe add the national average for comparison.."),
                    plotOutput("plot1"))
               ),
           fluidRow(
             column(2, 
                    h2(strong("HEAT MAP"))),
             column(2,
                    selectizeInput(inputId = "var1",label = h4(strong("Select 1st variable:")),
                                   choices = var_option)),
             column(2,
                    selectizeInput(inputId = "var2",label = h4(strong("Select 2nd variable:")),
                                   choices = var_option))),
           fluidRow(
             column(2, 
                    h4(" ")),
             column(10, 
                    
                    plotOutput("heatmap")))
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
