library(shiny)
library(googleVis)
library(shinythemes)
library(shinydashboardPlus)



fluidPage(
  theme=shinytheme("journal"),
  tags$head(
    tags$style(HTML(".navbar .navbar-nav {float: right}"), 
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
  tabPanel("OVERVIEW", 
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
  tabPanel("EXPLORE", 
           fluidRow(
             column(2,
                    selectizeInput(inputId = "State",label = h4(strong("Choose a state:")),
                                   choices = unique(data_byState[, 'State'])),
                    radioButtons("bar", label = h4(strong("Choose from:")),
                                 choices = list("Count of Accidents" = "Count",
                                                "Accidents/Population (in %)" = "Proportion"),
                                 selected = "Count"),
                    selectizeInput(inputId = "humidity2",label = h4(strong("Humidity Level:")),
                                   choices = unique(my_data %>% filter(!is.na(humidity)) %>% select(.,humidity2)))
                    ),
             column(5,
                    h5("Blue bar provides the national average for comparison"),
                    plotOutput("bar2")),
             column(5,
                    h3("plot"))
               ),
           hr(),
           fluidRow(
             column(2, 
                    h2(strong("HEAT MAP")),
                    selectizeInput(inputId = "var1",label = h4(strong("Select 1st variable:")),
                                   choices = var_option),
                    selectizeInput(inputId = "var2",label = h4(strong("Select 2nd variable:")),
                                   choices = var_option)),
             column(5, 
                    plotOutput("heatmap")))
           ),
  navbarMenu(
    "DATA",
    tabPanel("DATA TABLE",
             "... to fill in ...",
             htmlOutput("visibility")
             ),
    tabPanel("DATA SOURCE",
             "... to fill in ..."
             )
  ),
  tabPanel(
    "ABOUT ME/CONTACT",
    fluidRow(
      h1("about me")
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
