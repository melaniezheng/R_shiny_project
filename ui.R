library(shiny)
library(googleVis)
library(shinythemes)
library(shinydashboard)


fluidPage(
  theme = shinytheme("journal"),
  tags$head(
    tags$style(
      HTML(".navbar .navbar-nav {float: right}"),
      type = "text/css",
      "body {padding-top: 70px;}"
    )
  ),
  navbarPage(
    title = "US CAR ACCIDENTS",
    #title = div(img(src = "linkedin.png", height = "100px", style = "position: relative; top: -40px; right: -1100px;")),
    id = "nav",
    position = "fixed-top",
    tabPanel("OVERVIEW", icon = icon('map'),
             fluidRow(
               column(
                 8,
                 h2(strong("Car Accidents in the U.S.")),
                 h4(
                   "The data is gathered from 2.25 million US car accident records from Februrary 2016 to March 2019."
                 ),
                 htmlOutput("USmap")
               ),
               column(
                 3,
                 br(),
                 br(),
                 br(),
                 radioButtons(
                   "map",
                   label = h4(strong("Choose from:")),
                   choices = list(
                     "Count of Accidents" = "Count",
                     "Accidents/Population (in %)" = "proportion"
                   ),
                   selected = "Count"
                 ),
                 htmlOutput("map_desc"),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 wellPanel(style = "background: #E80000",
                           h4(strong(htmlOutput(
                             "top1"
                           ))), ),
                 wellPanel(style = "background: #E85E5E",
                           h4(strong(htmlOutput(
                             "top2"
                           )))),
                 wellPanel(style = "background: #E8AFAF",
                           h4(strong(htmlOutput(
                             "top3"
                           )))),
                 br()
                 #img(src="car_accident.jpg", width="320")
               ),
               column(1, )
             )),
    tabPanel(
      "EXPLORE",
      icon = icon('poll'),
      fluidRow(
        column(
          2,
          selectizeInput(
            inputId = "State",
            label = h4(strong("Select a state:")),
            choices = unique(data_2[, 'State'])
          ),
          wellPanel(style = "background: #E80000",
                    h4(strong(
                      htmlOutput("top1county")
                    ))),
          wellPanel(style = "background: #E85E5E",
                    h4(strong(
                      htmlOutput("top2county")
                    ))),
          wellPanel(style = "background: #E8AFAF",
                    h4(strong(
                      htmlOutput("top3county")
                    )))
        ),
        column(
          9,
          br(),
          h3(leafletOutput(
            "leaflet", width = 1050, height = 600
          ),
          style = "text-align: right;"),
          "Zoom in to view more details",
        )
      ),
      br(),
      hr(),
      fluidRow(
        column(
          2,
          radioButtons(
            "bar",
            label = h4(strong("Choose from:")),
            choices = list(
              "Count of Accidents" = "Count",
              "Accidents/Population (in %)" = "Proportion"
            ),
            selected = "Count"
          ),
          hr(),
          checkboxGroupInput(
            "Severity",
            label = h4(strong("Filter by severity:")),
            choices = (unique(sort(my_data$Severity))),
            selected = 0:4
          )
        ),
        #selectizeInput(inputId = "humidity2",label = h5(strong("Humidity Level:")),
        #choices = unique(my_data %>% filter(!is.na(humidity)) %>% select(.,humidity2))),
        column(5,
               plotOutput("bar"),
               htmlOutput("bar_desc")),
        column(5,
               plotOutput("bar2"))
      ),
      hr(),
      fluidRow(
        column(
          2,
          h4(strong("HEAT MAP")),
          selectizeInput(
            inputId = "var1",
            label = h5(strong("Select a variable:")),
            choices = var_option
          ),
          selectizeInput(
            inputId = "var2",
            label = h5(strong("Select another variable:")),
            choices = var_option
          )
        ),
        column(9,
               htmlOutput("heatmap_desc"),
               plotOutput("heatmap"))
      ),
      hr(),
      fluidRow(
        column(2),
        column(10, align="left",
               h4("CAR INSURANCE PREMIUM VS CAR ACCIDENTS PER CAPITA"),
               htmlOutput("insurance"),
               "hover over the line chart to see which states has the highest car insurance premium"
        )
        )),
    navbarMenu(
      "DATA",
      icon = icon("database"),
      tabPanel(
        "DATA TABLE",
        fluidRow(
          "Dataset is very large with 2.25 million records. ",
          selectizeInput(
            inputId = "State2",
            label = h4(strong("Choose a state:")),
            choices = unique(data_2[, 'State'])
          ),
          hr(),
          DT::dataTableOutput("table")
        )
      ),
      tabPanel("DATA SOURCE",
               h3("Data is found in Kaggle."))
    ),
    tabPanel(
      "ABOUT ME/CONTACT",
      fluidRow(
        br(),
        img(src = "melanie.png", width = "17%", style = "display: block; margin-left: auto; margin-right: auto;")
        #,style="border-radius: 50%"
      ),
      fluidRow(
        h3(strong("MELANIE ZHENG"), style = "text-align: center"),
        h5("meilingjung@gmail.com", style = "text-align: center")
      ),
      hr(),
      fluidRow(column(5, ""),
               column(
                 3,
                 tags$h3(
                   HTML('&nbsp;'),
                   HTML('&nbsp;'),
                   HTML('&nbsp;'),
                   tags$a(
                     href = 'https://www.linkedin.com/in/melanie-zheng/',
                     img(
                       src = 'LinkedIn.png',
                       title = "My LinkedIn",
                       height = "50px"
                     )
                   ),
                   HTML('&nbsp;'),
                   tags$a(href = 'https://github.com/melaniezheng/R_shiny_project/', img(
                     src = 'github.jpg',
                     title = "My Github",
                     height = "50px"
                   ))
                 )
               )),
      fluidRow(
        column(2, ""),
        column(
          1,
          h4(icon("briefcase"), style = "text-align: right; line-height: 165%;"),
          br(),
          br(),
          h4(icon("graduation-cap"), style = "text-align: right; line-height: 220%;"),
          br(),
          br(),
          br(),
          h4(icon("globe"), style = "text-align: right; line-height: 200%"),
          br(),
          br(),
          h4(icon("heart"), style = "text-align: right; line-height: 170%;")
        ),
        column(
          6,
          h4(
            "Currently a NYC Data Science fellow. Previously worked as a product manager at Viacom and project manager at Citigroup.",
            style = "text-align: left; line-height: 150%;"
          ),
          br(),
          h4(
            "Working on Master's Degree in Computer Science with Machine Learning specialization at Georgia Institute of Technology and obtained the Bachelor's degree in Mathematics from the Baruch College, CUNY, in New York City.",
            style = "text-align: left; line-height: 150%;"
          ),
          br(),
          h4(
            "I enjoy writing useful & reusable software tools for data analytics. Obsessed with creating beautiful, neat & informative data visualizations for complex datasets.",
            style = "text-align: left; line-height: 150%;"
          ),
          br(),
          h4(
            "Passionate about good food and good music. I love travelling, experiencing different culture and meeting new people :) ",
            style = "text-align: left; line-height: 150%;"
          )
        ),
        column(3, "")
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
