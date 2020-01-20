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
    id = "nav",
    position = "fixed-top",
    tabPanel("OVERVIEW", icon = icon('map'),
             fluidRow(
               column(8,
                 h2(strong("Car Accidents in the U.S.")),
                 h4(
                   "The data is gathered from 3 million US car accident records from Februrary 2016 to December 2019."
                 ),
                 htmlOutput("USmap")
               ),
               column(3,
                 br(),
                 br(),
                 radioButtons(
                   "map",
                   label = h4(strong("Choose from:")),
                   choices = list("Count of Accidents" = "Count",
                                  "Accidents per capita (in %)" = "proportion"),selected = "Count"),
                 tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #E84646;
                                                  border-top: 1px solid #E84E4E ;
                                                  border-bottom: 1px solid #E84E4E ;}
                                 .irs-from, .irs-to, .irs-single { background: #E84646 }"
                 )),
                 sliderInput("year", label = h4("Year Range"), min = 2016, 
                             max = 2019, value = c(2016, 2019), sep = ""),
                 "Hover over the states to see more detail.",
                 hr(),
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
               column(1, "")
             )),
    
    navbarMenu("EXPLORE",icon = icon("poll"),
    tabPanel(
      "BY STATE",
      fluidRow(
        column(
          2,
          selectizeInput(
            inputId = "State",
            label = h5(strong("Select a state:")),
            choices = unique(data_state$State)
          ),
          tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #E84646;
                                                  border-top: 1px solid #E84E4E ;
                                                  border-bottom: 1px solid #E84E4E ;}
                                 .irs-from, .irs-to, .irs-single { background: #E84646 }"
          )),
          sliderInput("year2", label = h5("Year Range"), min = 2016, 
                      max = 2019, value = c(2019, 2019), sep = ""),
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
          7,
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
          h4(strong("MONTHLY VIEW")),
          radioButtons(
            "bar",
            label = h5(strong("Choose from:")),
            choices = list(
              "Count of Accidents" = "Count",
              "Accidents per capita (in %)" = "Proportion"
            ),
            selected = "Count"
          ),
          hr(),
          checkboxGroupInput(
            "Severity",
            label = h5(strong("Filter by severity:")),
            choices = (unique(sort(my_data$Severity))),
            selected = 0:4
          )
        ),
        #selectizeInput(inputId = "humidity2",label = h5(strong("Humidity Level:")),
        #choices = unique(my_data %>% filter(!is.na(humidity)) %>% select(.,humidity2))),
        column(5,
               plotOutput("bar")),
        column(5,
               plotOutput("bar2"))
      ),
      hr(),
      fluidRow(
        column(
          2,
          h4(strong("WEATHER CONDITIONS")),
          br(),
          h5(strong("HEAT MAP/DENSITY GRAPH")),
          selectizeInput(
            inputId = "var1",
            label = h5(strong("Select a variable:")),
            choices = var_option
          ),
          selectizeInput(
            inputId = "var2",
            label = h5(strong("Select another variable:")),
            choices = var_option
          ),
          # hr(),
          # h5(strong("DENSITY GRAPH")),
          # selectizeInput(
          #   inputId = "density",
          #   label = h5(strong("Select a variable:")),
          #   choices = var_option
          # )
        ),
        column(5,
               plotOutput("heatmap")),
        column(5,
               plotOutput("density"))
      )),
    tabPanel("ACCIDENTS VS INSURANCE",
             fluidRow(align="center",
                      h3("Car Accidents per capita vs. Car Insurance Premium"),
                      "Numbers represent the annual average.",
                      htmlOutput("insurance"),
                      htmlOutput("insurance_desc")
             ))),
    navbarMenu(
      "DATA",
      icon = icon("database"),
      tabPanel(
        "ACCIDENTS DATA",
        fluidRow(column(3,
          "Dataset is very large with 3 million records. "),
          selectizeInput(
            inputId = "State2",
            label = h4(strong("Choose a state:")),
            choices = unique(data_state$State)
          )),
        fluidRow(
          uiOutput("url_kaggle", align="right"),
          hr(),
          DT::dataTableOutput("table")
        )
      ),
      tabPanel("POPULATION DATA",
               br(),h4("POPULATION DATA"),br(),
               fluidRow(column(6,
                               uiOutput("url_population", align="left"),
                               hr(),
                 DT::dataTableOutput("table_population")
               ))),
      tabPanel("INSURANCE DATA",
               br(),h4("INSURANCE DATA"),br(),
               fluidRow(column(6,
                               uiOutput("url_insurance"),
                               hr(),
                               DT::dataTableOutput("table_insurance"))))
    ),
    tabPanel(
      "ABOUT ME", icon = icon("linkedin"),
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
            "Currently a NYC Data Science fellow and CFA Exam Level III Candidate. Previously worked as a product manager at Viacom and project manager at Citigroup.",
            style = "text-align: left; line-height: 150%;"
          ),
          br(),
          h4(
            "Working on Master's Degree in Computer Science with Machine Learning specialization at Georgia Institute of Technology and obtained a Bachelor's degree in Mathematics from Baruch College, CUNY, in New York City.",
            style = "text-align: left; line-height: 150%;"
          ),
          br(),
          h4(
            "I enjoy writing useful & reusable software tools for data analytics. Obsessed with creating beautiful, neat & informative data visualizations for complex datasets.",
            style = "text-align: left; line-height: 150%;"
          ),
          br(),
          h4(
            "Passionate about good food and good music. I love travelling, experiencing different cultures, and meeting new people :) ",
            style = "text-align: left; line-height: 150%;"
          )
        ),
        column(3, "")
      )
    )

  ))
