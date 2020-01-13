class(data)
nrow(data)

my_data <- data %>%  
  mutate(.,year=substr(Weather_Timestamp,1,4), month=month.abb[as.integer(substr(Weather_Timestamp,6,7))]) %>% 
  filter(.,is.na(month)) %>% 
  mutate(., year =substr(Start_Time,1,4), month = month.abb[as.integer(substr(Start_Time,6,7))]) 
my_data$month <- factor(my_data$month, levels = month.abb, ordered = T)

pie <- ggplot(my_data %>% 
                group_by(.,year, month) %>% 
                summarise(.,count_per_month=n()) %>% 
                left_join(.,my_data %>% group_by(.,year) %>% summarise(., count_per_year=n()),by="year") %>% 
                mutate(.,perc.mon.accident=count_per_month/count_per_year) %>% 
                group_by(.,month) %>% 
                summarise(., mon.average=mean(count_per_month)) 
              , aes(x="",y=mon.average, group=factor(month),fill=factor(month))) + 
  geom_bar(stat="identity", width=1,position = 'dodge') + xlab("Month") + ylab("Average No. of Accident")+
  geom_text(aes(label=round(mon.average)))

pie


pie <- pie + coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(perc.average*100), "%")), position = position_stack(vjust = 0.5))+
  scale_fill_brewer(aesthetics="fill")+ theme_void() + theme(axis.text.x=element_blank())



data1 <- my_data %>% 
  group_by(.,year, month) %>% 
  summarise(.,count_per_month=n()) %>% 
  left_join(.,my_data %>% group_by(.,year) %>% summarise(., count_per_year=n()),by="year") %>% 
  mutate(.,perc.mon.accident=count_per_month/count_per_year) %>% 
  group_by(.,month) %>% 
  summarise(., mon.average=mean(count_per_month))
data1



tabPanel("by State", 
         fluidRow(
           boxPlus(
             width = 12,
             title = "Select a state", 
             closable = TRUE, 
             status = "warning", 
             solidHeader = FALSE, 
             collapsible = TRUE,
             enable_sidebar = TRUE,
             sidebar_width = 30,
             sidebar_start_open = TRUE,
             sidebar_content = tagList(
               selectizeInput(inputId = "State",label = "Select a state",choices = unique(data_byState[, 'State']))
             ),
             plotOutput("precipitation")
           )),
         fluidRow(
           column(3, 
                  h3("Select a state."),
                  h3("Selectize Input."),
                  h3("Top 3 Counties with highest car accident count"),
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