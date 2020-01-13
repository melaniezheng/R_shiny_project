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



#tabPanel("by State",
        # sidebarLayout(
        # sidebarPanel(
       #  selectizeInput(inputId = "State",label = "Select a state",choices = unique(data_byState[, 'State']))
       #  ),
       #  mainPanel(
      #   htmlOutput("plot1")
         #    )
         #  )
         #fluidRow(column(2, selectizeInput(inputId = "State",
         #      label = "Select a state",
         #      choices = unique(data_byState[, 'State']))),
         #  column(10, htmlOutput("plot1"))
         #)
