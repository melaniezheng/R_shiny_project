library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(googleVis)


data <- fread(file = "~/NYCDSA/R_shiny_project/US_Accidents_May19.csv", stringsAsFactors = FALSE)
#population data per state
population <- fread(file= "~/NYCDSA/R_shiny_project/US_Population_2019_Jul.csv", header = TRUE, stringsAsFactors = FALSE)

#clean up population data
colnames(population)=c("StateName","State","Population")
population <- population[-1,]
population$Population=as.integer(population$Population)

kPopulation <- sum(population$Population)

#join accidents data with population data
my_data <- data %>%  
  left_join(., population, by="State") %>% 
  mutate(.,year=substr(Start_Time,1,4), month=factor(month.abb[as.integer(substr(Start_Time,6,7))], levels=month.abb, ordered = T)) %>% 
  rename(., temperature = `Temperature(F)`, windChill=`Wind_Chill(F)`,humidity=`Humidity(%)`,pressure=`Pressure(in)`,
         visibility=`Visibility(mi)`, windspeed=`Wind_Speed(mph)`, precipitation=`Precipitation(in)`)

# humidity vs no. of accidents -- positive correlation
ggplot(my_data %>% group_by(.,humidity) %>% summarise(., count=n()), aes(humidity, count)) + geom_point(na.rm=T)
ggplot(my_data %>% group_by(.,temperature) %>% summarise(., count=n()), aes(temperature, count)) + geom_point(na.rm=T)+ ylim(0,5000)
ggplot(my_data %>% group_by(.,visibility) %>% summarise(., count=n()), aes(visibility, count)) + geom_point(na.rm=T)
ggplot(my_data %>% group_by(.,precipitation) %>% summarise(., count=n()), aes(precipitation, count)) + geom_point(na.rm=T) + ylim(0,5000)
ggplot(my_data %>% group_by(.,windspeed) %>% summarise(., count=n()), aes(windspeed, count)) + geom_point(na.rm=T)  + ylim(0,5000)
ggplot(my_data %>% group_by(.,windChill) %>% summarise(., count=n()), aes(windChill, count)) + geom_point(na.rm=T) + ylim(0,5000)


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
)

data3 <- my_data %>% select(.,State,Amenity,Bump,Crossing,Give_Way,Junction,No_Exit,Railway,Roundabout,Station,Stop) %>% 
  filter(.,State=="CA") %>% 
  mutate(.,
         BUMP=ifelse(Bump=="TRUE",1,0),
         CROSSING=ifelse(Crossing=="TRUE",1,0),
         GIVE_WAY=ifelse(Give_Way=="TRUE",1,0),
         JUNCTION=ifelse(Junction=="TRUE",1,0),
         NO_EXIT=ifelse(No_Exit=="TRUE",1,0),
         RAILWAY=ifelse(Railway=="TRUE",1,0),
         ROUNDABOUT=ifelse(Roundabout=="TRUE",1,0),
         STATION=ifelse(Station=="TRUE",1,0),
         STOP=ifelse(Stop=="TRUE",1,0)) %>% 
  group_by(.,State) %>% 
  summarise(.,
            Bump_count=sum(BUMP),
            Crossing_count=sum(CROSSING),
            GiveWay_count=sum(GIVE_WAY),
            Junction_count=sum(JUNCTION),
            NoExit_count=sum(NO_EXIT),
            Railway_count=sum(RAILWAY),
            Roundabout_count=sum(ROUNDABOUT),
            Station_count=sum(STATION),
            Stop_count=sum(STOP))
