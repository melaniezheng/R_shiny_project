library(data.table)
library(dplyr)
library(ggplot2)
library(googleVis)

data <- fread(file = "US_Accidents_May19.csv")
#population data per state
#each state, location of accident


my_data <- data %>%  
  mutate(.,year=substr(Start_Time,1,4), month=month.abb[as.integer(substr(Start_Time,6,7))]) 
my_data$month <- factor(my_data$month, levels = month.abb, ordered = T)


data1 <- my_data %>% 
  group_by(.,year, month) %>% summarise(.,count_per_month=n()) %>% 
  left_join(.,my_data %>% group_by(.,year) %>% summarise(., count_per_year=n()),by="year") %>% 
  mutate(.,perc.mon.accident=count_per_month/count_per_year) %>% 
  group_by(.,month) %>% summarise(., mon.average=round(mean(count_per_month)))

data2 <- my_data %>% 
  group_by(.,State) %>% summarise(.,Count=n()) 

data_byState <- my_data %>% 
  group_by(.,State,County) %>% summarise(.,Count=n())

data_byZipcode <- my_data %>% 
  filter(., !is.na(`Precipitation(in)`)) %>% group_by(., Zipcode) %>% 
  summarise(., avg.precipitation=mean(`Precipitation(in)`), accident.count=n())



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

sample <- tail(my_data,10)
data_visibility <- my_data %>% group_by(.,`Visibility(mi)`) %>% summarise(.,Count=n())

