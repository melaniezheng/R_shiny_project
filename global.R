library(data.table)
library(dplyr)
library(ggplot2)
library(googleVis)

data <- fread(file = "~/NYCDSA/R_shiny_project/US_Accidents_May19.csv", stringsAsFactors = FALSE)
#population data per state
population <- fread(file= "~/NYCDSA/R_shiny_project/US_Population_2019_Jul.csv", header = TRUE, stringsAsFactors = FALSE)

colnames(population)=c("StateName","State","Population")
population <- population[-1,]
population$Population=as.integer(population$Population)
kPopulation <- sum(population$Population)

############ to delete ###########
# set humidity category 0-25, 25-50, 50-75, 75-100


##################################
my_data <- data %>%  
  left_join(., population, by="State") %>% 
  mutate(.,year=substr(Start_Time,1,4), 
         month=factor(month.abb[as.integer(substr(Start_Time,6,7))], levels=month.abb, ordered = T)) %>% 
  mutate(., humidity=case_when(`Humidity(%)`<25 ~ '<25%',
                               `Humidity(%)`>=25 & `Humidity(%)`<=50 ~ '25%-50%',
                               `Humidity(%)`>=50 & `Humidity(%)`<=75 ~ '50%-75%',
                               `Humidity(%)`>75 ~ '>75%')) %>% 
  rename(., day_night=Sunrise_Sunset) %>% 
  select(., -Turning_Loop, -Civil_Twilight, -Nautical_Twilight)

# monthly average accident count and average accident % in proportion to the population (ideally 
# in proportion to the number of drivers would make much more sense)
data1 <- my_data %>% 
  group_by(.,year, month) %>% summarise(., count.each.month=n()) %>% 
  group_by(.,month) %>% summarise(., count.avg.month=round(mean(count.each.month))) %>% 
  mutate(., proportion = count.avg.month/kPopulation)

# accident count per state.
data2 <- my_data %>% 
  group_by(.,State) %>% summarise(.,Count=n()) 

# accident in proportion to state population.
data_2 <- my_data %>% 
  group_by(.,State) %>% summarise(.,Count=n()) %>% 
  left_join(.,population,by="State") %>% 
  mutate(.,proportion=round(Count/Population*100,2))

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

