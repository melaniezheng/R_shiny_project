library(data.table)
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(googleVis)
library(maps)
library(leaflet)


var_option <- c("temperature", "windchill", "humidity", "pressure", "visibility", "windspeed", 
                "precipitation")


#data <- fread(file = "~/NYCDSA/R_shiny_project/US_Accidents_May19.csv", stringsAsFactors = FALSE)
#population data per state
population <- fread(file= "US_Population_2019_Jul.csv", header = TRUE, stringsAsFactors = FALSE)
counties <- fread(file="uscounties.csv", header = T, stringsAsFactors = F)
#geocode <- fread(file="Geocodes_USA_with_Counties.csv", header = T, stringsAsFactors = F)
colnames(population)=c("StateName","State","Population")
population <- population[-1,]
population$Population=as.integer(population$Population)
# kPopulation <- sum(population$Population)


# my_data <- data %>%
#   #left_join(., population, by="State") %>%
#   mutate(.,year=substr(Start_Time,1,4),
#          month=factor(month.abb[as.integer(substr(Start_Time,6,7))], levels=month.abb, ordered = T)) %>%
#   rename(., day_night=Sunrise_Sunset, temperature=`Temperature(F)`, windchill=`Wind_Chill(F)`,
#          pressure=`Pressure(in)`, visibility=`Visibility(mi)`, windspeed=`Wind_Speed(mph)`,
#          precipitation=`Precipitation(in)`, humidity=`Humidity(%)`) %>%
#   select(., -Turning_Loop, -Civil_Twilight, -Nautical_Twilight, -Astronomical_Twilight, -Railway, -Roundabout,
#          -Station, -Stop, -Traffic_Calming, -Traffic_Signal, -No_Exit, -Junction, -Give_Way, -Crossing, -Bump,
#          -Amenity, -Timezone, -Airport_Code, -Weather_Condition, -Wind_Direction, -Weather_Timestamp, -Street, -Side,
#          -Description, -Number, -End_Time, -Source, -ID,-End_Lat, -End_Lng, -`Distance(mi)`, -TMC, -Start_Time, -Zipcode,
#          -Country)

# write.csv(my_data, "~/NYCDSA/R_shiny_project/US_Accidents.csv", row.names = F)

# data for nation-wide average
my_data <- fread(file = "~/NYCDSA/R_shiny_project/US_Accidents.csv", stringsAsFactors = FALSE)
states <- unique(my_data$State)
colState <- map("state", fill=T, plot=F,
                region=states)  # for leaflet

data_USA <- my_data %>% group_by(.,year,month,State) %>% summarise(.,count=n()) %>% inner_join(.,population,by="State") %>%
  select(.,-StateName) %>% mutate(.,proportion=count/Population*100) %>%
  group_by(year, month) %>% summarise(avg=mean(count), avg_prop=mean(proportion)) %>%
  group_by(.,month) %>% summarise(.,Count=round(mean(avg)), Proportion=round(mean(avg_prop),3)) %>%
  mutate(., type="USA")


# monthly average accident count and average accident % in proportion to the population (ideally 
# in proportion to the number of drivers would make much more sense)
# data1 <- my_data %>% 
#   group_by(.,year, month) %>% summarise(., count.each.month=n()) %>% 
#   group_by(.,month) %>% summarise(., count.avg.month=round(mean(count.each.month))) %>% 
#   mutate(., proportion = count.avg.month/kPopulation)

# accident count per state.
data2 <- my_data %>% 
  group_by(.,State) %>% summarise(.,Count=n()) 

#accident count per city.
data_city <- my_data %>% 
  group_by(., State, City) %>% summarise(.,Count=n())

# accident in proportion to state population.
data_2 <- my_data %>% 
  group_by(.,State) %>% summarise(.,Count=n()) %>% 
  left_join(.,population,by="State") %>% 
  mutate(.,proportion=round(Count/Population*100,3)) 

# data_County <- my_data %>% filter(.,State== "CA") %>% group_by(., State,County) %>% 
#   summarise(.,Count=n()) %>% 
#   left_join(.,population,by="State") %>% 
#   mutate(.,Proportion=round(Count/Population*100,3))


# data_County_geocode <- left_join(data_County, geocode %>% group_by(.,county) %>% summarise(.,lat=median(latitude),long=median(longitude)) 
#           %>% rename(., County=county), by="County")


# data_byZipcode <- my_data %>% 
#   filter(., !is.na(precipitation)) %>% group_by(., Zipcode) %>% 
#   summarise(., avg.precipitation=mean(precipitation), accident.count=n())

# data_visibility <- my_data %>% group_by(.,visibility) %>% summarise(.,Count=n())

