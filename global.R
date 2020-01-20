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

# pre-process data and save to .csv. original dataset is 800MB which is too large for shiny app.
# data <- fread(file = "~/NYCDSA/R_shiny_project/US_Accidents_Dec19.csv", stringsAsFactors = FALSE)

# my_data <- data %>%
#   mutate(.,year=substr(Start_Time,1,4),month=substr(Start_Time,6,7)) %>%
#   rename(., day_night=Sunrise_Sunset, temperature=`Temperature(F)`, windchill=`Wind_Chill(F)`,
#          pressure=`Pressure(in)`, visibility=`Visibility(mi)`, windspeed=`Wind_Speed(mph)`,
#          precipitation=`Precipitation(in)`, humidity=`Humidity(%)`) %>%
#   select(., -Turning_Loop, -Civil_Twilight, -Nautical_Twilight, -Astronomical_Twilight, -Railway, -Roundabout,
#          -Station, -Stop, -Traffic_Calming, -Traffic_Signal, -No_Exit, -Junction, -Give_Way, -Crossing, -Bump,
#          -Amenity, -Timezone, -Airport_Code, -Weather_Condition, -Wind_Direction, -Weather_Timestamp, -Street, -Side,
#          -Description, -Number, -End_Time, -Source, -ID,-End_Lat, -End_Lng, -`Distance(mi)`, -TMC, -Start_Time, -Zipcode,
#          -Country)

# write.csv(my_data, "~/NYCDSA/R_shiny_project/US_Accidents.csv", row.names = F)

#population data per state
population_raw <- fread(file="population_2016to2019.csv", stringsAsFactors = F, header = T)
population_raw <- population_raw %>% gather(., key="year", value="Population", 3:6) %>% 
  mutate(.,year=as.integer(year), Population= as.integer(gsub(",","",Population)))

# annual insurance premium per state
insurance <- fread(file="Car_Insurance_Cost_by_State.csv", header = T, stringsAsFactors = F)
colnames(insurance) <- c("StateName","Insurance")
kInsurance <- round(mean(insurance$Insurance))

# car accidents data
my_data <- fread(file = "US_Accidents.csv", stringsAsFactors = FALSE)
my_data <- my_data %>% mutate(.,month=factor(month.abb[as.integer(month)], levels=month.abb, ordered = T))

colState <- map("state", fill=T, plot=F,
                region=states)  # for leaflet

# data_byYearMon <- my_data %>% filter(., State=="AL") %>% 
#   group_by(., State, year, month) %>% summarise(., Count=n()) %>% 
#   left_join(., population, by="State") %>% 
#   mutate(., proportion=Count/Population)

data_USA <- my_data %>% group_by(.,year,month,State) %>% summarise(.,count=n()) %>% 
  inner_join(.,population,by="State") %>% select(.,-StateName) %>% 
  mutate(.,proportion=count/Population*100) %>% 
  group_by(year, month) %>% summarise(avg=mean(count), avg_prop=mean(proportion)) %>%
  group_by(.,month) %>% summarise(.,Count=round(mean(avg)), Proportion=round(mean(avg_prop),3)) %>%
  mutate(., type="USA")

k_proportion=mean(data_USA$Proportion)

insurance_USA <- data_USA %>% group_by(.,type) %>% summarise(.,Count=round(mean(Count)), proportion=mean(Proportion))
insurance_USA$Insurance=as.integer(kInsurance)

# accident in proportion to state population.
data_byStateYear <- my_data %>% 
  group_by(.,State, year) %>% summarise(.,Count=n()) %>% 
  inner_join(.,population_raw,by=c("State","year")) %>% 
  mutate(.,proportion=round(Count/Population*100,3)) 


data_state <- data_byStateYear %>% 
  group_by(.,State, StateName) %>% 
  summarise(.,Count=mean(Count), Population=mean(Population), proportion=mean(proportion))
states <- unique(data_state$State)

# df <- my_data %>%
#   filter(., State == "AL") %>%
#   group_by(.,as.character(my_data[,"humidity"])) %>%
#   summarise(., Count=n()) %>% 
#   `colnames<-`(c("humidity", "Count"))
# ggplot(df, aes_string(x="humidity",y="Count"))+geom_point(na.rm=T)

# data_state_insurance <- my_data %>% #filter(.,State=="AL") %>%
#   group_by(.,State,year) %>% summarise(.,count=n()) %>%
#   group_by(.,State) %>% summarise(.,Count=round(mean(count))) %>%
#   left_join(., population, by="State") %>%
#   mutate(.,proportion=round(Count/Population*100,3)) %>%
#   left_join(.,insurance, by="StateName") %>%
#   select(.,State,Count,proportion,Insurance) %>%
#   rename(., type=State)

# df <- as.data.frame(rbind(as.data.frame(data_state %>% select),insurance_USA) %>%
#                       mutate(.,n=1:50)) %>% rename(., Accidents=proportion)

data_state_insurance2 <- data_state %>% left_join(.,insurance, by="StateName") %>%
  select(.,State,Count,proportion,Insurance) %>%
  rename(.,type=State)

df <- as.data.frame(rbind(as.data.frame(data_state_insurance2),insurance_USA)) %>%
                      mutate(.,n=1:50) %>% rename(., Accidents=proportion)
