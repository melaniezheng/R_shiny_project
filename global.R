library(data.table)
library(dplyr)
library(ggplot2)
library(googleVis)

var_option <- c("temperature", "windchill", "humidity", "pressure", "visibility", "windspeed", 
                "precipitation")


data <- fread(file = "~/NYCDSA/R_shiny_project/US_Accidents_May19.csv", stringsAsFactors = FALSE)
#population data per state
population <- fread(file= "~/NYCDSA/R_shiny_project/US_Population_2019_Jul.csv", header = TRUE, stringsAsFactors = FALSE)

colnames(population)=c("StateName","State","Population")
population <- population[-1,]
population$Population=as.integer(population$Population)
kPopulation <- sum(population$Population)

############ to delete ###########
# set humidity category 0-25, 25-50, 50-75, 75-100
#colnames(my_data)
#var1 <- my_data$temperature
#var2 <- my_data$humidity
#ggplot(my_data) + 
  #geom_bin2d(aes(var1,var2), na.rm = T)  +
  #scale_fill_gradient(low="#FCC8C5", high="#D10C00")

##################################
my_data <- data %>%  
  left_join(., population, by="State") %>% 
  mutate(.,year=substr(Start_Time,1,4), 
         month=factor(month.abb[as.integer(substr(Start_Time,6,7))], levels=month.abb, ordered = T)) %>% 
  mutate(., humidity2=case_when(`Humidity(%)`<25 ~ '<25%',
                               `Humidity(%)`>=25 & `Humidity(%)`<=50 ~ '25%-50%',
                               `Humidity(%)`>=50 & `Humidity(%)`<=75 ~ '50%-75%',
                               `Humidity(%)`>75 ~ '>75%')) %>% 
  rename(., day_night=Sunrise_Sunset, temperature=`Temperature(F)`, windchill=`Wind_Chill(F)`,
         pressure=`Pressure(in)`, visibility=`Visibility(mi)`, windspeed=`Wind_Speed(mph)`,
         precipitation=`Precipitation(in)`, humidity=`Humidity(%)`) %>% 
  select(., -Turning_Loop, -Civil_Twilight, -Nautical_Twilight, -Astronomical_Twilight, -Railway, -Roundabout, 
         -Station, -Stop, -Traffic_Calming, -Traffic_Signal, -No_Exit, -Junction, -Give_Way, -Crossing, -Bump, 
         -Amenity, -Timezone, -Airport_Code)

# data for nation-wide average
data_USA <- my_data %>% group_by(.,year,month,State) %>% summarise(.,count=n()) %>% inner_join(.,population,by="State") %>% 
  select(.,-StateName) %>% mutate(.,proportion=count/Population*100) %>% 
  group_by(year, month) %>% summarise(avg=mean(count), avg_prop=mean(proportion)) %>% 
  group_by(.,month) %>% summarise(.,Count=round(mean(avg)), Proportion=round(mean(avg_prop),3)) %>% 
  mutate(., type="USA") 

# SC_selected <- my_data %>% filter(., State=="SC") %>% 
#   group_by(.,year, month, State) %>% 
#   summarise(.,count=n())%>% inner_join(.,population,by="State") %>% select(.,-StateName) %>% 
#   mutate(.,proportion=count/Population*100) %>% 
#   group_by(., month) %>% summarise(.,Count=round(mean(count)), Proportion=round(mean(proportion),3)) %>% 
#   mutate(., type="SC") 

# new <- rbind(data_USA, SC_selected)

# ggplot(data=new, aes(month, proportion, fill=type))+
  # geom_col(position="dodge", width = 0.4) +
  # xlab("Month") +
  # ylab("")+
  # ggtitle("Number of Accidents")



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
  filter(., !is.na(precipitation)) %>% group_by(., Zipcode) %>% 
  summarise(., avg.precipitation=mean(precipitation), accident.count=n())

sample <- tail(my_data,10)
data_visibility <- my_data %>% group_by(.,visibility) %>% summarise(.,Count=n())

