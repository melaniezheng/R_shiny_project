library(data.table)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(googleVis)
library(maps)
library(leaflet)

var_option <- c("temperature","humidity","windchill","pressure","visibility","windspeed","precipitation")

#population data per state
population_raw <- fread(file="population_2016to2019.csv", stringsAsFactors = F, header = T)
population_raw <- population_raw %>% gather(., key="year", value="Population", 3:6) %>% 
  mutate(.,year=as.integer(year), Population= as.integer(gsub(",","",Population)))

# annual insurance premium per state
insurance <- fread(file="Car_Insurance_Cost_by_State.csv", header = T, stringsAsFactors = F)
colnames(insurance) <- c("StateName","Insurance")

# car accidents data
my_data <- fread(file = "US_Accidents.csv", stringsAsFactors = FALSE)
my_data <- my_data %>% mutate(.,month=factor(month.abb[as.integer(month)], levels=month.abb, ordered = T))

# accident in proportion to state population.
data_byStateYear <- my_data %>% 
  group_by(.,State, year) %>% 
  summarise(.,Count=n()) %>% 
  inner_join(.,population_raw,by=c("State","year")) %>% 
  mutate(.,proportion=round(Count/Population*1000,2)) 

data_state <- data_byStateYear %>% 
  group_by(.,State, StateName) %>% 
  summarise(.,Count=mean(Count), Population=mean(Population), proportion=mean(proportion))

# join with insurance data
df <- as.data.frame(data_state %>% 
                    left_join(.,insurance, by="StateName") %>%
                    select(.,State,Count,proportion,Insurance) %>%
                    rename(.,type=State)) %>% 
  mutate(.,n=1:49) %>% 
  rename(., Accidents=proportion)




