library(data.table)
library(shiny)
library(shinydashboard)
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
kInsurance <- round(mean(insurance$Insurance))

# car accidents data
my_data <- fread(file = "US_Accidents.csv", stringsAsFactors = FALSE)
my_data <- my_data %>% mutate(.,month=factor(month.abb[as.integer(month)], levels=month.abb, ordered = T))

#colState <- map("state", fill=T, plot=F, region=states)  # for leaflet

data_USA <- my_data %>% group_by(.,year,month,State) %>% summarise(.,count=n()) %>% 
  inner_join(.,population_raw, by=c("year","State")) %>% 
  mutate(., proportion=round(count/Population*100,3)) %>% 
  group_by(.,year,month) %>% summarise(avg=mean(count),avg_prop=mean(proportion)) %>% 
  group_by(.,month) %>% summarise(.,Count=round(mean(avg)), Proportion=round(mean(avg_prop),3)) %>% 
  mutate(., type="USA")


  
# ggplot(my_data %>% gather(., key="key", value="value", c("humidity","temperature"))) + 
#   geom_freqpoly(aes(value, color=key), na.rm=T) +
#   facet_wrap(~key, scales = "free")+
#   labs(fill = "Weather Variables")+
#   theme(legend.position="right")

# ggplot(my_data %>% gather(., key="key", value="value", c("humidity","temperature"))) +
#   geom_histogram(aes(value), color="white", fill="#E85E5E",na.rm=T) +
#   facet_wrap(~key, scales = "free")+
#   labs(fill = "Weather Variables")+
#   theme(legend.position="right")

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


data_state_insurance2 <- data_state %>% left_join(.,insurance, by="StateName") %>%
  select(.,State,Count,proportion,Insurance) %>%
  rename(.,type=State)

df <- as.data.frame(rbind(as.data.frame(data_state_insurance2),insurance_USA)) %>%
                      mutate(.,n=1:50) %>% rename(., Accidents=proportion)

# daily <- as.data.frame(my_data %>% group_by(., Date, State) %>%
#                          summarise(.,Count=n()) %>% mutate(., Count=as.numeric(Count),
#                                                            Day=as.character(weekdays(as.Date(Date))),
#                                                            Is.Weekend=ifelse(Day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
#                                                                           "Weekday", "Weekend")))

# timeseries <- gvisAnnotationChart(daily %>% filter(., State %in% c("CA")),
#                     datevar = "Date", numvar="Count",# annotationvar = "Is.Weekend",#idvar="Day",
#                     options=list(displayAnnotations=TRUE,
#                                  legendPosition='newRow',
#                                  width=1150, height=400))
# plot(timeseries)
