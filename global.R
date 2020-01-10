library(data.table)
library(dplyr)
library(zoo)
data <- fread(file = "US_Accidents_May19.csv")
class(data)
colnames(data)
nrow(data)

tail(data,10)
data <- data %>%  mutate(.,year=format(as.Date(as.yearmon(substr(Weather_Timestamp,1,7), "%b-%y")), "%y")) %>% 
  mutate(.,month=format(as.Date(as.yearmon(substr(Weather_Timestamp,1,7), "%b-%y")), "%b"))
