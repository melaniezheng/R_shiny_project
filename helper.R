# pre-process data and save to .csv. original dataset is 1GB which is too large for shiny app.
data <- fread(file = "US_Accidents_Dec19.csv", stringsAsFactors = FALSE)

my_data <- data %>%
  mutate(.,year=substr(Start_Time,1,4),month=substr(Start_Time,6,7), Date=substr(Start_Time,1,10)) %>%
  filter(., year > "2016") %>% 
  rename(., day_night=Sunrise_Sunset, temperature=`Temperature(F)`, windchill=`Wind_Chill(F)`,
         pressure=`Pressure(in)`, visibility=`Visibility(mi)`, windspeed=`Wind_Speed(mph)`,
         precipitation=`Precipitation(in)`, humidity=`Humidity(%)`) %>%
  select(., -Turning_Loop, -Civil_Twilight, -Nautical_Twilight, -Astronomical_Twilight, -Railway, -Roundabout,
         -Station, -Stop, -Traffic_Calming, -Traffic_Signal, -No_Exit, -Junction, -Give_Way, -Crossing, -Bump,
         -Amenity, -Timezone, -Airport_Code, -Weather_Condition, -Wind_Direction, -Weather_Timestamp, -Street, -Side,
         -Description, -Number, -End_Time, -Source, -ID,-End_Lat, -End_Lng, -`Distance(mi)`, -TMC, -Start_Time, -Zipcode,
         -Country)

write.csv(my_data, "~/NYCDSA/R_shiny_project/US_Accidents.csv", row.names = F)



