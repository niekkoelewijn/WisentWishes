# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to get average CCI values per hour per season

## Read weather data per hour into R

# Path to hourly weather data
setwd("~/WisentWishes")
WeatherHourPath <- "~/WisentWishes/MScThesisData/EnvironmentalVariables/HourWeatherData/"
WeatherHourVec <- list.files(path = WeatherHourPath, pattern = ".csv")

# Load Hour CCI data KNMI
DeelenHour <- read_csv(paste0(WeatherHourPath, WeatherHourVec[1]))
deKooyHour <- read_csv(paste0(WeatherHourPath, WeatherHourVec[2]))
VolkelHour <- read_csv(paste0(WeatherHourPath, WeatherHourVec[3]))
WilhelminadorpHour <- read_csv(paste0(WeatherHourPath, WeatherHourVec[4]))

# General function to get average CCI for specified season and hour of day
getMeanCCI <- function(StudyArea, season, time){
  
  # Check whether the input for StudyArea is valid
  StudyArea <- match.arg(StudyArea, choices = c("Kraansvlak", "Maashorst2016", "Maashorst20172021",
                                                "Maashorst2022", "SlikkenvdHeenHabituate", "SlikkenvdHeen", 
                                                "VeluweHabituate", "Veluwe"))
  
  # Check whether the input for time is valid
  time <- match.arg(time, choices = c("00:00:00", "01:00:00", "02:00:00",
                                      "03:00:00", "04:00:00", "05:00:00", 
                                      "06:00:00", "07:00:00", "08:00:00", 
                                      "09:00:00", "10:00:00", "11:00:00", 
                                      "12:00:00", "13:00:00", "14:00:00", 
                                      "15:00:00", "16:00:00", "17:00:00", 
                                      "18:00:00", "19:00:00", "20:00:00", 
                                      "21:00:00", "22:00:00", "23:00:00"))
  
  # Get hourly weather data from specified Study area
  HourWeatherData <- switch(StudyArea,
                            Kraansvlak = deKooyHour,
                            Maashorst2016 = VolkelHour,
                            Maashorst20172021 = VolkelHour,
                            Maashorst2022 = VolkelHour,
                            SlikkenvdHeenHabituate = WilhelminadorpHour,
                            SlikkenvdHeen = WilhelminadorpHour,
                            VeluweHabituate = DeelenHour,
                            Veluwe = DeelenHour)
  
  # Get hour of the day from specified time
  hour <- switch(time,
                 "00:00:00" = 1, 
                 "01:00:00" = 2, 
                 "02:00:00" = 3,
                 "03:00:00" = 4, 
                 "04:00:00" = 5, 
                 "05:00:00" = 6, 
                 "06:00:00" = 7, 
                 "07:00:00" = 8, 
                 "08:00:00" = 9, 
                 "09:00:00" = 10, 
                 "10:00:00" = 11, 
                 "11:00:00" = 12, 
                 "12:00:00" = 13, 
                 "13:00:00" = 14, 
                 "14:00:00" = 15, 
                 "15:00:00" = 16, 
                 "16:00:00" = 17, 
                 "17:00:00" = 18, 
                 "18:00:00" = 19, 
                 "19:00:00" = 20, 
                 "20:00:00" = 21, 
                 "21:00:00" = 22, 
                 "22:00:00" = 23, 
                 "23:00:00" = 24)
  
  # Modify HourWeatherData to be able to select for season and hour
  HourWeatherData <- HourWeatherData %>% 
    
    # Reclassify YYYYMMDD to a character
    mutate(YYYYMMDD = as.character(YYYYMMDD)) %>% 
    
    # Get year from YYYYMMDD
    mutate(year = substr(YYYYMMDD, 1, 4)) %>% 
    
    # Get month from YYYYMMDD
    mutate(month = substr(YYYYMMDD, 5, 6)) %>% 
    
    # Get day from YYYYMMDD
    mutate(day = substr(YYYYMMDD, 7, 8)) %>% 
    
    # change date column classes to numeric
    mutate(year = as.numeric(year)) %>% 
    mutate(month = as.numeric(month)) %>% 
    mutate(day = as.numeric(day)) %>% 
    
    # Create date column
    mutate(date = make_date(year = year, month = month, day = day)) %>% 
    
    # Get date_season column
    mutate(date_season = time2season(date, out.fmt = "seasons")) %>% 
    
    # Select columns of interest: needed for CCI: T (Ta), FF (WS), U (RH), and Q (RAD)
    select(date, date_season, HH, "T", FF, U, Q) %>% 
    
    # Rename columns
    dplyr::rename(Ta = "T",
                  WS = FF,
                  RH = U,
                  RAD = Q ) %>% 
    
    # Convert values of columns
    mutate(Ta = Ta / 10 ,         # converted from 0.1 C to 1 C
           WS = WS / 10,          # converted from 0.1 m/s to 1 m/s
           RAD = RAD * 10000 / 86400)%>% # converted from J/cm2 to W/m2
    
    # Create CCI column
    mutate(CCI = CCI(Ta, RH, WS, RAD))
  
  # Now get CCI data for the specified time and season
  CCIdata <- HourWeatherData %>% 
    
    # Filter for specified season and hour
    dplyr::filter(date_season == season,
                  HH == hour) %>% 
    
    # Drop NA values for CCI
    drop_na()
  
  # Get mean CCI
  meanCCI <- mean(CCIdata$CCI)
  
  # Return meanCCI
  return(meanCCI)
}

SlikkenvdHeenSummer <- getMeanCCI("SlikkenvdHeen", "summer", "15:00:00")

SlikkenvdHeenWinter <- getMeanCCI("Veluwe", "winter", "05:00:00")













