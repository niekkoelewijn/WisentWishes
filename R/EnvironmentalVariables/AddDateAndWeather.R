# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to add additional date variables and weather statistics to the tracks

## Load GPS tracks from previous step

# Paths to tracks step 2
setwd("~/WisentWishes")
EVStep2Path <- "~/WisentWishes/MScThesisData/GPS location data/Step2EV/"
EVStep2Vec <- list.files(path = EVStep2Path)

# Get names of tracks, without .csv
NameVec <- c()
for(i in seq_along(EVStep2Vec)){
  NameVec[i] <- tools::file_path_sans_ext(EVStep2Vec[i])
}

# Create list of files from preprocessed directory
DistanceTracks <- lst()
for(i in seq_along(EVStep2Vec)){
  DistanceTracks[[i]] <- read_csv(file = paste0(EVStep2Path, EVStep2Vec[i]))
}

# Add names to tracks
names(DistanceTracks) <- NameVec


## Read weather data into R

# Path to weather data
setwd("~/WisentWishes")
WeatherPath <- "~/WisentWishes/MScThesisData/EnvironmentalVariables/WeatherVariables/"
WeatherVec <- list.files(path = GPSInputPath, pattern = ".csv")

# Load weather data as tibbles
Deelen <- as_tibble(read.csv(paste0(WeatherPath, WeatherVec[1])))
deKooy <- as_tibble(read.csv(paste0(WeatherPath, WeatherVec[2])))
Volkel <- as_tibble(read.csv(paste0(WeatherPath, WeatherVec[3])))
Wilhelminadorp <- as_tibble(read.csv(paste0(WeatherPath, WeatherVec[4])))

WeatherDataList <- list(Deelen, deKooy, Volkel, Wilhelminadorp)
names(WeatherDataList) <- c("Deelen", "deKooy", "Volkel", "Wilhelminadorp")

## Create general function to add time and weather variables to the tracks

# Create function to calculate comprehensive climate index (CCI)
CCI <- function(Ta, RH, WS, RAD){
  
  ### Function to calculate the CCI, which is a relative indicator of the
  ### environmental conditions surrounding an animal. The CCI is the adjusted
  ### temperature (ºC) taken into account Ta (ambient temperature in C), 
  ### RH (relative humidity in %), WS (wind speed in m/s) and RAD (solar 
  ### radiation in W/m^2)
  
  ## CCI is calculated as Ta + Eq. 1 + Eq. 2 + Eq. 3
  
  # Eq. 1: RH correction factor
  Eq1 <- exp(0.00182 * RH + 1.8 * 10^-5 * Ta * RH) * (0.000054 * Ta^2 + 0.00192 * Ta - 0.0246) * (RH - 30)
  
  # Eq. 2: WS correction factor
  Eq2 <- (-6.56 / (exp(1 / (2.26 * WS + 0.23)^(0.45 * (2.9 + 1.14 * 10^-6 * WS^2.5 - log(2.26 * WS + 0.33, base = 0.3)^-2))))) - 0.00566 * WS^2 + 3.33
  
  # Eq. 3: RAD correction factor
  Eq3 <- 0.0076 * RAD - 0.00002 * RAD * Ta + 0.00005 * Ta^2 * sqrt(RAD) + 0.1 * Ta - 2
  
  # Calculate CCI
  CCI <- Ta + Eq1 + Eq2 + Eq3
  
  # Return CCI
  return(CCI)
}

# Create funcion to add time and weather variables
AddTimeWeather <- function(GPSTrackList, WeatherDataList, NameVec){
  
  ## Get first and last time element of each study area
  
  # Kraansvlak
  startKraansvlak <- GPSTrackList$KraansvlakTrack1$time[1]
  endKraansvlak <- GPSTrackList$KraansvlakTrack4$time[length(GPSTrackList$KraansvlakTrack4$time)]
  
  startdateKraansvlak <- make_date(year = substr(startKraansvlak, 1, 4),
                                   month = substr(startKraansvlak, 6, 7),
                                   day = substr(startKraansvlak, 9, 10))
  enddateKraansvlak <- make_date(year = substr(endKraansvlak, 1, 4),
                            month = substr(endKraansvlak, 6, 7),
                            day = substr(endKraansvlak, 9, 10))
  
  # Maashorst
  startMaashorst <- GPSTrackList$MaashorstTrack1$time[1]
  endMaashorst <- GPSTrackList$MaashorstTrack11$time[length(GPSTrackList$MaashorstTrack11$time)]
  
  startdateMaashorst <- make_date(year = substr(startMaashorst, 1, 4),
                                   month = substr(startMaashorst, 6, 7),
                                   day = substr(startMaashorst, 9, 10))
  enddateMaashorst <- make_date(year = substr(endMaashorst, 1, 4),
                             month = substr(endMaashorst, 6, 7),
                             day = substr(endMaashorst, 9, 10))
  
  # Slikken vd Heen
  startSlikkenvdHeen <- GPSTrackList$SlikkenvdHeenHabTrack1$time[1]
  endSlikkenvdHeen <- GPSTrackList$SlikkenvdHeenTrack3$time[length(GPSTrackList$SlikkenvdHeenTrack3$time)]
  
  startdateSlikkenvdHeen <- make_date(year = substr(startSlikkenvdHeen, 1, 4),
                                  month = substr(startSlikkenvdHeen, 6, 7),
                                  day = substr(startSlikkenvdHeen, 9, 10))
  enddateSlikkenvdHeen <- make_date(year = substr(endSlikkenvdHeen, 1, 4),
                                month = substr(endSlikkenvdHeen, 6, 7),
                                day = substr(endSlikkenvdHeen, 9, 10))
  
  # Veluwe
  startVeluwe <- GPSTrackList$VeluweHabTrack1$time[1]
  endVeluwe <- GPSTrackList$VeluweTrack15$time[length(GPSTrackList$VeluweTrack15$time)]
  
  startdateVeluwe <- make_date(year = substr(startVeluwe, 1, 4),
                                      month = substr(startVeluwe, 6, 7),
                                      day = substr(startVeluwe, 9, 10))
  enddateVeluwe <- make_date(year = substr(endVeluwe, 1, 4),
                                    month = substr(endVeluwe, 6, 7),
                                    day = substr(endVeluwe, 9, 10))
  
  
  
  ## Filter elements of WeatherDataList to start and end date of the different study areas
  
  # Iterate over elements of WeatherDataList
  for(i in seq_along(WeatherDataList)){
    
    # Add date attribute to elements of WeatherDataList, and carry out some 
    # additional data preprocessing
    WeatherDataList[[i]] <- WeatherDataList[[i]] %>% 
      
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
      
      # Select columns of interest
      select(date, FG, TG, SQ, Q, UG, DR, RH) %>% 
      
      # Convert values of columns
      mutate(FG = FG / 10 ,         # converted from 0.1 m/s to 1 m/s
             TG = TG / 10,          # converted from 0.1 C to 1 C
             SQ = SQ / 10,          # converted from 0.1 hour to 1 hour
             Q = Q * 10000 / 86400, # converted from J/cm2 to W/m2
                                    # UG is in percent, that how I want it
             DR = DR / 10,          # converted from 0.1 hour to 1 hour
             RH = RH / 10)%>%       # converted from 0.1 mm to 1 mm 
      
      # Replace values total_precipitation_day
      # if RH < 0.05 mm, a value of -1 was given by the KNMI, I will replace
      # that with a 0, as -1 mm precipition does not make sense
      mutate(RH = replace(RH, RH == -0.1, 0)) %>% 
      
      # Rename columns
      dplyr::rename(average_windspeed_day = FG,
             average_temperature_day = TG,
             sunshine_duration_day = SQ,
             solar_radiation = Q,
             average_relative_humidity = UG,
             precipitation_duration_day = DR,
             total_precipitation_day = RH)
      
      
      
    if(i == 1){
      WeatherDataList[[i]] <- WeatherDataList[[i]] %>% 
        
        # Get only the rows corresponding to dates in the range of dates I
        # have GPS track points from 
        filter(date >= startdateVeluwe & date <= enddateVeluwe) %>% 
        
        # Create CCI column in the WeatherDataList
        mutate(CCI = CCI(average_temperature_day, average_relative_humidity, 
                          average_windspeed_day, solar_radiation))
    }
    else if(i == 2){
      WeatherDataList[[i]] <- WeatherDataList[[i]] %>% 
        
        # Get only the rows corresponding to dates in the range of dates I
        # have GPS track points from 
        filter(date >= startdateKraansvlak & date <= enddateKraansvlak) %>% 
        
        # Create CCI column in the WeatherDataList
        mutate(CCI = CCI(average_temperature_day, average_relative_humidity, 
                          average_windspeed_day, solar_radiation))
      
    }
    else if(i == 3){
      WeatherDataList[[i]] <- WeatherDataList[[i]] %>% 
        
        # Get only the rows corresponding to dates in the range of dates I
        # have GPS track points from 
        filter(date >= startdateMaashorst & date <= enddateMaashorst) %>% 
        
        # Create CCI column in the WeatherDataList
        mutate(CCI = CCI(average_temperature_day, average_relative_humidity, 
                          average_windspeed_day, solar_radiation))
    }
    else if(i == 4){
      WeatherDataList[[i]] <- WeatherDataList[[i]] %>% 
        
        # Get only the rows corresponding to dates in the range of dates I
        # have GPS track points from 
        filter(date >= startdateSlikkenvdHeen & date <= enddateSlikkenvdHeen) %>% 
        
        # Create CCI column in the WeatherDataList
        mutate(CCI = CCI(average_temperature_day, average_relative_humidity, 
                          average_windspeed_day, solar_radiation))
    }
  }
  
  # Name WeatherDataList
  names(WeatherDataList) <- c("Deelen", "deKooy", "Volkel", "Wilhelminadorp")
  
  
  ## Add time attributes to GPSTrackList
  
  # Iterate over elements of GPSTrackList
  for(i in seq_along(GPSTrackList)){
    
    # Add time attributes to elements of GPSTrackList
    GPSTrackList[[i]] <- GPSTrackList[[i]] %>% 
      
      # Add date
      mutate(date = make_date(year = substr(time, 1, 4),
                               month = substr(time, 6, 7),
                               day = substr(time, 9, 10))) %>% 
      
      # Add time
      mutate(hms = format(time, "%H:%M:%S")) %>% 
      
      # Add weekday
      mutate(weekday = weekdays(time, abbreviate = F)) %>% 
      
      # Add weekend / business day
      mutate(day_type = ifelse(weekday %in% c("Sunday", "Saturday"), "weekend", "business day")) %>% 
      
      # Add season
      mutate(season = time2season(time, out.fmt = "seasons"))
    
    # For sunrise / sunset calculations, I need coordinates in lat / lon
    LatLonTable <- GPSTrackList[[i]] %>% 
      
      # Transform to class sf
      st_as_sf(coords = c("X", "Y"), crs = st_crs(28992)) %>% 
      
      # Transfrom WGS84 to RD new
      st_transform(crs = st_crs(4326))
    
    # Get x and y column
    Lon <- st_coordinates(LatLonTable)[,1]
    Lat <- st_coordinates(LatLonTable)[,2]
    
    # Calculate time of sunrise and sunset
    sunrise_set <- suncalc::getSunlightTimes(data = data.frame(date = GPSTrackList[[i]]$date, lat = Lat,
                                                               lon = Lon),
                                             keep = c("sunrise", "sunset"),
                                             tz = Sys.timezone())
    
    # Determine for each time poinnt of the element of GPSTrackList wheater it is at day or night
    day_night_tibble <- ifelse(GPSTrackList[[i]]$time >= sunrise_set$sunrise & 
                        GPSTrackList[[i]]$time <= sunrise_set$sunset,
                        "day", "night")
    
    # Create tibble with same ID values as the points of GPSTrackList element
    day_night_tibble <-day_night_tibble %>% 
      as_tibble() %>% 
      rowid_to_column("ID") %>% 
      dplyr::rename(day_night = value)
    
    # Add time attributes to elements of GPSTrackList
    GPSTrackList[[i]] <- GPSTrackList[[i]] %>% 
      
      # Add day / night
      inner_join(day_night_tibble, by = "ID")
    
    ## Add weather attributes to GPSTrackList
    
    # The first 4 tracks are the Kraansvlak tracks
    if(i %in% 1:4){
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>% 
        inner_join(WeatherDataList$deKooy, by = "date")
      
    }
    # Track 5 to 15 are Maashorst tracks
    else if(i %in% c(5:15)){
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>% 
        inner_join(WeatherDataList$Volkel, by = "date")
    }
    # Track 16 - 19 are Slikken vd Heen tracks
    else if(i %in% c(16:22)){
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>% 
        inner_join(WeatherDataList$Wilhelminadorp, by = "date")
    }
    # Track 23 - 27 are Veluwe tracks
    else if(i %in% c(23:47)){
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>% 
        inner_join(WeatherDataList$Deelen, by = "date")
    }
  }
  
  # Name elements from list
  names(GPSTrackList) <- NameVec
  
  # Return GPSTrackList
  return(GPSTrackList)
}

# Call AddTimeWeather
WeatherTracks <- AddTimeWeather(DistanceTracks, WeatherDataList, NameVec)

## Write output to file

# Create path
path <- "~/WisentWishes/MScThesisData/GPS location data/Step3EV/"

# Create directory
if(!dir.exists(path)){
  dir.create(path)
}

# Write elements of Landuse Tracks list to file
for(i in seq_along(names(WeatherTracks))){
  write_csv(WeatherTracks[[i]], file = paste0(path, names(WeatherTracks)[i], ".csv"))
} 






