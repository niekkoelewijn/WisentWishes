# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to add additional date variables and weather statistics to the tracks
date <- as.Date(LanduseTracks$VeluweTrack9$time_of_fix_decoded, "%d/%m/%Y")
weekday <-  weekdays(LanduseTracks$VeluweTrack9$time, abbreviate = F)



my_seasons <- time2season(LanduseTracks$VeluweTrack9$time,                # Convert dates to seasons
                          out.fmt = "seasons")
my_seasons                                         # Print seasons
# [1] "autumm" "spring" "winter" "winter" "summer" "autumm"


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

# Create funcion
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
    
    # Add date attribute to elements of WeatherDataList
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
      mutate(date = make_date(year = year, month = month, day = day))
    
    if(i == 1){
      WeatherDataList[[i]] <- WeatherDataList[[i]] %>% 
        
        # Get only the rows corresponding to dates in the range of dates I
        # have GPS track points from 
        filter(date >= startdateVeluwe & date <= enddateVeluwe)
    }
    else if(i == 2){
      WeatherDataList[[i]] <- WeatherDataList[[i]] %>% 
        
        # Get only the rows corresponding to dates in the range of dates I
        # have GPS track points from 
        filter(date >= startdateKraansvlak & date <= enddateKraansvlak)
    }
    else if(i == 3){
      WeatherDataList[[i]] <- WeatherDataList[[i]] %>% 
        
        # Get only the rows corresponding to dates in the range of dates I
        # have GPS track points from 
        filter(date >= startdateMaashorst & date <= enddateMaashorst)
    }
    else if(i == 4){
      WeatherDataList[[i]] <- WeatherDataList[[i]] %>% 
        
        # Get only the rows corresponding to dates in the range of dates I
        # have GPS track points from 
        filter(date >= startdateSlikkenvdHeen & date <= enddateSlikkenvdHeen)
    }
  }
  
  
  ## Add time and weather attributes to GPSTrackList
  
  # Iterate over elements of GPSTrackList
  for(i in seq_along(GPSTrackList)){
    # The first 4 tracks are the Kraansvlak tracks
    if(i %in% 1:4){
      
      
      
    }
    # Track 5 to 15 are Maashorst tracks
    else if(i %in% c(5:15)){
      
    }
    # Track 16 - 19 are Slikken vd Heen tracks
    else if(i %in% c(16:22)){
      
      
      
    }
    # Track 23 - 27 are Veluwe tracks
    else if(i %in% c(23:47)){
      
      
      
    }
  }  
  
    
}

## Filter out dates from weather datasets that are out of the range of dates that 



