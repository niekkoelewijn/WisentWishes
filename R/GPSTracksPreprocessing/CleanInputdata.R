# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

# Install required packages
if(!"rgdal" %in% rownames(installed.packages())){install.packages("rgdal")}
if(!"rgeos" %in% rownames(installed.packages())){install.packages("rgeos")}
if(!"sf" %in% rownames(installed.packages())){install.packages("sf")}
if(!"raster" %in% rownames(installed.packages())){install.packages("raster")}
if(!"atlastools" %in% rownames(installed.packages())){install.packages("atlastools")}
if(!"tidyverse" %in% rownames(installed.packages())){install.packages("tidyverse")}
if(!"lubridate" %in% rownames(installed.packages())){install.packages("lubridate")}
if(!"momentuHMM" %in% rownames(installed.packages())){install.packages("momentuHMM")}
if(!"tools" %in% rownames(installed.packages())){install.packages("tools")}
if(!"hydroTSM" %in% rownames(installed.packages())){install.packages("hydroTSM")}
if(!"photobiology" %in% rownames(installed.packages())){install.packages("photobiology")}
if(!"suncalc" %in% rownames(installed.packages())){install.packages("suncalc")}
if(!"ggplot2" %in% rownames(installed.packages())){install.packages("ggplot2")}
if(!"reshape" %in% rownames(installed.packages())){install.packages("reshape")}
if(!"gridExtra" %in% rownames(installed.packages())){install.packages("gridExtra")}

# Load required packages
library(rgdal)
library(rgeos)
library(sf)
library(raster)
library(atlastools)
library(tidyverse)
library(lubridate)
library(momentuHMM)
library(tools)
library(hydroTSM)
library(photobiology)
library(suncalc)
library(ggplot2)
library(reshape)
library(gridExtra)

### Clean GPS-input data

## Load GPS input data as tibble

# Paths to GPS input files
setwd("~/WisentWishes")
GPSInputPath <- "~/WisentWishes/MScThesisData/GPS location data/RawInput/"
GPSInputPathVec <- list.files(path = GPSInputPath)

# Load input tables as tibbles
EverestRaw <- read_csv(paste0(GPSInputPath, GPSInputPathVec[1]))
CaliopeRaw <- read_csv(paste0(GPSInputPath, GPSInputPathVec[2]))
DeliaRaw <- read_csv(paste0(GPSInputPath, GPSInputPathVec[3]))
KraansvlakRaw <- read_csv(paste0(GPSInputPath, GPSInputPathVec[4]))
KraylaRaw <- read_csv(paste0(GPSInputPath, GPSInputPathVec[5]))
KroosjaRaw <- read_csv(paste0(GPSInputPath, GPSInputPathVec[6]))
MaaikeRaw <- read_csv(paste0(GPSInputPath, GPSInputPathVec[7]))
NadiaRaw <- read_csv(paste0(GPSInputPath, GPSInputPathVec[8]))
NevayaRaw <- read_csv(paste0(GPSInputPath, GPSInputPathVec[9]))
SharaRaw <- read_csv(paste0(GPSInputPath, GPSInputPathVec[10]))
VeluweRaw <- read_csv(paste0(GPSInputPath, GPSInputPathVec[11]))

## Get odd input tables in same format

# Most input data comes from GPS trackers from the brand Followit
# This Followit data has the columns time, device_id, type, lat, lgn
# fix, temp and hdop. The tables KraansvlakRaw, NevayaRaw, SharaRaw
# and VeluweRaw have other orders of columns. Here, the column names and orders
# are standerdized.

# Kraansvlak
KraansvlakRaw <- KraansvlakRaw %>% 
  
  # Temperature data is delivered in port 12 messages, but correct location data
  # of the bison is delivered in port 1 messages. With different layers of mutatue
  # steps, I couple the port 1 messages to the correct port 12 temperature attribute.
  
  mutate(
    temp = if_else(temperature == "\\N", lag(temperature), temperature) 
    ) %>% 
  mutate(
      temp = if_else(temp == "\\N", lag(temp), temp) 
    ) %>%
  mutate(
    temp = if_else(temp == "\\N", lag(temp), temp) 
  ) %>% 
  mutate(
    temp = if_else(temp == "\\N", lag(temp), temp) 
  ) %>%
  mutate(
    temp = if_else(temp == "\\N", lag(temp), temp) 
  ) %>%
  
  # Get temp as numeric values
  mutate(temp = as.numeric(temp)) %>% 
  
  # Get only port 1 messages
  filter(port == 1) %>% 
  
  # Remove na values
  drop_na() %>% 
  
  # Select columns of interest
  select(timestamp, latitude, longitude, temp, hdop) %>%
  
  # Rename columns
  dplyr::rename(lat = latitude) %>% 
  dplyr::rename(lng = longitude) %>% 
  dplyr::rename(time = timestamp)


# Nevaya
NevayaRaw <- NevayaRaw %>% 
  
  # Select columns of interest
  select(time, lat, lng, temp, hdop) %>% 
  
  # Select only does rows with a temperature attribute
  drop_na(temp)


# Shara
SharaRaw <- SharaRaw %>% 
  
  # Shara has no temp attribute. I will assign the average temperature of the 
  # Veluwe area to the gps fixes for convenience
  mutate(temp = 10.1) %>%
  
  # Select columns of interest
  select(time, lat, lng, temp, hdop) %>% 

  # Drop NA's
  drop_na()
  
  
# Veluwe
VeluweRaw <- VeluweRaw %>% 
  
  # The timestemp of Veluwe the Date and time column, the content needs to be 
  # seperated
  
  # Seperate Date
  separate(Date, sep = "/", into = c("month", "day", "year")) %>% 
  
  # Seperate Time
  separate(Time, sep = ":", into = c("hour", "minute", "second")) %>%
  
  # change date/time column classes to numeric
  mutate(month = as.numeric(month)) %>% 
  mutate(day = as.numeric(day)) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(hour = as.numeric(hour)) %>% 
  mutate(minute = as.numeric(minute)) %>% 
  mutate(second = as.numeric(second)) %>% 
  
  # Now merch them all in a dttm column
  mutate(time = make_datetime(year, month, day, hour, minute, second)) %>%  
  
  # Rename columns
  dplyr::rename(hdop = H.DOP) %>% 
  dplyr::rename(temp = Temp.C.) %>% 
  dplyr::rename(lat = Latitude) %>% 
  dplyr::rename(lng = Longitude) %>%
  
  # Select columns of interest
  select(time, lat, lng, temp, hdop)


# Now all datasets have the same, I can generalize the next preprocessing steps
# in a function called General Cleaning
GeneralCleaning <- function(GPSfile){
  
  # Edit the input GPS file in a piped structure
  ProcessedGPSfile <- GPSfile %>% 
    
    # Remove 0 values for lat or lng out of the dataset
    filter(lat != 0 | lng != 0) %>% 
    
    # Remove NA values
    drop_na() %>% 
    
    # Select columns of interest 
    select(time, lat, lng, temp, hdop) %>% 
    
    # Remove rows with hdop values greater than or equal to 10
    filter(hdop < 10) %>%
    
    # Get column with time since unix epoch
    mutate(time_coded = as.integer(time))
  
  return(ProcessedGPSfile)
}

# Now get the 11 datasets in a list
RawGPSdataList <- list(EverestRaw, CaliopeRaw, DeliaRaw, KraansvlakRaw, 
                       KraylaRaw, KroosjaRaw, MaaikeRaw, NadiaRaw, NevayaRaw,
                       SharaRaw, VeluweRaw)

# And apply the General Cleaning function on each element of the list 
Step1ProprocessingList <- lapply(RawGPSdataList, GeneralCleaning)


## Write the elements of the list to a file

# Create path
path <- "MScThesisData/GPS location data/Step1Preprocess/"

# Create directory
if(!dir.exists(path)){
  dir.create(path)
}

# Create vector of file names
NameVec <- c("EverestStep1", "CaliopeStep1", "DeliaStep1", "KraansvlakStep1", 
                     "KraylaStep1", "KroosjaStep1", "MaaikeStep1", "NadiaStep1", "NevayaStep1",
                     "SharaStep1", "VeluweStep1")

# Write csv's
for(i in seq_along(Step1ProprocessingList)){
  write_csv(Step1ProprocessingList[[i]], file = paste0(path, NameVec[i], ".csv"))
} 





