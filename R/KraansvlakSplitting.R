# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to select and split the GPS tracks of the Kraansvlak based on the 
### activity logbook

## Get the Kraansvlak GPS data from step 4

# Paths to Kraansvlak tracks
setwd("~/WisentWishes")
GPSStep4aPath <- "~/WisentWishes/MScThesisData/GPS location data/Step4Preprocess/"
GPSStep4aVec <- list.files(path = GPSStep4Path, pattern = regex("^Kraansvlak."))

# Load GPS tracks of the Kraansvlak as tibbles
Track1 <- read_csv(paste0(GPSStep4aPath, GPSStep4aVec[1]))
Track2 <- read_csv(paste0(GPSStep4aPath, GPSStep4aVec[2]))
Track3 <- read_csv(paste0(GPSStep4aPath, GPSStep4aVec[3]))

### Judging from the logbook of intervention actions, no specific filitering needs
### to be undertaken with the first track of the Kraansvlak. In track 2, management
### interventions such as luring, feeding and confining between 4th of March 2021 
### and 12th of April heavily influenced the habitat selection of the European bison.
### In track 3, these management interventions took place between 15th of Februari 
### and 30th of March 2022.

## Filtering of track 2

# Get numeric start and end date of management intervention
start <- as.numeric(as.POSIXct(strptime("2021-03-04 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
end <- as.numeric(as.POSIXct(strptime("2021-04-12 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Pipe in which the following steps are taken
# 1: Filter out dates in which the management interventions took place
# 2: Recalculate the speed, angle and time interval attributes
# 3: Split the into track separate tracks when the time interval exceeds 10 hours
Track2Filtered <- Track2 %>% 
  atl_filter_covariates(filters = c(
    "!inrange(time_coded, start, end)"
  )
  ) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()


## Filtering of track 3

# Get numeric start and end date of management intervention
start <- as.numeric(as.POSIXct(strptime("2022-02-15 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
end <- as.numeric(as.POSIXct(strptime("2022-03-30 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Pipe in which the following steps are taken
# 1: Filter out dates in which the management interventions took place
# 2: Recalculate the speed, angle and time interval attributes
# 3: Split the into track separate tracks when the time interval exceeds 10 hours
Track3Filtered <- Track3 %>% 
  atl_filter_covariates(filters = c(
    "!inrange(time_coded, start, end)"
  )
  ) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()
  

## Create list of filtered Kraansvlak tracks
KraansvlakFilteredTracks <- lst()
KraansvlakFilteredTracks[[1]] <- Track1
KraansvlakFilteredTracks <- append(KraansvlakFilteredTracks, Track2Filtered)
KraansvlakFilteredTracks <- append(KraansvlakFilteredTracks, Track3Filtered)

## Write the elements of the lists to files

# Create path
path <- "~/WisentWishes/MScThesisData/GPS location data/Step5Preprocess/"

# Create directory
if(!dir.exists(path)){
  dir.create(path)
}

# Write elements
for(i in seq_along(KraansvlakFilteredTracks)){
  write_csv(KraansvlakFilteredTracks[[i]], file = paste0(path, "KraansvlakTracks", as.character(i), ".csv"))
} 



































