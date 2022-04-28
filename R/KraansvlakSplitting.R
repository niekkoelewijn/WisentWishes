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
start <- as.numeric(as.POSIXct(strptime("2021-03-04 00:00:00", format = "%Y-%m-%d %H:%M:%S")))
end <- as.numeric(as.POSIXct(strptime("2021-04-12 00:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Filter out dates in which the management interventions took place
Track2Filtered <- Track2 %>% 
  atl_filter_covariates(filters = c(
    "!inrange(time_coded, start, end)"
  )
  )






































