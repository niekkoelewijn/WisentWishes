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
GPSStep4Path <- "~/WisentWishes/MScThesisData/GPS location data/Step4Preprocess/"
GPSStep4Vec <- list.files(path = GPSStep4Path, pattern = regex("^Kraansvlak."))








































