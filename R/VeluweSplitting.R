# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to select and split the GPS tracks of the Veluwe based on the 
### activity logbook

setwd("~/WisentWishes")
GPSStep4bPath <- "~/WisentWishes/MScThesisData/GPS location data/Step4Preprocess/"
GPSStep4bVec <- list.files(path = GPSStep4Path, pattern = regex("^Veluwe"))
