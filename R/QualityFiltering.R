# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

# Quality filtering of the GPS datasets

# Function to filter out rows with high HDOP values
QualityFilter <- function(GPSdata, value = 10, path){
  QualityFiltered <- GPSdata[as.numeric(GPSdata$hdop) < value, ]
  write.csv(QualityFiltered, file = paste0(path, "_qualityfiltered.csv"))
}

# Load spatially corrected GPS data

# Veluwe
path = "~/WisentWishes/MScThesisData/GPS location data/Veluwe/Preprocessed/"
SharaVeluwe20162020SC <- read.csv(paste0(path, "SharaVeluwe20162020_spatialfiltered.csv"))
names(SharaVeluwe20162020SC)[names(SharaVeluwe20162020SC) == 'H.DOP'] <- "hdop"
SharaVeluwe2022SC <- read.csv(paste0(path, "SharaVeluwe2022_spatialfiltered.csv"))
# SharaVeluwe2022SC does not have a hdop attribute, so I will not further analyze this dataset

# Maashorst
path = "~/WisentWishes/MScThesisData/GPS location data/Maashorst/Preprocessed/"
DeliaMaashorst2016SC <- read.csv(paste0(path, "DeliaMaashorst2016_spatialfiltered.csv"))
KraylaMaashorst2016SC <- read.csv(paste0(path, "KraylaMaashorst2016_spatialfiltered.csv"))
KroosjaMaashorst20162018SC <- read.csv(paste0(path, "KroosjaMaashorst20162018_spatialfiltered.csv"))
MaaikeMaashorst20192022SC <- read.csv(paste0(path, "MaaikeMaashorst20192022_spatialfiltered.csv"))
NevayaMaashorst2022SC <- read.csv(paste0(path, "NevayaMaashorst2022_spatialfiltered.csv"))
BullEverestMaashorst2022SC <- read.csv(paste0(path, "BullEverestMaashorst2022_spatialfiltered.csv"))
# NevayaMaashorst2022SC does not have a hdop attribute, so I will not further analyze this dataset

# Slikken vd Heen
path = "~/WisentWishes/MScThesisData/GPS location data/SlikkenvdHeen/Preprocessed/"
CaliopeSvdH20202021SC <- read.csv(paste0(path, "CaliopeSvdH20202021_spatialfiltered.csv"))
NadiaSvdH20202022SC <- read.csv(paste0(path, "NadiaSvdH20202022_spatialfiltered.csv"))

# Kraansvlak
path = "~/WisentWishes/MScThesisData/GPS location data/Kraansvlak/Preprocessed/"
Kraansvlak20202022SC <- read.csv(paste0(path, "Kraansvlak20202022_spatialfiltered.csv"))


## Quality filtering the spatially filtered datasets

# Make a list of all datasets
TrackList <- list(SharaVeluwe20162020SC, DeliaMaashorst2016SC, KraylaMaashorst2016SC, 
                  KroosjaMaashorst20162018SC, MaaikeMaashorst20192022SC,
                  BullEverestMaashorst2022SC, CaliopeSvdH20202021SC, NadiaSvdH20202022SC, 
                  Kraansvlak20202022SC)

# Make vector of paths
firstpart <- "~/WisentWishes/MScThesisData/GPS location data"
VeluwePath <- "/Veluwe/Preprocessed/"
MaashorstPath <- "/Maashorst/Preprocessed/"
SlikkenvdHeenPath <- "/SlikkenvdHeen/Preprocessed/"
KraansvlakPath <- "/Kraansvlak/Preprocessed/"
PathVec <- c(paste0(firstpart, VeluwePath, "SharaVeluwe20162020"), paste0(firstpart, MaashorstPath, "DeliaMaashorst2016"), 
             paste0(firstpart, MaashorstPath, "KraylaMaashorst2016"), paste0(firstpart, MaashorstPath, "KroosjaMaashorst20162018"), 
             paste0(firstpart, MaashorstPath, "MaaikeMaashorst20192022"),paste0(firstpart, MaashorstPath, "BullEverestMaashorst2022"), 
             paste0(firstpart, SlikkenvdHeenPath, "CaliopeSvdH20202021"), paste0(firstpart, SlikkenvdHeenPath, "NadiaSvdH20202022"), 
             paste0(firstpart, KraansvlakPath, "Kraansvlak20202022"))

# Write quality filter output to csv
for(i in 1:length(TrackList)){
  QualityFilter(GPSdata = TrackList[[i]], value = 10,
                path = PathVec[i])
}
















