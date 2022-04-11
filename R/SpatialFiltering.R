# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

## function to load shapefiles of study areas in R ##

# Install required packages
if(!"rgdal" %in% rownames(installed.packages())){install.packages("rgdal")}
if(!"sf" %in% rownames(installed.packages())){install.packages("sf")}
if(!"raster" %in% rownames(installed.packages())){install.packages("raster")}
if(!"atlastools" %in% rownames(installed.packages())){install.packages("atlastools")}


# Load required packages
library(rgdal)
library(sf)
library(raster)
library(atlastools)

# Create vector of paths to study area shapefiles
VeluwePath <- "~/WisentWishes/MScThesisData/GISFilesAreas/Veluwe/WisentGebiedVeluwe.shp"
Maashorst2016Path <- "~/WisentWishes/MScThesisData/GISFilesAreas/Maashorst/WisentGebied2016.shp"
Maashorst20172021Path <- "~/WisentWishes/MScThesisData/GISFilesAreas/Maashorst/WisentGebied2017-2021.shp"
Maashorst2022Path <- "~/WisentWishes/MScThesisData/GISFilesAreas/Maashorst/WisentGebied2022.shp"
SlikkenvdHeenPath <- "~/WisentWishes/MScThesisData/GISFilesAreas/SlikkenvdHeen/WisentGebied.shp"
KraansvlakPath <- "~/WisentWishes/MScThesisData/GISFilesAreas/Kraansvlak/WisentGebiedKraansvlak.shp"
PathVec <- c(VeluwePath, Maashorst2016Path,Maashorst20172021Path,Maashorst2022Path, SlikkenvdHeenPath, KraansvlakPath)

# Read study area shapefiles into R
VeluweStudyArea <- readOGR(PathVec[1])
Maashorst2016StudyArea <- readOGR(PathVec[2])
Maashorst20172021StudyArea <- readOGR(PathVec[3])
Maashorst2022StudyArea <- readOGR(PathVec[4])
SlikkenvdHeenStudyArea <- readOGR(PathVec[5])
KraansvlakStudyArea <- readOGR(PathVec[6])

# Convert polygons to sf
VeluweStudyAreaSf <- st_as_sf(VeluweStudyArea)
Maashorst2016StudyAreaSf <- st_as_sf(Maashorst2016StudyArea)
Maashorst20172021StudyAreaSf <- st_as_sf(Maashorst20172021StudyArea)
Maashorst2022StudyAreaSf <- st_as_sf(Maashorst2022StudyArea)
SlikkenvdHeenStudyAreaSf <- st_as_sf(SlikkenvdHeenStudyArea)
KraansvlakStudyAreaSf <- st_as_sf(KraansvlakStudyArea)

# Load GPS-data csv's into R

# Veluwe
SharaVeluwe20162020 <- read.csv("~/WisentWishes/MScThesisData/GPS location data/Veluwe/VeluweData2016-2020.csv")
names(SharaVeluwe20162020)[names(SharaVeluwe20162020) == 'Latitude'] <- "lat"
names(SharaVeluwe20162020)[names(SharaVeluwe20162020) == 'Longitude'] <- "lng"
SharaVeluwe2022 <- read.csv("~/WisentWishes/MScThesisData/GPS location data/Veluwe/SharaVeluwe2022.csv")

# Maashorst
DeliaMaashorst2016 <- read.csv("~/WisentWishes/MScThesisData/GPS location data/Maashorst/DeliaMaashorst2016.csv")
KraylaMaashorst2016 <- read.csv("~/WisentWishes/MScThesisData/GPS location data/Maashorst/KraylaMaashorst2016.csv")
KroosjaMaashorst20162018 <- read.csv("~/WisentWishes/MScThesisData/GPS location data/Maashorst/KroosjaMaashorst2016-2018.csv")
MaaikeMaashorst20192022 <- read.csv("~/WisentWishes/MScThesisData/GPS location data/Maashorst/MaaikeMaashorst2019-2022.csv")
NevayaMaashorst2022 <- read.csv("~/WisentWishes/MScThesisData/GPS location data/Maashorst/NevayaMaashorst2022.csv")
BullEverestMaashorst2022 <- read.csv("~/WisentWishes/MScThesisData/GPS location data/Maashorst/BullEverestMaashorst2022.csv")

# Slikken vd Heen
CaliopeSvdH20202021 <- read.csv("~/WisentWishes/MScThesisData/GPS location data/SlikkenvdHeen/CaliopeSvdH2020-2021.csv")
NadiaSvdH20202022 <- read.csv("~/WisentWishes/MScThesisData/GPS location data/SlikkenvdHeen/NadiaSvdH2020-2022.csv")

# Kraansvlak, rename columns and filter for port 1 and assign most recent temperature attribute 
Kraansvlak20202022 <- read.csv("~/WisentWishes/MScThesisData/GPS location data/Kraansvlak/Kraansvlak.csv")
names(Kraansvlak20202022)[names(Kraansvlak20202022) == 'latitude'] <- "lat"
names(Kraansvlak20202022)[names(Kraansvlak20202022) == 'longitude'] <- "lng"
for(i in 1:length(Kraansvlak20202022[,"temperature"])){
  if(Kraansvlak20202022[i,"temperature"] == "\\N"){
    Kraansvlak20202022[i,"temperature"] = Kraansvlak20202022[i-1,"temperature"]
  }
}
Kraansvlak20202022 <- Kraansvlak20202022[Kraansvlak20202022$port == 1, ]
Kraansvlak20202022 <- Kraansvlak20202022[, c(31, 30, 1, 21, 22, 12, 25)]

### Filter with polygons ###

# Function
SpatialFilter <- function(GPSfile, polygon, x, y, remove= F, path, name_file){
  SpatialFiltered <- atlastools::atl_filter_bounds(data = GPSfile, 
                                                   x = x, y = y, 
                                                   sf_polygon = polygon,
                                                   remove_inside = remove)
  write.csv(SpatialFiltered, file = paste0(path, name_file, "_spatialfiltered.csv"))
}


## Veluwe 

# Preprocessed directory
dir <- "~/WisentWishes/MScThesisData/GPS location data/Veluwe/Preprocessed"
if(!dir.exists(dir)){
  dir.create(dir)
}

# List with gps data and vector with names
VeluweTracks <- list(SharaVeluwe20162020, SharaVeluwe2022)
VeluweTracksChar <- c("SharaVeluwe20162020", "SharaVeluwe2022")

# For loop to write files
for(i in 1:length(VeluweTracks)){
  SpatialFilter(GPSfile = VeluweTracks[[i]], 
                polygon = VeluweStudyAreaSf, 
                x = "lng", y = "lat", name_file = VeluweTracksChar[i],
                path = "~/WisentWishes/MScThesisData/GPS location data/Veluwe/Preprocessed/")
}

## Maashorst

# Preprocessed directory
dir <- "~/WisentWishes/MScThesisData/GPS location data/Maashorst/Preprocessed"
if(!dir.exists(dir)){
  dir.create(dir)
}

# List with gps data, vector with names and list with study area polygons
MaashorstIndividuals <- list(DeliaMaashorst2016, KraylaMaashorst2016, KroosjaMaashorst20162018,
                             MaaikeMaashorst20192022, NevayaMaashorst2022, BullEverestMaashorst2022)
MaashorstIndividualsChar <- c("DeliaMaashorst2016", "KraylaMaashorst2016", "KroosjaMaashorst20162018",
                                  "MaaikeMaashorst20192022", "NevayaMaashorst2022", "BullEverestMaashorst2022")
MaashorstAreas <- list(Maashorst2016StudyAreaSf, Maashorst2016StudyAreaSf, Maashorst20172021StudyAreaSf, 
                       Maashorst20172021StudyAreaSf, Maashorst2022StudyAreaSf, Maashorst2022StudyAreaSf)

# For loop to write files
for(i in 1:length(MaashorstIndividuals)){
  SpatialFilter(GPSfile = MaashorstIndividuals[[i]], 
                polygon = MaashorstAreas[[i]], 
                x = "lng", y = "lat", name_file = MaashorstIndividualsChar[i],
                path = "~/WisentWishes/MScThesisData/GPS location data/Maashorst/Preprocessed/")
}

## Slikken vd Heen

# Preprocessed directory
dir <- "~/WisentWishes/MScThesisData/GPS location data/SlikkenvdHeen/Preprocessed"
if(!dir.exists(dir)){
  dir.create(dir)
}

# List with gps data and vector with names
SlikkenvdHeenIndividuals <- list(CaliopeSvdH20202021, NadiaSvdH20202022)
SlikkenvdHeenIndividualsChar <- c("CaliopeSvdH20202021", "NadiaSvdH20202022")

# For loop to write files
for(i in 1:length(SlikkenvdHeenIndividuals)){
  SpatialFilter(GPSfile = SlikkenvdHeenIndividuals[[i]], 
                polygon = SlikkenvdHeenStudyAreaSf, 
                x = "lng", y = "lat", name_file = SlikkenvdHeenIndividualsChar[i],
                path = "~/WisentWishes/MScThesisData/GPS location data/SlikkenvdHeen/Preprocessed/")
}

## Kraansvlak

# Preprocessed directory
dir <- "~/WisentWishes/MScThesisData/GPS location data/Kraansvlak/Preprocessed"
if(!dir.exists(dir)){
  dir.create(dir)
}

# Spatial Filter
SpatialFilter(GPSfile = Kraansvlak20202022, 
                polygon = KraansvlakStudyAreaSf, 
                x = "lng", y = "lat", name_file = "Kraansvlak20202022",
                path = "~/WisentWishes/MScThesisData/GPS location data/Kraansvlak/Preprocessed/")


## Transform study areas to RDnew for later steps
VeluweStudyAreaSfRDn <- st_transform(VeluweStudyAreaSf, st_crs(28992))
Maashorst2016StudyAreaSfRDn <- st_transform(Maashorst2016StudyAreaSf, st_crs(28992))
Maashorst20172021StudyAreaSfRDn <- st_transform(Maashorst20172021StudyAreaSf, st_crs(28992))
Maashorst2022StudyAreaSfRDn <- st_transform(Maashorst2022StudyAreaSf, st_crs(28992))
SlikkenvdHeenStudyAreaSfRDn <- st_transform(SlikkenvdHeenStudyAreaSf, st_crs(28992))
KraansvlakStudyAreaSfRDn <- st_transform(KraansvlakStudyAreaSf, st_crs(28992))





