# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: Henjo de Knegt
# Year: 2022

## function to load shapefiles of study areas in R ##

# Install required packages
if(!"rgdal" %in% rownames(installed.packages())){install.packages("rgdal")}
if(!"sf" %in% rownames(installed.packages())){install.packages("sf")}
if(!"raster" %in% rownames(installed.packages())){install.packages("raster")}
if(!"dplyr" %in% rownames(installed.packages())){install.packages("dplyr")}


# Load required packages
library(rgdal)
library(sf)
library(raster)
library(dplyr)

# Create vector of paths to study area shapefiles
VeluwePath <- "~/WisentWishes/MScThesisData/GISFilesAreas/Veluwe/Wisentraster.shp"
MaashorstPath <- "~/WisentWishes/MScThesisData/GISFilesAreas/Maashorst/RE__GIS_data_Maashorst/StroomrasterBegrazingsgebied.shp"
SlikkenvdHeenPath <- "~/WisentWishes/MScThesisData/GISFilesAreas/SlikkenvdHeen/WisentGebied.shp"
KraansvlakPath <- "~/WisentWishes/MScThesisData/GISFilesAreas/Kraansvlak/Wisentgebied2014/Wisentgebied2014.shp"
PathVec <- c(VeluwePath, MaashorstPath, SlikkenvdHeenPath, KraansvlakPath)

# Read tudy area shapefiles into R
VeluweStudyArea <- readOGR(PathVec[1])
MaashorstStudyArea <- readOGR(PathVec[2])
SlikkenvdHeenStudyArea <- readOGR(PathVec[3])
KraansvlakStudyArea <- readOGR(PathVec[4])

# Transform sp to sf
VeluweStudyAreaSf <- st_as_sf(VeluweStudyArea) # RD new
MaashorstStudyAreaSf <- st_as_sf(MaashorstStudyArea) # WGS 84 / Pseudo-Mercator
SlikkenvdHeenStudyAreaSf <- st_as_sf(SlikkenvdHeenStudyArea) # WGS 84
KraansvlakStudyAreaSf <- st_as_sf(KraansvlakStudyArea) # RD new

# Transform proj4string to WGS84
TargetCRS <- st_crs(SlikkenvdHeenStudyAreaSf)

st_transform(x = VeluweStudyAreaSf, crs = TargetCRS)
st_transform(x = MaashorstStudyAreaSf, crs = TargetCRS)
st_transform(x = KraansvlakStudyAreaSf, crs = TargetCRS)

# Plot study areas in map of the Netherlands
path <- "~/WisentWishes/MScThesisData/GISFilesAreas/RDownloads/world-administrative-boundaries/world-administrative-boundaries.shp"
World <- readOGR(path)
WorldSf <- st_as_sf(World)
Netherlands <- WorldSf[WorldSf$iso3 == "NLD",]

for(i in 1:length(PathVec)){
  
}
