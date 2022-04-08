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





