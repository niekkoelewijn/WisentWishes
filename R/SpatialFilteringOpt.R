# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

# Install required packages
if(!"rgdal" %in% rownames(installed.packages())){install.packages("rgdal")}
if(!"sf" %in% rownames(installed.packages())){install.packages("sf")}
if(!"raster" %in% rownames(installed.packages())){install.packages("raster")}
if(!"atlastools" %in% rownames(installed.packages())){install.packages("atlastools")}
if(!"tidyverse" %in% rownames(installed.packages())){install.packages("tidyverse")}

# Load required packages
library(rgdal)
library(sf)
library(raster)
library(atlastools)
library(tidyverse)

# Function to spatially filter GPS-data
SpatialFilter <- function(GPSdata, spatial filter){
  
}