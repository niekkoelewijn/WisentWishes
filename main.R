# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022


### DESCRIPTION ###

# In this project GPS-data of the European bison is analysed to investigate
# its habitat selection. The project is carried out in seperate steps. The
# first step is the pre-processing of GPS-data


### PREPROSSESSING ###

# Step 1: Cleaning input data
source("~/WisentWishes/R/CleanInputdata.R")

# Step 2: Spatial filtering of input data
source("~/WisentWishes/R/SpatialFiltering.R")

# Here comes eventually some temporal filtering, if needed!

# Quality filtering the GPS datasets so that all points with hdop > 10 are removed
source("~/WisentWishes/R/QualityFiltering.R")

# Filter speed and turning angle
source("~/WisentWishes/R/SpeedAngleFilter.R")





