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

# Step 3: Add speed / angle attributes
source("~/WisentWishes/R/SpeedAngleAttributes.R")

# Step 4: Temporal splitting
source("~/WisentWishes/R/TemporalSplitting.R")

# Step 5a: Adapt Kraansvlak tracks based on logbook with interventions
source("~/WisentWishes/R/KraansvlakSplitting.R")

# Step 5b: Adapt Veluwe tracks on logbook with interventions
source("~/WisentWishes/R/VeluweSplitting.R")

# Step 5c: Adapt Maashorst tracks on logbook with interventions
source("~/WisentWishes/R/MaashorstSplitting.R")

# Step 5d: Adapt Slikken vd Heen tracks on logbook with interventions
source("~/WisentWishes/R/SlikkenvdHeenSplitting.R")

# Step 6: Interpolation of tracks to get equal step sizes
source("~/WisentWishes/R/Interpolating.R")

# Step 7: Replace interpolated points that fall outside the study area
source("~/WisentWishes/R/ReplacePoints.R")

