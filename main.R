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
source("~/WisentWishes/R/GPSTracksPreprocessing/CleanInputdata.R")

# Step 2: Spatial filtering of input data
source("~/WisentWishes/R/GPSTracksPreprocessing/SpatialFiltering.R")

# Step 3: Add speed / angle attributes
source("~/WisentWishes/R/GPSTracksPreprocessing/SpeedAngleAttributes.R")

# Step 4: Temporal splitting
source("~/WisentWishes/R/GPSTracksPreprocessing/TemporalSplitting.R")

# Step 5a: Adapt Kraansvlak tracks based on logbook with interventions
source("~/WisentWishes/R/GPSTracksPreprocessing/KraansvlakSplitting.R")

# Step 5b: Adapt Veluwe tracks on logbook with interventions
source("~/WisentWishes/R/GPSTracksPreprocessing/VeluweSplitting.R")

# Step 5c: Adapt Maashorst tracks on logbook with interventions
source("~/WisentWishes/R/GPSTracksPreprocessing/MaashorstSplitting.R")

# Step 5d: Adapt Slikken vd Heen tracks on logbook with interventions
source("~/WisentWishes/R/GPSTracksPreprocessing/SlikkenvdHeenSplitting.R")

# Step 6: Interpolation of tracks to get equal step sizes
# Fear not when this step returns an error, it mostly works 
# after 3-4 tries.
source("~/WisentWishes/R/GPSTracksPreprocessing/Interpolating.R")

# Step 7: Replace interpolated points that fall outside the study area
source("~/WisentWishes/R/GPSTracksPreprocessing/ReplacePoints.R")

# Step 8: Finalize track preprocessing
source("~/WisentWishes/R/GPSTracksPreprocessing/TrackCleaning.R")


### ENVIRONMENTAL VARIABLES ###

# Step 1: Land use map preprocessing
source("~/WisentWishes/R/EnvironmentalVariables/LandusePreprocessing.R")

# Step 2: Adding land use class to points of GPS tracks
source("~/WisentWishes/R/EnvironmentalVariables/AddLanduseToPoint.R")

# Step 3: Adding distance attributes to points of GPS tracks
source("~/WisentWishes/R/EnvironmentalVariables/AddDistanceToPoint.R")

# Step 4: Add date and weather variables to the sctipt
source("~/WisentWishes/R/EnvironmentalVariables/AddDateAndWeather.R")


### RESULTS ###

# Step 1: Calculate proportion habitat available per study area
source("~/WisentWishes/R/Results/JacobsIndex/HabitatProportion.R")

# Step 2: Put tracks in correct format for jacobs index calculations
source("~/WisentWishes/R/Results/JacobsIndex/ClusterTracks.R")

# Step 3: Calculate Jacob's index per landuse class per study area
source("~/WisentWishes/R/Results/JacobsIndex/JI_per_studyarea.R")

# Step 4: Calculate overall Jacob's index
source("~/WisentWishes/R/Results/JacobsIndex/JI_overall.R")

# Step 5: Clarify relation Jacob's index and proportion availability of a landuse class
source("~/WisentWishes/R/Results/JacobsIndex/RelationProportionAvailableAndJI.R")

# Step 6: Visualizations in ggplot of JI overall, per study area and relation with availability
source("~/WisentWishes/R/Results/JacobsIndex/VisualizationsJI.R")

# Step 7: Exploratory Jacob's indexes to see influence of variables
source("~/WisentWishes/R/Results/JacobsIndex/VariableInfluenceJI.R")






