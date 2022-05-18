# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to extract the land use class to each point in the GPS tracks


## Load files from the last step of the preprocessing

# Paths to preprocessed tracks
setwd("~/WisentWishes")
GPSPreprocessedPath <- "~/WisentWishes/MScThesisData/GPS location data/Step8Preprocess/"
GPSPreprocessedVec <- list.files(path = GPSPreprocessedPath)

# Get names of tracks, without .csv
NameVec <- c()
for(i in seq_along(GPSPreprocessedVec)){
  NameVec[i] <- tools::file_path_sans_ext(GPSPreprocessedVec[i])
}

# Create list of files from preprocessed directory
PreprocessedTracks <- lst()
for(i in seq_along(GPSPreprocessedVec)){
  PreprocessedTracks[[i]] <- read_csv(file = paste0(GPSPreprocessedPath, GPSPreprocessedVec[i]))
}

# Add names to list to understand what tibbles are from which tracks
names(PreprocessedTracks) <- NameVec


## Create function to add landuse attribute to tracks
AddLandUse <- function(GPSTrackList, LanduseMapList){
  
  # Make result list
  LandUseTrackList <- list()
  
  # Loop over elements of GPSTrackList
  for(i in seq_along(GPSTrackList)){
    
    # The first 4 tracks are the Kraansvlak tracks
    if(i >= 1 & i <= 4){
       
      # Make the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Extract values from Kraansvlak landuse map
      LandUsePerRow <- raster::extract(LandUseMaskList$Kraansvlak, GPSTrackList[[i]])
      
      # Add the landuse class as an attribute to the point dataset
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(landuse_class = LandUsePerRow) %>% 
        mutate(ifelse(landuse_class == 46, 1, landuse_class)) %>% 
        mutate(ifelse(landuse_class == 47, 1, landuse_class)) %>% 
        mutate(ifelse(landuse_class == 332, 10, landuse_class))
        
    }
  }
}

TryOut <- PreprocessedTracks$KraansvlakTrack1
  
coordinates(TryOut) <- c("X", "Y")

test <- raster::extract(LandUseMaskList$Kraansvlak, TryOut)

EndTry <- TryOut %>%
  as_tibble() %>% 
  mutate(landuse_class = test) %>% 
  if(landuse_class == 46){mutate(landuse_class = 1)} %>% 
  if(landuse_class == 47){mutate(landuse_class = 1)} %>% 
  if(landuse_class == 332){mutate(landuse_class = 10)}

EndTry$landuse_class

EndTry <- TryOut %>%
  as_tibble() %>% 
  mutate(landuse_class = test) 






