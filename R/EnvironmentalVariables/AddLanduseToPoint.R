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

# Adapt LUT to current land use maps
LUTLanduseClasses <- LUTLGN %>% 
  select(landuse_class, landuse_class_code) %>% 
  rename(landuse_code = landuse_class_code) %>% 
  distinct()

## Create function to add landuse attribute to tracks
AddLanduse <- function(GPSTrackList, LanduseMapList){
  
  # Loop over elements of GPSTrackList
  for(i in seq_along(GPSTrackList)){
    
    # The first 4 tracks are the Kraansvlak tracks
    if(i %in% 1:4){
       
      # Make the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Extract values from Kraansvlak landuse map
      LandUsePerRow <- raster::extract(MaskedList$Kraansvlak, GPSTrackList[[i]])
      
      # Add the landuse class as an attribute to the point dataset
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(landuse_code = LandUsePerRow) %>% 
        inner_join(LUTLanduseClasses, by = "landuse_code")
      
    }
    # Track 5 and 8 are Maashorst 2016 tracks
    else if(i %in% c(5,8)){
      
      # Make the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Extract values from Maashorst 2016 landuse map
      LandUsePerRow <- raster::extract(MaskedList$Maashorst2016, GPSTrackList[[i]])
      
      # Add the landuse class as an attribute to the point dataset
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(landuse_code = LandUsePerRow) %>% 
        inner_join(LUTLanduseClasses, by = "landuse_code")
    }
    # Track 6 is a Maashorst 2022 track
    else if(i %in% c(6)){
      
      # Make the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Extract values from Maashorst 2016 landuse map
      LandUsePerRow <- raster::extract(MaskedList$Maashorst2022, GPSTrackList[[i]])
      
      # Add the landuse class as an attribute to the point dataset
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(landuse_code = LandUsePerRow) %>% 
        inner_join(LUTLanduseClasses, by = "landuse_code")
    }
    # Track 7 and 9:15 are Maashorst 2017 - 2021 tracks
    else if(i %in% c(7, 9:15)){
      
      # Make the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Extract values from Maashorst 2016 landuse map
      LandUsePerRow <- raster::extract(MaskedList$Maashorst20172021, GPSTrackList[[i]])
      
      # Add the landuse class as an attribute to the point dataset
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(landuse_code = LandUsePerRow) %>% 
        inner_join(LUTLanduseClasses, by = "landuse_code")
    }
    # Track 16 - 19 are Slikken vd Heen habituate tracks
    else if(i %in% c(16:19)){
      
      # Make the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Extract values from Maashorst 2016 landuse map
      LandUsePerRow <- raster::extract(MaskedList$SlikkenvdHeenHabituate, GPSTrackList[[i]])
      
      # Add the landuse class as an attribute to the point dataset
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(landuse_code = LandUsePerRow) %>% 
        inner_join(LUTLanduseClasses, by = "landuse_code")
    }
    # Track 20 - 22 are Slikken vd Heen tracks
    else if(i %in% c(20:22)){
      
      # Make the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Extract values from Maashorst 2016 landuse map
      LandUsePerRow <- raster::extract(MaskedList$SlikkenvdHeen, GPSTrackList[[i]])
      
      # Add the landuse class as an attribute to the point dataset
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(landuse_code = LandUsePerRow) %>% 
        inner_join(LUTLanduseClasses, by = "landuse_code")
    }
    # Track 23 - 27 are Veluwe habituate tracks
    else if(i %in% c(23:27)){
      
      # Make the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Extract values from Maashorst 2016 landuse map
      LandUsePerRow <- raster::extract(MaskedList$VeluweHabituate, GPSTrackList[[i]])
      
      # Add the landuse class as an attribute to the point dataset
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(landuse_code = LandUsePerRow) %>% 
        inner_join(LUTLanduseClasses, by = "landuse_code")
    }
    # Track 28 - 47 are Veluwe tracks
    else if(i %in% c(28:47)){
      
      # Make the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Extract values from Maashorst 2016 landuse map
      LandUsePerRow <- raster::extract(MaskedList$Veluwe, GPSTrackList[[i]])
      
      # Add the landuse class as an attribute to the point dataset
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(landuse_code = LandUsePerRow) %>% 
        inner_join(LUTLanduseClasses, by = "landuse_code")
    }
  }
  
  # Return track list
  return(GPSTrackList)
}

# Call AddLanduse
LanduseTracks <- AddLanduse(PreprocessedTracks, MaskedList)


## Write output to file

# Create path
path <- "~/WisentWishes/MScThesisData/GPS location data/Step1EV/"

# Create directory
if(!dir.exists(path)){
  dir.create(path)
}

# Write elements of Landuse Tracks list to file
for(i in seq_along(names(LanduseTracks))){
  write_csv(LanduseTracks[[i]], file = paste0(path, names(LanduseTracks)[i], ".csv"))
} 



coordinates(PreprocessedTracks$VeluweTrack13) <- c("X", "Y")
plot(PreprocessedTracks$VeluweTrack11)

