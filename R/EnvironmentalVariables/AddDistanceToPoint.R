# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to calculate and add the distance to specific land use types as an
### attribute to the points in the GPS track


## Load files of step 1 of environmental variables

# Paths to tracks step 1
setwd("~/WisentWishes")
EVStep1Path <- "~/WisentWishes/MScThesisData/GPS location data/Step1EV/"
EVStep1Vec <- list.files(path = EVStep1Path)

# Get names of tracks, without .csv
NameVec <- c()
for(i in seq_along(EVStep1Vec)){
  NameVec[i] <- tools::file_path_sans_ext(EVStep1Vec[i])
}

# Create list of files from preprocessed directory
LanduseTracks <- lst()
for(i in seq_along(EVStep1Vec)){
  LanduseTracks[[i]] <- read_csv(file = paste0(EVStep1Path, EVStep1Vec[i]))
}

# Create function to add distance attributes to GPS tracks
ExtractDistances <-  function(GPSTrackList, NameVec, LanduseList){
  
  # Create lists distances
  WaterDistanceList <- list()
  ForestDistanceList <- list()
  RoadDistanceList <- list()
  
  # Iterate over elements of LanduseList to create distance raster for water,
  # forest and roads. 
  for(i in seq_along(LanduseList)){
    
    # Select water values 
    Water <- LanduseList[[i]] == 5
    
    # Put non-water values to NA
    Water[Water < 1] <- NA
    
    # Create water distance raster
    WaterDistanceList[[i]] <- raster::distance(Water)
    
    # Select forest values 
    Forest <- LanduseList[[i]] %in% c(3, 4)
    
    # Put non-forest values to NA
    Forest[Forest < 1] <- NA
    
    # Create forest distance raster
    ForestDistanceList[[i]] <- raster::distance(Forest)
    
    # Select road values 
    Road <- LanduseList[[i]] == 8
    
    # Put non-road values to NA
    Road[Road < 1] <- NA
    
    # Create road distance raster
    RoadDistanceList[[i]] <- raster::distance(Road)
    
  }
  
  # Iterate over elements of GPSTrackList to add the distances to each element
  for(i in seq_along(GPSTrackList)){
    # The first 4 tracks are the Kraansvlak tracks
    if(i %in% 1:4){
      
      TrackSf <- st_as_sf(GPSTrackList[[i]], coords = c("X", "Y"), crs = 28992)
      
      # Add distances to the GPSTrackList element as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>% 
        mutate(WaterDistance = raster::extract(WaterDistanceList[[1]], TrackSf)) %>% 
        mutate(ForestDistance = raster::extract(ForestDistanceList[[1]], TrackSf)) %>% 
        mutate(RoadDistance = raster::extract(RoadDistanceList[[1]], TrackSf)) 
    
    }
    # Track 5 and 8 are Maashorst 2016 tracks
    else if(i %in% c(5,8)){
      
      TrackSf <- st_as_sf(GPSTrackList[[i]], coords = c("X", "Y"), crs = 28992)
      
      # Add distances to the GPSTrackList element as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>% 
        mutate(WaterDistance = raster::extract(WaterDistanceList[[2]], TrackSf)) %>% 
        mutate(ForestDistance = raster::extract(ForestDistanceList[[2]], TrackSf)) %>% 
        mutate(RoadDistance = raster::extract(RoadDistanceList[[2]], TrackSf)) 
      
    }
    # Track 6 is a Maashorst 2022 track
    else if(i %in% c(6)){
      
      TrackSf <- st_as_sf(GPSTrackList[[i]], coords = c("X", "Y"), crs = 28992)
      
      # Add distances to the GPSTrackList element as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>% 
        mutate(WaterDistance = raster::extract(WaterDistanceList[[3]], TrackSf)) %>% 
        mutate(ForestDistance = raster::extract(ForestDistanceList[[3]], TrackSf)) %>% 
        mutate(RoadDistance = raster::extract(RoadDistanceList[[3]], TrackSf)) 
      
    }
    # Track 7 and 9:15 are Maashorst 2017 - 2021 tracks
    else if(i %in% c(7, 9:15)){
      
      TrackSf <- st_as_sf(GPSTrackList[[i]], coords = c("X", "Y"), crs = 28992)
      
      # Add distances to the GPSTrackList element as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>% 
        mutate(WaterDistance = raster::extract(WaterDistanceList[[4]], TrackSf)) %>% 
        mutate(ForestDistance = raster::extract(ForestDistanceList[[4]], TrackSf)) %>% 
        mutate(RoadDistance = raster::extract(RoadDistanceList[[4]], TrackSf)) 
      
    }
    # Track 16 - 19 are Slikken vd Heen habituate tracks
    else if(i %in% c(16:19)){
      
      TrackSf <- st_as_sf(GPSTrackList[[i]], coords = c("X", "Y"), crs = 28992)
      
      # Add distances to the GPSTrackList element as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>% 
        mutate(WaterDistance = raster::extract(WaterDistanceList[[5]], TrackSf)) %>% 
        mutate(ForestDistance = raster::extract(ForestDistanceList[[5]], TrackSf)) %>% 
        mutate(RoadDistance = raster::extract(RoadDistanceList[[5]], TrackSf)) 
      
    }
    # Track 20 - 22 are Slikken vd Heen tracks
    else if(i %in% c(20:22)){
      
      TrackSf <- st_as_sf(GPSTrackList[[i]], coords = c("X", "Y"), crs = 28992)
      
      # Add distances to the GPSTrackList element as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>% 
        mutate(WaterDistance = raster::extract(WaterDistanceList[[6]], TrackSf)) %>% 
        mutate(ForestDistance = raster::extract(ForestDistanceList[[6]], TrackSf)) %>% 
        mutate(RoadDistance = raster::extract(RoadDistanceList[[6]], TrackSf)) 
      
    }
    # Track 23 - 27 are Veluwe habituate tracks
    else if(i %in% c(23:27)){
      
      TrackSf <- st_as_sf(GPSTrackList[[i]], coords = c("X", "Y"), crs = 28992)
      
      # Add distances to the GPSTrackList element as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>% 
        mutate(WaterDistance = raster::extract(WaterDistanceList[[7]], TrackSf)) %>% 
        mutate(ForestDistance = raster::extract(ForestDistanceList[[7]], TrackSf)) %>% 
        mutate(RoadDistance = raster::extract(RoadDistanceList[[7]], TrackSf)) 
      
    }
    # Track 28 - 47 are Veluwe tracks
    else if(i %in% c(28:47)){
      
      TrackSf <- st_as_sf(GPSTrackList[[i]], coords = c("X", "Y"), crs = 28992)
      
      # Add distances to the GPSTrackList element as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>% 
        mutate(WaterDistance = raster::extract(WaterDistanceList[[8]], TrackSf)) %>% 
        mutate(ForestDistance = raster::extract(ForestDistanceList[[8]], TrackSf)) %>% 
        mutate(RoadDistance = raster::extract(RoadDistanceList[[8]], TrackSf)) 
      
    }
  }
  
  # Add names to  GPSTrackList
  names(GPSTrackList) <- NameVec
  
  # Return the GPSTrackList
  return(GPSTrackList)
 
}

# Call ExtractDistances
DistanceTracks <- ExtractDistances(LanduseTracks, NameVec, MaskedList)


## Write output to file

# Create path
path <- "~/WisentWishes/MScThesisData/GPS location data/Step2EV/"

# Create directory
if(!dir.exists(path)){
  dir.create(path)
}

# Write elements of Landuse Tracks list to file
for(i in seq_along(names(DistanceTracks))){
  write_csv(DistanceTracks[[i]], file = paste0(path, names(DistanceTracks)[i], ".csv"))
} 



