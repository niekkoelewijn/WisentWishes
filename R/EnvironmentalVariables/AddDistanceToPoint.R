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
AddDistance <- function(GPSTrackList, NameVec){
  
  ### Create rasters of roads, forest and water land use types for each study area
  
  ## Kraansvlak
  
  # Create rasters roads, forest and water in the Kraansvlak
  KraansvlakWater <- MaskedList$Kraansvlak == 5
  KraansvlakForest <- MaskedList$Kraansvlak %in% c(3, 4)
  KraansvlakRoad <- MaskedList$Kraansvlak == 8
  
  # Put all values other than 1 to NA
  KraansvlakWater[KraansvlakWater < 1] <- NA
  KraansvlakForest[KraansvlakForest < 1] <- NA
  KraansvlakRoad[KraansvlakRoad < 1] <- NA
  
  # Transform raster to polygons
  KraansvlakWaterPolygons <- rasterToPolygons(KraansvlakWater)
  KraansvlakForestPolygons <- rasterToPolygons(KraansvlakForest)
  KraansvlakRoadPolygons <- rasterToPolygons(KraansvlakRoad)
  
  
  ## Maashorst 2016
  
  # Create rasters roads, forest and water in the Maashorst from 2016
  Maashorst2016Water <- MaskedList$Maashorst2016 == 5
  Maashorst2016Forest <- MaskedList$Maashorst2016 %in% c(3, 4)
  Maashorst2016Road <- MaskedList$Maashorst2016 == 8
  
  # Put all values other than 1 to NA
  Maashorst2016Water[Maashorst2016Water < 1] <- NA
  Maashorst2016Forest[Maashorst2016Forest < 1] <- NA
  Maashorst2016Road[Maashorst2016Road < 1] <- NA
  
  # Transform raster to polygons
  Maashorst2016WaterPolygons <- rasterToPolygons(Maashorst2016Water)
  Maashorst2016ForestPolygons <- rasterToPolygons(Maashorst2016Forest)
  Maashorst2016RoadPolygons <- rasterToPolygons(Maashorst2016Road)
  
  
  ## Maashorst 2022
  
  # Create rasters roads, forest and water in the Maashorst from 2022
  Maashorst2022Water <- MaskedList$Maashorst2022 == 5
  Maashorst2022Forest <- MaskedList$Maashorst2022 %in% c(3, 4)
  Maashorst2022Road <- MaskedList$Maashorst2022 == 8
  
  # Put all values other than 1 to NA
  Maashorst2022Water[Maashorst2022Water < 1] <- NA
  Maashorst2022Forest[Maashorst2022Forest < 1] <- NA
  Maashorst2022Road[Maashorst2022Road < 1] <- NA
  
  # Transform raster to polygons
  Maashorst2022WaterPolygons <- rasterToPolygons(Maashorst2022Water)
  Maashorst2022ForestPolygons <- rasterToPolygons(Maashorst2022Forest)
  Maashorst2022RoadPolygons <- rasterToPolygons(Maashorst2022Road)
  
  
  ## Maashorst 2027 - 2021
  
  # Create rasters roads, forest and water in the Maashorst from 2017 to 2021
  Maashorst20172021Water <- MaskedList$Maashorst20172021 == 5
  Maashorst20172021Forest <- MaskedList$Maashorst20172021 %in% c(3, 4)
  Maashorst20172021Road <- MaskedList$Maashorst20172021 == 8
  
  # Put all values other than 1 to NA
  Maashorst20172021Water[Maashorst20172021Water < 1] <- NA
  Maashorst20172021Forest[Maashorst20172021Forest < 1] <- NA
  Maashorst20172021Road[Maashorst20172021Road < 1] <- NA
  
  # Transform raster to polygons
  Maashorst20172021WaterPolygons <- rasterToPolygons(Maashorst20172021Water)
  Maashorst20172021ForestPolygons <- rasterToPolygons(Maashorst20172021Forest)
  Maashorst20172021RoadPolygons <- rasterToPolygons(Maashorst20172021Road)
  
  
  ## Slikken vd Heen habituate area
  
  # Create rasters roads, forest and water in the Slikken vd Heen habituate area
  SlikkenvdHeenHabituateWater <- MaskedList$SlikkenvdHeenHabituate == 5
  SlikkenvdHeenHabituateForest <- MaskedList$SlikkenvdHeenHabituate %in% c(3, 4)
  SlikkenvdHeenHabituateRoad <- MaskedList$SlikkenvdHeenHabituate == 8
  
  # Put all values other than 1 to NA
  SlikkenvdHeenHabituateWater[SlikkenvdHeenHabituateWater < 1] <- NA
  SlikkenvdHeenHabituateForest[SlikkenvdHeenHabituateForest < 1] <- NA
  SlikkenvdHeenHabituateRoad[SlikkenvdHeenHabituateRoad < 1] <- NA
  
  # Transform raster to polygons
  SlikkenvdHeenHabituateWaterPolygons <- rasterToPolygons(SlikkenvdHeenHabituateWater)
  SlikkenvdHeenHabituateForestPolygons <- rasterToPolygons(SlikkenvdHeenHabituateForest)
  SlikkenvdHeenHabituateRoadPolygons <- rasterToPolygons(SlikkenvdHeenHabituateRoad)
  
  
  ## Slikken vd Heen
  
  # Create rasters roads, forest and water in the Slikken vd Heen
  SlikkenvdHeenWater <- MaskedList$SlikkenvdHeen == 5
  SlikkenvdHeenForest <- MaskedList$SlikkenvdHeen %in% c(3, 4)
  SlikkenvdHeenRoad <- MaskedList$SlikkenvdHeen == 8
  
  # Put all values other than 1 to NA
  SlikkenvdHeenWater[SlikkenvdHeenWater < 1] <- NA
  SlikkenvdHeenForest[SlikkenvdHeenForest < 1] <- NA
  SlikkenvdHeenRoad[SlikkenvdHeenRoad < 1] <- NA
  
  # Transform raster to polygons
  SlikkenvdHeenWaterPolygons <- rasterToPolygons(SlikkenvdHeenWater)
  SlikkenvdHeenForestPolygons <- rasterToPolygons(SlikkenvdHeenForest)
  SlikkenvdHeenRoadPolygons <- rasterToPolygons(SlikkenvdHeenRoad)
  
  
  ## Veluwe habituate area
  
  # Create rasters roads, forest and water in the Veluwe habituate area
  VeluweHabituateWater <- MaskedList$VeluweHabituate == 5
  VeluweHabituateForest <- MaskedList$VeluweHabituate %in% c(3, 4)
  VeluweHabituateRoad <- MaskedList$VeluweHabituate == 8
  
  # Put all values other than 1 to NA
  VeluweHabituateWater[VeluweHabituateWater < 1] <- NA
  VeluweHabituateForest[VeluweHabituateForest < 1] <- NA
  VeluweHabituateRoad[VeluweHabituateRoad < 1] <- NA
  
  # Transform raster to polygons
  VeluweHabituateWaterPolygons <- rasterToPolygons(VeluweHabituateWater)
  VeluweHabituateForestPolygons <- rasterToPolygons(VeluweHabituateForest)
  VeluweHabituateRoadPolygons <- rasterToPolygons(VeluweHabituateRoad)

  
  ## Veluwe
  
  # Create rasters roads, forest and water in the Veluwe
  VeluweWater <- MaskedList$Veluwe == 5
  VeluweForest <- MaskedList$Veluwe %in% c(3, 4)
  VeluweRoad <- MaskedList$Veluwe == 8
  
  # Put all values other than 1 to NA
  VeluweWater[VeluweWater < 1] <- NA
  VeluweForest[VeluweForest < 1] <- NA
  VeluweRoad[VeluweRoad < 1] <- NA
  
  # Transform raster to polygons
  VeluweWaterPolygons <- rasterToPolygons(VeluweWater)
  VeluweForestPolygons <- rasterToPolygons(VeluweForest)
  VeluweRoadPolygons <- rasterToPolygons(VeluweRoad)
  
  ### There are two ponds in the Veluwe study area, that cannot be reached
  ### by the bisons, as they are fenced of. I will remove these unreachable
  ### ponds from the VeluweWaterPolygons
  
  # Polygons of the unreachable ponds
  UnreachableWater1 <- readOGR("~/WisentWishes/MScThesisData/EnvironmentalVariables/LGN2020/VeluweUnreachableWater1.shp")
  UnreachableWater2 <- readOGR("~/WisentWishes/MScThesisData/EnvironmentalVariables/LGN2020/VeluweUnreachableWater2.shp")
  
  # Subtract these two polygons from the VeluweWaterPolygons
  VeluweWaterPolygons <- VeluweWaterPolygons - UnreachableWater1
  VeluweWaterPolygons <- VeluweWaterPolygons - UnreachableWater2
  
  
  # Calculate distances to specific landuse classes for each point in each element in list
  for(i in seq_along(GPSTrackList)){
    
    # The first 4 tracks are the Kraansvlak tracks
    if(i %in% 1:4){
      
      # Convert the list element to a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Calculate the minimal distance to the land use classes for each point
      DistanceToWater <- apply(gDistance(KraansvlakWaterPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToForest <- apply(gDistance(KraansvlakForestPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToRoad <- apply(gDistance(KraansvlakRoadPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      
      # Add the distance to the land use classes as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(distance_to_water = DistanceToWater,
               distance_to_forest = DistanceToForest,
               distance_to_road = DistanceToRoad)
      
      print(paste0(i, "/47"))
      
    }
    # Track 5 and 8 are Maashorst 2016 tracks
    else if(i %in% c(5,8)){
      
      # Make from the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Calculate the minimal distance to the land use classes for each point
      DistanceToWater <- apply(gDistance(Maashorst2016WaterPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToForest <- apply(gDistance(Maashorst2016ForestPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToRoad <- apply(gDistance(Maashorst2016RoadPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      
      # Add the distance to the land use classes as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(distance_to_water = DistanceToWater,
               distance_to_forest = DistanceToForest,
               distance_to_road = DistanceToRoad)
      
      print(paste0(i, "/47"))
      
    }
    # Track 6 is a Maashorst 2022 track
    else if(i %in% c(6)){
      
      # Make from the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Calculate the minimal distance to the land use classes for each point
      DistanceToWater <- apply(gDistance(Maashorst2022WaterPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToForest <- apply(gDistance(Maashorst2022ForestPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToRoad <- apply(gDistance(Maashorst2022RoadPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      
      # Add the distance to the land use classes as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(distance_to_water = DistanceToWater,
               distance_to_forest = DistanceToForest,
               distance_to_road = DistanceToRoad)
      
      print(paste0(i, "/47"))
      
    }
    # Track 7 and 9:15 are Maashorst 2017 - 2021 tracks
    else if(i %in% c(7, 9:15)){
      
      # Make from the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Calculate the minimal distance to the land use classes for each point
      DistanceToWater <- apply(gDistance(Maashorst20172021WaterPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToForest <- apply(gDistance(Maashorst20172021ForestPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToRoad <- apply(gDistance(Maashorst20172021RoadPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      
      # Add the distance to the land use classes as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(distance_to_water = DistanceToWater,
               distance_to_forest = DistanceToForest,
               distance_to_road = DistanceToRoad)
      
      print(paste0(i, "/47"))
      
    }
    # Track 16 - 19 are Slikken vd Heen habituate tracks
    else if(i %in% c(16:19)){
      
      # Make from the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Calculate the minimal distance to the land use classes for each point
      DistanceToWater <- apply(gDistance(SlikkenvdHeenHabituateWaterPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToForest <- apply(gDistance(SlikkenvdHeenHabituateForestPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToRoad <- apply(gDistance(SlikkenvdHeenHabituateRoadPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      
      # Add the distance to the land use classes as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(distance_to_water = DistanceToWater,
               distance_to_forest = DistanceToForest,
               distance_to_road = DistanceToRoad)
      
      print(paste0(i, "/47"))
      
    }
    # Track 20 - 22 are Slikken vd Heen tracks
    else if(i %in% c(20:22)){
      
      # Make from the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Calculate the minimal distance to the land use classes for each point
      DistanceToWater <- apply(gDistance(SlikkenvdHeenWaterPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToForest <- apply(gDistance(SlikkenvdHeenForestPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToRoad <- apply(gDistance(SlikkenvdHeenRoadPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      
      # Add the distance to the land use classes as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(distance_to_water = DistanceToWater,
               distance_to_forest = DistanceToForest,
               distance_to_road = DistanceToRoad)
      
      print(paste0(i, "/47"))
      
    }
    # Track 23 - 27 are Veluwe habituate tracks
    else if(i %in% c(23:27)){
      
      # Make from the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Calculate the minimal distance to the land use classes for each point
      DistanceToWater <- apply(gDistance(VeluweHabituateWaterPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToForest <- apply(gDistance(VeluweHabituateForestPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToRoad <- apply(gDistance(VeluweHabituateRoadPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      
      # Add the distance to the land use classes as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(distance_to_water = DistanceToWater,
               distance_to_forest = DistanceToForest,
               distance_to_road = DistanceToRoad)
      
      print(paste0(i, "/47"))
      
    }
    # Track 28 - 47 are Veluwe tracks
    else if(i %in% c(28:47)){
      
      # Make from the tibble a Spatial point class
      coordinates(GPSTrackList[[i]]) <- c("X", "Y")
      
      # Calculate the minimal distance to the land use classes for each point
      DistanceToWater <- apply(gDistance(VeluweWaterPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToForest <- apply(gDistance(VeluweForestPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      DistanceToRoad <- apply(gDistance(VeluweRoadPolygons, GPSTrackList[[i]], byid=TRUE), 1, min)
      
      # Add the distance to the land use classes as an attribute
      GPSTrackList[[i]] <- GPSTrackList[[i]] %>%
        as_tibble() %>% 
        mutate(distance_to_water = DistanceToWater,
               distance_to_forest = DistanceToForest,
               distance_to_road = DistanceToRoad)
      
      print(paste0(i, "/47"))
      
    }
  }
  
  # Name elements from list
  names(GPSTrackList) <- NameVec
  
  # Return track list
  return(GPSTrackList)
     
}

# Call AddDistance
DistanceTracks <- AddDistance(LanduseTracks, NameVec)


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



