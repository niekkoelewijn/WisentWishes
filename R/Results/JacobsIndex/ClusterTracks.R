# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script drag the tracks in the same study areas together, to determine r, the
### proportion of habitat used.


## Load files from the last step of environmental variable addition

# Path to tracks
setwd("~/WisentWishes")
TrackPath <- "~/WisentWishes/MScThesisData/GPS location data/Step3EV/"
TrackVec <- list.files(path = TrackPath)

# Get names of tracks, without .csv
NameVec <- c()
for(i in seq_along(GPSPreprocessedVec)){
  NameVec[i] <- tools::file_path_sans_ext(GPSPreprocessedVec[i])
}

# Create list of tracks
Tracks <- lst()
for(i in seq_along(TrackVec)){
  Tracks[[i]] <- read_csv(file = paste0(TrackPath, TrackVec[i]))
}

# Add names to tracks
names(Tracks) <- NameVec


## For each study area, cluster all tracks into 1 tibble

# Create general function to create 1 datasheet out of different tracks
ClusterTracks <- function(StudyArea, year = NULL){
  
  # Create path to tracks of the last step of environmental variable addition
  TrackPath <- "~/WisentWishes/MScThesisData/GPS location data/Step3EV/"
  
  if(is.null(year)){
    
    # List track files from the given study area
    TrackFiles <- list.files(path = TrackPath, pattern = StudyArea)
    
  }else if(year == 2016){
    
    # List track files from the given study area
    TrackFiles <- list.files(path = TrackPath, pattern = StudyArea)
    
    # Select only tracks from Maashorst 2016, track 5 and 8
    Maashorst5 <- TrackFiles[grepl(pattern = ".1.", TrackFiles)]
    Maashorst8 <- TrackFiles[grepl(pattern = ".4.", TrackFiles)]
    TrackFiles <- c(Maashorst5, Maashorst8)
    
  }else if(year == 2022){
    
    # List track files from the given study area
    TrackFiles <- list.files(path = TrackPath, pattern = StudyArea)
    
    # Select only track from Maashorst 2022, track 6
    TrackFiles <- TrackFiles[grepl(pattern = ".10.", TrackFiles)]
  }else if(year == 20172021){
    # List track files from the given study area
    TrackFiles <- list.files(path = TrackPath, pattern = StudyArea)
    
    # Select only track from Maashorst 2017-2021
    TrackFiles <- TrackFiles[c(3, 5:11)]
  }
  
  
  # Create an empty tibble with 28 columns, the number of attributes that each
  # point has
  TrackPoints <- as_tibble(data.frame(matrix(NA, nrow = 1, ncol = 28)))
  
  # Get column names from the track list of previous step
  colnames(TrackPoints) <- colnames(WeatherTracks$KraansvlakTrack1)
  
  # Iterate over elements of TrackFiles to read them and add them to TrackPoints
  for(i in seq_along(TrackFiles)){
    TrackPoints <- TrackPoints %>% 
      add_row(read_csv(file = paste0(TrackPath, TrackFiles[i]))) %>% 
      filter(!is.na(ID))
  }
  
  # Assign unique code to each row in the dataset
  TrackPoints <- TrackPoints %>% 
    rename(track_row_ID = ID) %>% 
    rowid_to_column("ID")
  
  # Return TrackPoints
  return(TrackPoints)
}


## Call ClusterTracks for each study area

# Kraansvlak
KraansvlakPoints <- ClusterTracks("Kraansvlak")

# Slikken vd Heen
SlikkenvdHeenPoints <- ClusterTracks("SlikkenvdHeenT")

# Slikken vd Heen habituate
SlikkenvdHeenHabPoints <- ClusterTracks("SlikkenvdHeenH")

# Veluwe
VeluwePoints <- ClusterTracks("VeluweT")

# Veluwe habituate
VeluweHabPoints <- ClusterTracks("VeluweH")


## At the Maashorst, the study area size was adapted multiple times, so 
## for each study area configuration, separate clusters of tracks are needed

# Maashorst 2016
Maashorst2016Points <- ClusterTracks("Maashorst", year = 2016)

# Maashorst 2017 - 2021
Maashorst20172021Points <- ClusterTracks("Maashorst", year = 20172021)

# Maashorst 2022
Maashorst2022Points <- ClusterTracks("Maashorst", year = 2022)


## Put datasets in 1 list
PointsList <- list()
PointsList[[1]] <- KraansvlakPoints
PointsList[[2]] <- Maashorst2016Points
PointsList[[3]] <- Maashorst20172021Points
PointsList[[4]] <- Maashorst2022Points
PointsList[[5]] <- SlikkenvdHeenHabPoints
PointsList[[6]] <- SlikkenvdHeen
PointsList[[7]] <- VeluweHabPoints
PointsList[[8]] <- VeluwePoints







