# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to interpolate GPS tracks to get the same temporal resolution
### of 1 hour for each track.


## Load files from step 5

# Paths to step 5 tracks
setwd("~/WisentWishes")
GPSStep5Path <- "~/WisentWishes/MScThesisData/GPS location data/Step5Preprocess/"
GPSStep5Vec <- list.files(path = GPSStep5Path)

# Get names of tracks, without .csv
NameVec <- c()
for(i in seq_along(GPSStep5Vec)){
  NameVec[i] <- tools::file_path_sans_ext(GPSStep5Vec[i])
}

# Create list of files from step 5 directory
Step5Tracks <- lst()
for(i in seq_along(GPSStep5Vec)){
  Step5Tracks[[i]] <- read_csv(file = paste0(GPSStep5Path, GPSStep5Vec[i]))
}

# Add names to list to understand what tibbles are from which tracks
names(Step5Tracks) <- NameVec


## Create function to interpolate points in tracks
InterpolateTracks <- function(GPSTrackList, NameVec){
  
  # Copy the content of GPSTrackList
  IDGPSTrackList <- GPSTrackList
  
  # Add a Track ID to tracks, as momentuHMM::crawlWrap requires a ID field
  for(i in seq_along(IDGPSTrackList)){
    IDGPSTrackList[[i]] <- mutate(IDGPSTrackList[[i]], ID = i)
  }
  
  # Interpolate tracks to a temporal resolution of 1 hour for each track
  InterpolatedGPSTrackList <- lst()
  for(i in seq_along(IDGPSTrackList)){
    InterpolatedGPSTrackList[[i]] <- momentuHMM::crawlWrap(IDGPSTrackList[[i]], 
                                                           Time.name = "time", coord = c("X", "Y"), 
                                                           time.scale = "hours", proj = 28992, timeStep = 3600)
    
  }
  
  # Get list of second element of each crawlWrap result (this is the interpolated track)
  ResultList <- lst()
  for(i in seq_along(InterpolatedGPSTrackList)){
    ResultList[[i]] <- InterpolatedGPSTrackList[[i]]$crwPredict
  }
  
  # Name elements from list
  names(ResultList) <- NameVec
  
  # Return interpolated tracks
  return(ResultList)
}

InterpolatedTrackList <- InterpolateTracks(Step5Tracks, NameVec)


## Write the elements of the lists to files

# Create path to directory
path <- "~/WisentWishes/MScThesisData/GPS location data/Step6Preprocess/"

# Create directory
if(!dir.exists(path)){
  dir.create(path)
}

# Write elements of Interpolated Track list to file
for(i in seq_along(names(InterpolatedTrackList))){
  write_csv(InterpolatedTrackList[[i]], file = paste0(path, names(InterpolatedTrackList)[i], ".csv"))
} 

# Script to visualize an interpolation
#VeluweSuppFedHabTrack1 <- CleanedTrackList$VeluweSuppFedHabTrack1
#VeluweSuppFedHabTrack1 <- as.tibble(VeluweSuppFedHabTrack1)
#InterpolatedTrackSf <- st_as_sf(VeluweSuppFedHabTrack1, coords = c("X", "Y"))
#InterpolatedTrackSf$X <- st_coordinates(InterpolatedTrackSf)[,1]
#InterpolatedTrackSf$Y <- st_coordinates(InterpolatedTrackSf)[,2]
#plot(st_geometry(TransformPolygon(VeluweHabituateArea)))
#plot(st_geometry(InterpolatedTrackSf), add=T)
#hist(log(InterpolatedTrackSf$speed_in))
#summary(VeluweSuppFedHabTrack1)



