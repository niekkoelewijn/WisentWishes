# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to clean tracks. End product of this script is ready to assign
### environmental variables to.

### The tracks currently still have datapoints that are not part of the 1 hour
### interval sequence. These points have to be removed from the dataset, they
### have the attribute locType "o". The tracks have lots of unnessesary colums
### and some column names should be renamed. The temperature and hdop attribute 
### needs to be interpolated for interpolated points.


## Load files from step 7

# Paths to step 7 tracks
setwd("~/WisentWishes")
GPSStep7Path <- "~/WisentWishes/MScThesisData/GPS location data/Step7Preprocess/"
GPSStep7Vec <- list.files(path = GPSStep7Path)

# Get names of tracks, without .csv
NameVec <- c()
for(i in seq_along(GPSStep7Vec)){
  NameVec[i] <- tools::file_path_sans_ext(GPSStep7Vec[i])
}

# Create list of files from step 7 directory
Step7Tracks <- lst()
for(i in seq_along(GPSStep7Vec)){
  Step7Tracks[[i]] <- read_csv(file = paste0(GPSStep7Path, GPSStep7Vec[i]))
}

# Add names to list to understand what tibbles are from which tracks
names(Step7Tracks) <- NameVec


## Create general function to clean tracks
TrackCleaning <- function(GPSTrackList, NameVec){
  
  # Make copy of GPSTrackList
  CleanedTrackList <- GPSTrackList
  
  # Create function for temperature for each element in list
  for(i in seq_along(CleanedTrackList)){
    
    # Remove all NA values for TimeNum, this is how you remove the interpolated
    # points out of the track
    TrackElement <- CleanedTrackList[[i]] %>% 
      as_tibble() %>% 
      drop_na(TimeNum) %>% 
      arrange(time_coded)
    
    # Get vector of time coded, temperature and hdop
    time <- TrackElement$time_coded
    temp <- TrackElement$temp
    hdop <- TrackElement$hdop
    
    # Interpolate linear function for temperature between each data point
    temp_fun <- approxfun(x = time, y = temp, method = "linear")
    
    # Interpolate linear function for hdop between each data point
    hdop_fun <- approxfun(x = time, y = hdop, method = "linear")
    
    # Assign interpolated temperature and hdop values to interpolated points
    CleanedTrackList[[i]] <- CleanedTrackList[[i]] %>%
      as_tibble() %>% 
      mutate(temp = ifelse(is.na(temp), temp_fun(as.numeric(time)), temp)) %>% 
      mutate(hdop = ifelse(is.na(hdop), hdop_fun(as.numeric(time)), hdop))
  }
  
  # Name elements from list
  names(CleanedTrackList) <- NameVec
  
  # Remove "o" for all elements in list
  for(i in seq_along(CleanedTrackList)){
    
    # Remove "o" rows
    CleanedTrackList[[i]] <- CleanedTrackList[[i]] %>%
      
      # Get only the points on a 1 hour interval
      filter(locType == "p") %>% 
      
      # Sort on time
      arrange(time) %>% 
      
      # Get unique ID number to row
      rowid_to_column("PointID") %>%
      
      # Select subset of columns
      select(PointID, time, time_coded, mu.x, mu.y, temp, hdop, ID) %>% 
      
      # Rename columns
      dplyr::rename(ID = PointID, track_ID = ID, X = mu.x, Y = mu.y) %>% 
      
      # recalculate time_coded
      mutate(time_coded = as.numeric(time))
    
    # Add speed, angle, time interval and step length
    CleanedTrackList[[i]] <- CleanedTrackList[[i]] %>% 
      
      # Add attribute for speed in
      mutate(speed_in = atl_get_speed(data = CleanedTrackList[[i]], x = "X", y = "Y",
                                      time = "time_coded", type = "in")) %>% 
      
      # Add attribute for speed out
      mutate(speed_out = atl_get_speed(data = CleanedTrackList[[i]], x = "X", y = "Y",
                                       time = "time_coded", type = "out")) %>%
      
      # Add attribute for turning angle
      mutate(angle = atl_turning_angle(data = CleanedTrackList[[i]], x = "X", y = "Y",
                                       time = "time_coded")) %>% 
      
      # Add attribute for time interval
      mutate(time_interval = time_coded - lag(time_coded)) %>% 
      
      # Add attribute for step length
      mutate(step_length = distance(X, lag(X), Y, lag(Y)))
  }
  
  # Return CleanedTrackList
  return(CleanedTrackList)
}

CleanedTrackList <- TrackCleaning(Step7Tracks, NameVec)


## Write the elements of the lists to files

# Create path to directory
path <- "~/WisentWishes/MScThesisData/GPS location data/Step8Preprocess/"

# Create directory
if(!dir.exists(path)){
  dir.create(path)
}

# Write elements of Cleaned Track List to file
for(i in seq_along(names(CleanedTrackList))){
  write_csv(CleanedTrackList[[i]], file = paste0(path, names(CleanedTrackList)[i], ".csv"))
} 


