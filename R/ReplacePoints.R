# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to replace interpolated points in the tracks that felled outside the
### study area.

### In each study area, in some of the tracks, interpolated points where generated
### in places outside of the study area. In total 224 out of 122511 points fell
### outside the study area after interpolation, which is equal to 0.002% of the
### points. Out of 47 tracks, 16 had no interpolated points outside the study
### area, and 31 had 1 or more points outside the study area. Interpolated points
### outside the study area were mostly caused by the dispropotionate shape of the
### study areas, with lots of angles, edges, and for the Veluwe area a closed off
### area within the study area. I will replace the interpolated points, so that
### so that the track of the bisons becomes more realistic. No, when a point is
### generated outside the study area, the point is placed in a unrealistic short
### cut between two measured GPS points. By replacing these erroneous to more
### plausible places, the level of realism of the tracks increases.


## Load files from step 6

# Paths to step 6 tracks
setwd("~/WisentWishes")
GPSStep6Path <- "~/WisentWishes/MScThesisData/GPS location data/Step6Preprocess/"
GPSStep6Vec <- list.files(path = GPSStep6Path)

# Get names of tracks, without .csv
NameVec <- c()
for(i in seq_along(GPSStep6Vec)){
  NameVec[i] <- tools::file_path_sans_ext(GPSStep6Vec[i])
}

# Create list of files from step 6 directory
Step6Tracks <- lst()
for(i in seq_along(GPSStep6Vec)){
  Step6Tracks[[i]] <- read_csv(file = paste0(GPSStep6Path, GPSStep6Vec[i]))
}

# Add names to list to understand what tibbles are from which tracks
names(Step6Tracks) <- NameVec


## Create general function to adapt coordinates of a point in the dataset
ReplacePoint <- function(GPStrack, time, new_x, new_y){
  
  # Make shure the track is arranged on time
  UpdatedGPStrack <- GPStrack %>% 
    arrange(time)
  
  # Get row of the input time, the time where I want to adapt the coordinates
  RowNum <- which(
    UpdatedGPStrack$time == as.POSIXct(strptime(time, 
                                                format = "%Y-%m-%d %H:%M:%S", 
                                                tz = "GMT"))
    , arr.ind = TRUE)[1]
  
  # Replace current mu.x and mu.y with new, desired, coordinates
  UpdatedGPStrack[RowNum, ]$mu.x <- new_x
  UpdatedGPStrack[RowNum, ]$mu.y <- new_y
  
  # Return the updated GPS track
  Return(UpdatedGPStrack)
}






