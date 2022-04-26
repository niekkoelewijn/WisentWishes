# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to split up GPS datasets so that there does not exist a longer gap 
### than 10 hours between individual data points. 

## Get the GPS data from step 3

# Paths to GPS files step 3
setwd("~/WisentWishes")
GPSStep3Path <- "~/WisentWishes/MScThesisData/GPS location data/Step3Preprocess/"
GPSStep3Vec <- list.files(path = GPSStep3Path)

# Load GPS tables as tibbles
Caliope <- read_csv(paste0(GPSStep3Path, GPSStep3Vec[1]))
Delia <- read_csv(paste0(GPSStep3Path, GPSStep3Vec[2]))
Everest <- read_csv(paste0(GPSStep3Path, GPSStep3Vec[3]))
Kraansvlak <- read_csv(paste0(GPSStep3Path, GPSStep3Vec[4]))
Krayla <- read_csv(paste0(GPSStep3Path, GPSStep3Vec[5]))
Kroosja <- read_csv(paste0(GPSStep3Path, GPSStep3Vec[6]))
Maaike <- read_csv(paste0(GPSStep3Path, GPSStep3Vec[7]))
Nadia <- read_csv(paste0(GPSStep3Path, GPSStep3Vec[8]))
Nevaya <- read_csv(paste0(GPSStep3Path, GPSStep3Vec[9]))
Shara <- read_csv(paste0(GPSStep3Path, GPSStep3Vec[10]))
Veluwe <- read_csv(paste0(GPSStep3Path, GPSStep3Vec[11]))

# Make list of tibbles
TibbleList <- list(Everest, Caliope, Delia, Kraansvlak, 
                   Krayla, Kroosja, Maaike, Nadia, Nevaya,
                   Shara, Veluwe)

# Define function retrieve places in GPS dataset where tracks must be splitted
TemporalSplitter <- function(GPSfile){
  
  # Get places where tables must be split
  # 10 hours is 3600 * 10 = 36000 seconds
  BreakVec <- which(GPSfile$time_interval > 36000)
  
  # Create variable to store result of next step
  TrackList <- list()
  
  # Return input file if no rows have time_interval > 10 hours
  if(length(BreakVec) == 0){
    
    # Tracklist becomes equal to GPSfile
    TrackList[[1]] <- GPSfile
    
  }else if(length(BreakVec) == 1){
    
    # Assign values to elements
    TrackList[[1]] = GPSfile[1:BreakVec[1]-1, ]
    TrackList[[2]] = GPSfile[BreakVec[1]:nrow(GPSfile), ]
    
  }else{
    
    # Iterate over elements of the BreakVec 
    for(i in seq_along(BreakVec)+1){
      
      # Return input file if no rows have time_interval > 10 hours
      if(i != 1 & !is.na(BreakVec[i])){
        TrackList[[i]] = GPSfile[BreakVec[i-1]:BreakVec[i], ]
      }
      else{
        TrackList[[i]] <- GPSfile[BreakVec[i-1]:nrow(GPSfile), ]
      }
      
      # Remove row with large time interval from list
      TrackList[[i]] = slice(TrackList[[i]], 1:(n()-1))
    }
    
    # In loop, first element returns of TrackList returns NULL, but should be GPSfile[1:BreakVec[1], ]
    if(is.null(TrackList[[1]])){
      TrackList[[1]] = GPSfile[1:BreakVec[1], ]
      TrackList[[1]] = slice(TrackList[[1]], 1:(n()-1))
    }
  }

  # Filter out first 3 days to reduce disturbing impact of disturbing activities, 
  # such as replacement of GPS device
  
  # Create new empty list to store result of next step
  FilteredTrackList <- list()
  
  # Loop to filter list
  for(i in seq_along(TrackList)){
    
    # Use pipe to filter datasets in list
    FilteredTrackList[[i]] <- TrackList[[i]] %>% 
      
      # Filter out first element until first 3 days have past
      # 3 days is 3*24*3600 = 259200 seconds
      filter(!between(time_coded, time_coded[1], time_coded[1]+259200))
  }
  
  
  # Create new empty list to store result of next step
  FinalTrackList <- list()
  
  # Iterate over track list to exclude tracks with fewer than 100 points from analysis
  for(i in seq_along(FilteredTrackList)){
    if(nrow(FilteredTrackList[[i]]) > 100){
      FinalTrackList[i] <- FilteredTrackList[i]
    }
    else{
      next
    }
  }
  
  # Remove null values from list
  FinalTrackList <- FinalTrackList[!sapply(FinalTrackList, is.null)]
  
  # adapt speed_in, angle & time_interval of first element in each element of FinalTrack
  for(i in seq_along(FinalTrackList)){
    FinalTrackList[[i]]$speed_in[1] <- NA
    FinalTrackList[[i]]$angle[1] <- NA
    FinalTrackList[[i]]$time_interval[1] <- NA
  }
  
  return(FinalTrackList)

}

# Create list of seperate tracks for each GPS dataset
EverestTracks <- TemporalSplitter(TibbleList[[1]])
CaliopeTracks <- TemporalSplitter(TibbleList[[2]])
DeliaTracks <- TemporalSplitter(TibbleList[[3]])
KraansvlakTracks <- TemporalSplitter(TibbleList[[4]])
KraylaTracks <- TemporalSplitter(TibbleList[[5]])
KroosjaTracks <- TemporalSplitter(TibbleList[[6]])
MaaikeTracks <- TemporalSplitter(TibbleList[[7]])
NadiaTracks <- TemporalSplitter(TibbleList[[8]])
NevayaTracks <- TemporalSplitter(TibbleList[[9]])
SharaTracks <- TemporalSplitter(TibbleList[[10]])
VeluweTracks <- TemporalSplitter(TibbleList[[11]])

### Write the elements of the lists to files

# Create path
path <- "~/WisentWishes/MScThesisData/GPS location data/Step4Preprocess/"

# Create directory
if(!dir.exists(path)){
  dir.create(path)
}

## Write csv's

# Everest
for(i in seq_along(EverestTracks)){
  write_csv(EverestTracks[[i]], file = paste0(path, "EverestStep4Track", as.character(i), ".csv"))
} 

# Caliope
for(i in seq_along(CaliopeTracks)){
  write_csv(CaliopeTracks[[i]], file = paste0(path, "CaliopeStep4Track", as.character(i), ".csv"))
} 

# Delia
for(i in seq_along(DeliaTracks)){
  write_csv(DeliaTracks[[i]], file = paste0(path, "DeliaStep4Track", as.character(i), ".csv"))
} 

# Kraansvlak
for(i in seq_along(KraansvlakTracks)){
  write_csv(KraansvlakTracks[[i]], file = paste0(path, "KraansvlakStep4Track", as.character(i), ".csv"))
} 

# Krayla
for(i in seq_along(KraylaTracks)){
  write_csv(KraylaTracks[[i]], file = paste0(path, "KraylaStep4Track", as.character(i), ".csv"))
} 

# Kroosja
for(i in seq_along(KroosjaTracks)){
  write_csv(KroosjaTracks[[i]], file = paste0(path, "KroosjaStep4Track", as.character(i), ".csv"))
} 

# Maaike
for(i in seq_along(MaaikeTracks)){
  write_csv(MaaikeTracks[[i]], file = paste0(path, "MaaikeStep4Track", as.character(i), ".csv"))
} 

# Nadia
for(i in seq_along(NadiaTracks)){
  write_csv(NadiaTracks[[i]], file = paste0(path, "NadiaStep4Track", as.character(i), ".csv"))
} 

# Nevaya
for(i in seq_along(NevayaTracks)){
  write_csv(NevayaTracks[[i]], file = paste0(path, "NevayaStep4Track", as.character(i), ".csv"))
} 

# Shara
for(i in seq_along(SharaTracks)){
  write_csv(SharaTracks[[i]], file = paste0(path, "SharaStep4Track", as.character(i), ".csv"))
} 

# Veluwe
for(i in seq_along(VeluweTracks)){
  write_csv(VeluweTracks[[i]], file = paste0(path, "VeluweStep4Track", as.character(i), ".csv"))
} 




















