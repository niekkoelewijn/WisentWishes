# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022


### Script to filter out unrealistic speeds and turning angles to filter out unrealistic movement / spikes

## Get the GPS data from step 2

# Paths to GPS files step 2
setwd("~/WisentWishes")
GPSStep2Path <- "~/WisentWishes/MScThesisData/GPS location data/Step2Preprocess/"
GPSStep2Vec <- list.files(path = GPSStep2Path)

# Load GPS tables as tibbles
Caliope <- read_csv(paste0(GPSStep2Path, GPSStep2Vec[1]))
Delia <- read_csv(paste0(GPSStep2Path, GPSStep2Vec[2]))
Everest <- read_csv(paste0(GPSStep2Path, GPSStep2Vec[3]))
Kraansvlak <- read_csv(paste0(GPSStep2Path, GPSStep2Vec[4]))
Krayla <- read_csv(paste0(GPSStep2Path, GPSStep2Vec[5]))
Kroosja <- read_csv(paste0(GPSStep2Path, GPSStep2Vec[6]))
Maaike <- read_csv(paste0(GPSStep2Path, GPSStep2Vec[7]))
Nadia <- read_csv(paste0(GPSStep2Path, GPSStep2Vec[8]))
Nevaya <- read_csv(paste0(GPSStep2Path, GPSStep2Vec[9]))
Shara <- read_csv(paste0(GPSStep2Path, GPSStep2Vec[10]))
Veluwe <- read_csv(paste0(GPSStep2Path, GPSStep2Vec[11]))

# Make list of tibbles
TibbleList <- list(Everest, Caliope, Delia, Kraansvlak, 
                   Krayla, Kroosja, Maaike, Nadia, Nevaya,
                   Shara, Veluwe)

# Define function to add speed, angle, and time interval attibutes to GPS tables
AddAttributes <- function(GPSfile){
  
  # Filter GPS files in piped structure
  FilteredGPSfile <- GPSfile %>% 
    
    # Filter out points that are on the exact same location
    filter(X != lag(X) & Y != lag(Y))
  
  # Edit GPS files in piped structure
  ExpandedGPSfile <- FilteredGPSfile %>% 
    
    # Add attribute for speed in
    mutate(speed_in = atl_get_speed(data = FilteredGPSfile, x = "X", y = "Y",
                                    time = "time_coded", type = "in")) %>% 
    
    # Add attribute for speed out
    mutate(speed_out = atl_get_speed(data = FilteredGPSfile, x = "X", y = "Y",
                                    time = "time_coded", type = "out")) %>%
    
    # Add attribute for turning angle
    mutate(angle = atl_turning_angle(data = FilteredGPSfile, x = "X", y = "Y",
                                     time = "time_coded")) %>% 
    
    # Check out points that are on the exact same location
    filter(speed_out > 0) %>% 
    
    # Add attribute for time interval
    mutate(time_interval = time_coded - lag(time_coded))
  
  return(as_tibble(ExpandedGPSfile))
  
}


# Apply defined function to elements in list
SpeedAngle <- lapply(TibbleList, AddAttributes)

## Write the elements of the list to a file

# Create path
path <- "~/WisentWishes/MScThesisData/GPS location data/Step3Preprocess/"

# Create directory
if(!dir.exists(path)){
  dir.create(path)
}

# Create vector of file names
NameVec <- c("EverestStep3", "CaliopeStep3", "DeliaStep3", "KraansvlakStep3", 
             "KraylaStep3", "KroosjaStep3", "MaaikeStep3", "NadiaStep3", "NevayaStep3",
             "SharaStep3", "VeluweStep3")

# Write csv's
for(i in seq_along(SpeedAngle)){
  write_csv(SpeedAngle[[i]], file = paste0(path, NameVec[i], ".csv"))
} 




































