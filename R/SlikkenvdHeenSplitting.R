# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to select and split the GPS tracks of the Slikken vd Heen based on the 
### activity logbook

## Get the Slikken vd Heen GPS data from step 4

# Paths to Slikken vd Heen tracks
setwd("~/WisentWishes")
GPSStep4Path <- "~/WisentWishes/MScThesisData/GPS location data/Step4Preprocess/"
GPSStep4CaliopeVec <- list.files(path = GPSStep4Path, pattern = regex("^Caliope"))
GPSStep4NadiaVec <- list.files(path = GPSStep4Path, pattern = regex("^Nadia"))

# Load GPS tracks of the Slikken vd Heen as tibbles
Track1 <- read_csv(paste0(GPSStep4Path, GPSStep4CaliopeVec[1]))
Track2 <- read_csv(paste0(GPSStep4Path, GPSStep4CaliopeVec[2]))
Track3 <- read_csv(paste0(GPSStep4Path, GPSStep4NadiaVec[1]))
Track4 <- read_csv(paste0(GPSStep4Path, GPSStep4NadiaVec[2]))
Track5 <- read_csv(paste0(GPSStep4Path, GPSStep4NadiaVec[3]))


### I didn't receive a specific logbook of the management interventions regarding
### the bison population at the Slikken vd Heen. I found out in a news paper
### that the population was released in the area the 17th of februari 2020
### and that the population was confined to a habituate area from the start
### until September. From the tracks of the two individuals I could find out 
### that the whole study area became available starting from the first of september
### 2020. Both Caliope's and Nadia's first track felled completely within the 
### range of the habituate area confinement. The second of both individuals 
### felled partly in this time of habituate area confinement, and partly not,
### so these tracks should be splitted.


## Filtering of track 1

# Get numeric start and end date of habituate area confinement
starthab <- as.numeric(as.POSIXct(strptime("2020-02-17 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
endhab <- as.numeric(as.POSIXct(strptime("2020-09-01 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Get Slikken vd Heen habituate study area
HabituateArea <- TransformPolygon(SlikkenvdHeenHabituateArea)

# Create track of data points that fall in the range of dates in which
# habituate area confinement took place
Track1Habituate <- Track1 %>% 
  atl_filter_covariates(filters = c(
    "inrange(time_coded, starthab, endhab)"
  )
  ) %>% 
  atl_filter_bounds(x = "X", y = "Y", sf_polygon = HabituateArea, 
                    remove_inside = F) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()


## Filtering of track 2

# Create track of data points that fall in the range of dates in which
# habituate area confinement took place
Track2Habituate <- Track2 %>% 
  atl_filter_covariates(filters = c(
    "inrange(time_coded, starthab, endhab)"
  )
  ) %>% 
  atl_filter_bounds(x = "X", y = "Y", sf_polygon = HabituateArea, 
                    remove_inside = F) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()

# Get points from track where no management interventions took place
Track1Filtered <- Track2 %>% 
  atl_filter_covariates(filters = c(
    "!inrange(time_coded, starthab, endhab)"
  )
  ) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()


## Filtering of track 3

# Create track of data points that fall in the range of dates in which
# habituate area confinement took place
Track3Habituate <- Track3 %>% 
  atl_filter_covariates(filters = c(
    "inrange(time_coded, starthab, endhab)"
  )
  ) %>% 
  atl_filter_bounds(x = "X", y = "Y", sf_polygon = HabituateArea, 
                    remove_inside = F) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()


## Filtering of track 4

# Create track of data points that fall in the range of dates in which
# habituate area confinement took place
Track4Habituate <- Track4 %>% 
  atl_filter_covariates(filters = c(
    "inrange(time_coded, starthab, endhab)"
  )
  ) %>% 
  atl_filter_bounds(x = "X", y = "Y", sf_polygon = HabituateArea, 
                    remove_inside = F) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()

# Get points from track where no management interventions took place
Track2Filtered <- Track4 %>% 
  atl_filter_covariates(filters = c(
    "!inrange(time_coded, starthab, endhab)"
  )
  ) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()


## No filtering needed for track 5
Track3Filtered <- Track5


## Create list of filtered Slikken vd Heen tracks
SlikkenvdHeenFilteredTracks <- lst()
SlikkenvdHeenFilteredTracks <- append(SlikkenvdHeenFilteredTracks, Track1Filtered)
SlikkenvdHeenFilteredTracks <- append(SlikkenvdHeenFilteredTracks, Track2Filtered)
SlikkenvdHeenFilteredTracks[[3]] <- Track3Filtered


## Create list of Slikken vd Heen tracks where habituate area confinement took place
SlikkenvdHeenHabTracks <- lst()
SlikkenvdHeenHabTracks <- append(SlikkenvdHeenHabTracks, Track1Habituate)
SlikkenvdHeenHabTracks <- append(SlikkenvdHeenHabTracks, Track2Habituate)
SlikkenvdHeenHabTracks <- append(SlikkenvdHeenHabTracks, Track3Habituate)
SlikkenvdHeenHabTracks <- append(SlikkenvdHeenHabTracks, Track4Habituate)


## Write the elements of the lists to files

# Create path
path <- "~/WisentWishes/MScThesisData/GPS location data/Step5Preprocess/"

# Tracks where no management interventions took place
for(i in seq_along(SlikkenvdHeenFilteredTracks)){
  write_csv(SlikkenvdHeenFilteredTracks[[i]], file = paste0(path, "SlikkenvdHeenTrack", as.character(i), ".csv"))
} 

# Tracks where habituate area confinement took place
for(i in seq_along(SlikkenvdHeenHabTracks)){
  write_csv(SlikkenvdHeenHabTracks[[i]], file = paste0(path, "SlikkenvdHeenHabTrack", as.character(i), ".csv"))
} 








