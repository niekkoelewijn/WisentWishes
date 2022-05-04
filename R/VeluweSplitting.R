# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to select and split the GPS tracks of the Veluwe based on the 
### activity logbook


## Get the Veluwe GPS data from step 4

# Paths to Veluwe tracks
setwd("~/WisentWishes")
GPSStep4bPath <- "~/WisentWishes/MScThesisData/GPS location data/Step4Preprocess/"
GPSStep4bVec <- list.files(path = GPSStep4Path, pattern = regex("^Veluwe"))
GPSStep4SharaVec <- list.files(path = GPSStep4Path, pattern = regex("^Shara"))

# Load GPS tracks of the Veluwe as tibbles
Track1 <- read_csv(paste0(GPSStep4bPath, GPSStep4bVec[1]))
Track10 <- read_csv(paste0(GPSStep4bPath, GPSStep4bVec[2]))
Track11 <- read_csv(paste0(GPSStep4bPath, GPSStep4bVec[3]))
Track12 <- read_csv(paste0(GPSStep4bPath, GPSStep4bVec[4]))
Track2 <- read_csv(paste0(GPSStep4bPath, GPSStep4bVec[5]))
Track3 <- read_csv(paste0(GPSStep4bPath, GPSStep4bVec[6]))
Track4 <- read_csv(paste0(GPSStep4bPath, GPSStep4bVec[7]))
Track5 <- read_csv(paste0(GPSStep4bPath, GPSStep4bVec[8]))
Track6 <- read_csv(paste0(GPSStep4bPath, GPSStep4bVec[9]))
Track7 <- read_csv(paste0(GPSStep4bPath, GPSStep4bVec[10]))
Track8 <- read_csv(paste0(GPSStep4bPath, GPSStep4bVec[11]))
Track9 <- read_csv(paste0(GPSStep4bPath, GPSStep4bVec[12]))
Track13 <- read_csv(paste0(GPSStep4bPath, GPSStep4SharaVec[1]))
Track14 <- read_csv(paste0(GPSStep4bPath, GPSStep4SharaVec[2]))

### Judging from the logbook of intervention actions, track 1 should be removed
### as the European bison was released in the Veluwe area in 12-04-2016,
### while the data points of track 1 are in the range from 21-02-2016 until 02-03-2016.
### Track 2 ranges from 15-04-2016 until 17-05-2017. Until 15-07-2016, the animals
### where confined to a smaller so called "habituate area", so for these dates, the
### data points should be spatially filtered for the habituate area. From 19-12-2016
### until 02-04-2017, the animals where supplementary fed, which heavily influenced
### the habitat selection, so I decided to make the points of those days a separate track. 
### Track 3 ranges from 21-05-2017 until 21-09-2017. The animals where confined to the
### habituate area from 24-06-2017 until 16-07-2017. No spacial management interventions
### took place in the date range of track 4 and 5. Track 6 ranged from 25-12-2017 until
### 31-12-2017. Track 7 ranged from 21-01-2018 until 03-02-2018. Supplementary feeding 
### in the winter of 17/18 ranged from 18-12-2017 until 20-04-2018. Track 6 and 7 overlapped
### completely with the supplementary feeding period. Track 8 ranged from 10-02-2018 
### until 06-02-2019. Habituate area was closed in this period from 07-02-2018 until
### 13-02-2018, so I will remove the first 3 days of track 8. Supplementary feeding from
### the start of the track until the 20-04-2018. Habituate area was closed from 10-09-2018
### until 27-12-2018. Supplementary feeding from 30-11-2018 until the end of track 8.
### No spacial management interventions took place in the date range of track 9, 10 and 11.
### Track 12 ranged from 17-02-2019 until 31-08-2020. Confinement to habituate area
### from 12-03-2020 until 20-03-2020. Supplementary feeding from 20-05-2020 until
### 20-07-2020 due to dryness. The last habituate area confinement occured until 
### the 5th of Februari 2022. The first track of Shara with the new collar starts
### the 8th of Februari 2022, so I don't have to correct for that in the tracks of
### Shara. Besides, the no supplementary feeding took place in the Shara tracks.


## Track 1 is skipped from the analysis


## Filtering of track 2

# Get numeric start and end date of habituate area confinement
starthab <- as.numeric(as.POSIXct(strptime("2016-04-12 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
endhab <- as.numeric(as.POSIXct(strptime("2016-06-15 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Get Veluwe habituate study area
HabituateArea <- TransformPolygon(VeluweHabituateArea)

# Create track of datapoints that fall in the range of dates of track 2 in which
# habituate area confinement took place 
Track1Habituate <- Track2 %>% 
  atl_filter_covariates(filters = c(
    "inrange(time_coded, starthab, endhab)"
  )
  ) %>% 
  atl_filter_bounds(x = "X", y = "Y", sf_polygon = HabituateArea, 
                    remove_inside = F) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()

# Get numeric start and end date of supplementary feeding
startsub <- as.numeric(as.POSIXct(strptime("2016-12-19 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
endsub <- as.numeric(as.POSIXct(strptime("2017-04-02 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Create track of datapoints that fall in the range of dates in which supplementary feeding took place
Track1SuppFed <- Track2 %>% 
  atl_filter_covariates(filters = c(
    "inrange(time_coded, startsub, endsub)"
  )
  ) %>%
  AddAttributes() %>% 
  TemporalSplitter2()

# Get tracks out of track 2 in which no management interventions took place
Track1Filtered <- Track2 %>% 
  atl_filter_covariates(filters = c(
    "!inrange(time_coded, starthab, endhab)",
    "!inrange(time_coded, startsub, endsub)"
  )
  ) %>%
  AddAttributes() %>% 
  TemporalSplitter2()


## Filtering of track 3

# Get numeric start and end date of habituate area confinement
starthab <- as.numeric(as.POSIXct(strptime("2017-06-24 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
endhab <- as.numeric(as.POSIXct(strptime("2017-07-16 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Create track of data points that fall in the range of dates in which
# habituate area confinement took place 
Track2Habituate <- Track3 %>% 
  atl_filter_covariates(filters = c(
    "inrange(time_coded, starthab, endhab)"
  )
  ) %>% 
  atl_filter_bounds(x = "X", y = "Y", sf_polygon = HabituateArea, 
                    remove_inside = F) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()

# Get tracks out of track 3 in which no management interventions took place
Track2Filtered <- Track3 %>% 
  atl_filter_covariates(filters = c(
    "!inrange(time_coded, starthab, endhab)"
  )
  ) %>%
  AddAttributes() %>% 
  TemporalSplitter2()


## Track 4 & 5 do not need extra filtering
Track3Filtered <- Track4
Track4Filtered <- Track5


## Filtering of track 6 and 7

# Track 6 and 7 overlap completely with a period of supplementary feeding
Track2SuppFed <- Track6
Track3SuppFed <- Track7


## Filtering of track 8

# Get numeric start and end date of supplementary feeding period 1 of track 8
startsub1 <- as.numeric(as.POSIXct(strptime("2018-02-10 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
endsub1 <- as.numeric(as.POSIXct(strptime("2018-04-20 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Get numeric start and end date of habituate area confinement period 1 of track 8
starthab1 <- as.numeric(as.POSIXct(strptime("2018-02-10 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
endhab1 <- as.numeric(as.POSIXct(strptime("2018-02-13 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Create track of datapoints that fall in the range of dates in which supplementary 
# feeding took place. Remove first 3 days out of the track, as during these days, 
# the bisons where confinned to the habituate area, a too short period to create a seperate track.
Track4SuppFed <- Track8 %>% 
  atl_filter_covariates(filters = c(
    "!inrange(time_coded, starthab1, endhab1)",
    "inrange(time_coded, startsub1, endsub1)"
  )
  ) %>%
  AddAttributes() %>% 
  TemporalSplitter2()

# Get numeric start and end date of supplementary feeding period 2 of track 8
startsub2 <- as.numeric(as.POSIXct(strptime("2018-10-30 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
endsub2 <- as.numeric(as.POSIXct(strptime("2019-02-7 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Get numeric start and end date of habituate area confinement period 2 of track 8 
# in which NO supplementary feeding took place
starthab2 <- as.numeric(as.POSIXct(strptime("2018-09-10 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
endhab2 <- as.numeric(as.POSIXct(strptime("2018-11-30 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Create track of data points that fall in the range of dates in which habituate 
# area confinement took place, and no supplementary feeding took place
Track3Habituate <- Track8 %>% 
  atl_filter_covariates(filters = c(
    "inrange(time_coded, starthab2, endhab2)"
  )
  ) %>% 
  atl_filter_bounds(x = "X", y = "Y", sf_polygon = HabituateArea, 
                    remove_inside = F) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()

# Get numeric start and end date of habituate area confinement in which 
# suppelementary feeding took place
starthabsub <- as.numeric(as.POSIXct(strptime("2018-11-30 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
endhabsub <- as.numeric(as.POSIXct(strptime("2018-12-27 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Create track of datapoints that fall in the range of dates in which supplementary feeding took place
# during the period of habituate area confinement
Track1HabSubb <- Track8 %>% 
  atl_filter_covariates(filters = c(
    "inrange(time_coded, starthabsub, endhabsub)"
  )
  ) %>%
  atl_filter_bounds(x = "X", y = "Y", sf_polygon = HabituateArea, 
                    remove_inside = F) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()

# Get tracks out of track 8 in which no management interventions took place
Track5Filtered <- Track8 %>% 
  atl_filter_covariates(filters = c(
    "!inrange(time_coded, startsub1, endsub1)",
    "!inrange(time_coded, startsub2, endsub2)",
    "!inrange(time_coded, starthab2, endhab2)"
  )
  ) %>%
  AddAttributes() %>% 
  TemporalSplitter2()


## Track 9, 10 & 11 do not need extra filtering
Track6Filtered <- Track9
Track7Filtered <- Track10
Track8Filtered <- Track11


## Filtering of track 12 

# Get numeric start and end date of habituate area confinement of track 12
starthab <- as.numeric(as.POSIXct(strptime("2020-03-12 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
endhab <- as.numeric(as.POSIXct(strptime("2020-03-20 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Create track of data points that fall in the range of dates in which habituate 
# area confinement took place
Track4Habituate <- Track12 %>% 
  atl_filter_covariates(filters = c(
    "inrange(time_coded, starthab, endhab)"
  )
  ) %>% 
  atl_filter_bounds(x = "X", y = "Y", sf_polygon = HabituateArea, 
                    remove_inside = F) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()

# Get numeric start and end date of supplementary feeding of track 12
startsub <- as.numeric(as.POSIXct(strptime("2020-05-20 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
endsub <- as.numeric(as.POSIXct(strptime("2020-07-30 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Create track of data points that fall in the range of dates in which supplementary feeding took place
Track5SuppFed <- Track12 %>% 
  atl_filter_covariates(filters = c(
    "inrange(time_coded, startsub, endsub)"
  )
  ) %>%
  AddAttributes() %>% 
  TemporalSplitter2()

# Get tracks out of track 12 in which no management interventions took place
Track9Filtered <- Track12 %>% 
  atl_filter_covariates(filters = c(
    "!inrange(time_coded, startsub, endsub)",
    "!inrange(time_coded, starthab, endhab)"
  )
  ) %>%
  AddAttributes() %>% 
  TemporalSplitter2()

## Track 13 & 14 do not need extra filtering
Track10Filtered <- Track13
Track11Filtered <- Track14

## Create list of filtered Veluwe tracks
VeluweFilteredTracks <- lst()
VeluweFilteredTracks <- append(VeluweFilteredTracks, Track1Filtered)
VeluweFilteredTracks <- append(VeluweFilteredTracks, Track2Filtered)
VeluweFilteredTracks[[5]] <- Track3Filtered
VeluweFilteredTracks[[6]] <- Track4Filtered
VeluweFilteredTracks <- append(VeluweFilteredTracks, Track5Filtered)
VeluweFilteredTracks[[8]] <- Track6Filtered
VeluweFilteredTracks[[9]] <- Track7Filtered
VeluweFilteredTracks[[10]] <- Track8Filtered
VeluweFilteredTracks <- append(VeluweFilteredTracks, Track9Filtered)
VeluweFilteredTracks[[14]] <- Track10Filtered
VeluweFilteredTracks[[15]] <- Track11Filtered


## Create list of Veluwe tracks where supplementary feeding took place
VeluweSuppTracks <- lst()
VeluweSuppTracks <- append(VeluweSuppTracks, Track1SuppFed)
VeluweSuppTracks[[2]] <- Track2SuppFed
VeluweSuppTracks[[3]] <- Track3SuppFed
VeluweSuppTracks <- append(VeluweSuppTracks, Track4SuppFed)
VeluweSuppTracks <- append(VeluweSuppTracks, Track5SuppFed)


## Create list of Veluwe tracks where habituate area confinement took place
VeluweHabTracks <- lst()
VeluweHabTracks <- append(VeluweHabTracks, Track1Habituate)
VeluweHabTracks <- append(VeluweHabTracks, Track2Habituate)
VeluweHabTracks <- append(VeluweHabTracks, Track3Habituate)
VeluweHabTracks <- append(VeluweHabTracks, Track4Habituate)

## Create list of Veluwe tracks where habituate area confinement took place
VeluweSuppHabTracks <- lst()
VeluweSuppHabTracks <- append(VeluweSuppHabTracks, Track1HabSubb)

## Write the elements of the lists to files

# Create path
path <- "~/WisentWishes/MScThesisData/GPS location data/Step5Preprocess/"

# Tracks where no management interventions took place
for(i in seq_along(VeluweFilteredTracks)){
  write_csv(VeluweFilteredTracks[[i]], file = paste0(path, "VeluweTrack", as.character(i), ".csv"))
} 

# Tracks where supplementary feeding took place
for(i in seq_along(VeluweSuppTracks)){
  write_csv(VeluweSuppTracks[[i]], file = paste0(path, "VeluweSuppFedTrack", as.character(i), ".csv"))
} 

# Tracks where habituate area confinement took place
for(i in seq_along(VeluweHabTracks)){
  write_csv(VeluweHabTracks[[i]], file = paste0(path, "VeluweHabTrack", as.character(i), ".csv"))
} 

# Tracks where both supplementary feeding and habituate area confinement took place
for(i in seq_along(VeluweSuppHabTracks)){
  write_csv(VeluweSuppHabTracks[[i]], file = paste0(path, "VeluweSuppFedHabTrack", as.character(i), ".csv"))
} 
























