# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to select and split the GPS tracks of the Maashorst based on the 
### activity logbook

## Get the Maashorst GPS data from step 4

# Paths to Maashorst tracks
setwd("~/WisentWishes")
GPSStep4Path <- "~/WisentWishes/MScThesisData/GPS location data/Step4Preprocess/"
GPSStep4KraylaVec <- list.files(path = GPSStep4Path, pattern = regex("^Krayla"))
GPSStep4KroosjaVec <- list.files(path = GPSStep4Path, pattern = regex("^Kroosja"))
GPSStep4DeliaVec <- list.files(path = GPSStep4Path, pattern = regex("^Delia"))
GPSStep4MaaikeVec <- list.files(path = GPSStep4Path, pattern = regex("^Maaike"))
GPSStep4NevayaVec <- list.files(path = GPSStep4Path, pattern = regex("^Nevaya"))
GPSStep4EverestVec <- list.files(path = GPSStep4Path, pattern = regex("^Everest"))

# Load GPS tracks of the Slikken vd Heen as tibbles
Track1 <- read_csv(paste0(GPSStep4Path, GPSStep4KraylaVec[1]))                  # 2016-03-11 until 2016-03-31
Track2 <- read_csv(paste0(GPSStep4Path, GPSStep4KroosjaVec[1]))                 # 2016-12-04 until 2017-06-01
Track3 <- read_csv(paste0(GPSStep4Path, GPSStep4KroosjaVec[2]))                 # 2018-02-11 until 2018-08-16
Track4 <- read_csv(paste0(GPSStep4Path, GPSStep4DeliaVec[1]))                   # 2016-12-04 until 2016-12-11
Track5 <- read_csv(paste0(GPSStep4Path, GPSStep4MaaikeVec[1]))                  # 2019-03-18 until 2020-03-27
Track6 <- read_csv(paste0(GPSStep4Path, GPSStep4MaaikeVec[2]))                  # 2021-02-26 until 2022-01-27
Track7 <- read_csv(paste0(GPSStep4Path, GPSStep4NevayaVec[1]))                  # 2022-01-30 until 2022-04-03
Track8 <- read_csv(paste0(GPSStep4Path, GPSStep4EverestVec[1]))                 # 2022-02-26 until 2022-03-08


## The management of the bison population of the Maashorst resambles the way
## they are managed in the Kraansvlak; like in the Kraansvlak, each year the 
## animals of the Maashorst are lured to a specific place in the study area to 
## have a medical check-up and to undergo medical interventions. The dates of
## when these management interventions took place are known for 2021 and 2022;
## respectively between 2021-01-20 and 2021-02-24, and between 2022-01-08 and
## 2022-01-27 For 2020, I received a indication that the interventions took
## place in February and March, judging from the GPS track of Maaike, I 
## concluded that the dates should be between 2020-02-27 and 2020-03-20 The
## management intervention of 2019 did probably not overlap with GPS tracks that
## I analyze in this thesis research, as the track of Maaike starts at 
## 2019-03-18, and there were no individuals tracked between 2018-08-16 and 
## 2019-03-18. By having a close look at the distribution of GPS points, I 
## assume that the management interventions of 2017 and 2018 took place 
## respectively between 2017-01-10 and 2017-04-01, and between 2018-02-01 and
## 2018-04-01 Supplementary feeding of the animals in the Maashorst only takes
## place between the mentioned dates with management interventions, and only
## within the luring area. I decided to exclude the GPS points that fall within
## the ranges of dates with management intervention from the habitat selection
## analysis.


## No filtering needed for track 1 and 4
Track1Filtered <- Track1
Track2Filtered <- Track4


## Filtering of track 2

# Get numeric start and end date of management intervention
start <- as.numeric(as.POSIXct(strptime("2017-01-10 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
end <- as.numeric(as.POSIXct(strptime("2017-04-01 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Get Maashorst study area 2017-2021
StudyArea <- TransformPolygon(MaashorstStudyArea20172021)

# Get points from track where no management interventions took place
Track3Filtered <- Track2 %>% 
  atl_filter_covariates(filters = c(
    "!inrange(time_coded, start, end)"
  )
  ) %>%
  atl_filter_bounds(x = "X", y = "Y", sf_polygon = StudyArea, 
                   remove_inside = F) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()


## Filtering of track 3

# Get numeric start and end date of management intervention
start <- as.numeric(as.POSIXct(strptime("2018-02-11 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
end <- as.numeric(as.POSIXct(strptime("2018-04-01 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Get points from track where no management interventions took place
Track4Filtered <- Track3 %>% 
  atl_filter_covariates(filters = c(
    "!inrange(time_coded, start, end)"
  )
  ) %>% 
  atl_filter_bounds(x = "X", y = "Y", sf_polygon = StudyArea, 
                    remove_inside = F) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()


## Filtering of track 5

# Get numeric start and end date of management intervention
start <- as.numeric(as.POSIXct(strptime("2020-02-27 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
end <- as.numeric(as.POSIXct(strptime("2020-03-20 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Get points from track where no management interventions took place
Track5Filtered <- Track5 %>% 
  atl_filter_covariates(filters = c(
    "!inrange(time_coded, start, end)"
  )
  ) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()


## Filtering of track 6

# Get numeric start and end date of management intervention
start <- as.numeric(as.POSIXct(strptime("2022-01-08 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
end <- as.numeric(as.POSIXct(strptime("2022-01-27 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Get points from track where no management interventions took place
Track6Filtered <- Track6 %>% 
  atl_filter_covariates(filters = c(
    "!inrange(time_coded, start, end)"
  )
  ) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()


## No filtering needed for track 7 and 8
Track7Filtered <- Track7
Track8Filtered <- Track8


## Create list of filtered Maashorst tracks
MaashorstFilteredTracks <- lst()
MaashorstFilteredTracks[[1]] <- Track1Filtered
MaashorstFilteredTracks[[2]] <- Track2Filtered
MaashorstFilteredTracks <- append(MaashorstFilteredTracks, Track3Filtered)
MaashorstFilteredTracks <- append(MaashorstFilteredTracks, Track4Filtered)
MaashorstFilteredTracks <- append(MaashorstFilteredTracks, Track5Filtered)
MaashorstFilteredTracks <- append(MaashorstFilteredTracks, Track6Filtered)
MaashorstFilteredTracks[[10]] <- Track7Filtered
MaashorstFilteredTracks[[11]] <- Track8Filtered


## Write the elements of the lists to files

# Create path
path <- "~/WisentWishes/MScThesisData/GPS location data/Step5Preprocess/"

# Tracks where no management interventions took place
for(i in seq_along(MaashorstFilteredTracks)){
  write_csv(MaashorstFilteredTracks[[i]], file = paste0(path, "MaashorstTrack", as.character(i), ".csv"))
} 










