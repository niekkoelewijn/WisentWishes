# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to select and split the GPS tracks of the Veluwe based on the 
### activity logbook


## Get the Kraansvlak GPS data from step 4

# Paths to Kraansvlak tracks
setwd("~/WisentWishes")
GPSStep4bPath <- "~/WisentWishes/MScThesisData/GPS location data/Step4Preprocess/"
GPSStep4bVec <- list.files(path = GPSStep4Path, pattern = regex("^Veluwe"))

# Load GPS tracks of the Kraansvlak as tibbles
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
### 20-07-2020 due to dryness.

## Filtering of track 2

# Get numeric start and end date of habituate area confinement
start <- as.numeric(as.POSIXct(strptime("2016-04-12 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
end <- as.numeric(as.POSIXct(strptime("2016-06-15 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Get Veluwe habituate study area
HabituateArea <- TransformPolygon(VeluweHabituateArea)

# First track of the temporally filtered tracks of the Veluwe 
Track1Habituate <- Track2 %>% 
  atl_filter_covariates(filters = c(
    "inrange(time_coded, start, end)"
  )
  ) %>% 
  atl_filter_bounds(x = "X", y = "Y", sf_polygon = HabituateArea, 
                    remove_inside = F) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()


# Get numeric start and end date of days without interventions
start <- as.numeric(as.POSIXct(strptime("2016-06-15 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
end <- as.numeric(as.POSIXct(strptime("2017-12-19 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# First track of the temporally filtered tracks of the Veluwe 
Track1Filtered <- Track2 %>% 
  atl_filter_covariates(filters = c(
    "inrange(time_coded, start, end)"
  )
  ) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()


# Get numeric start and end date of supplementary feeding
start <- as.numeric(as.POSIXct(strptime("2016-12-19 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
end <- as.numeric(as.POSIXct(strptime("2017-04-02 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

# Third track of the temporally filtered tracks of the Veluwe WITH supplementary feeding
Track1SuppFed <- Track2 %>% 
  atl_filter_covariates(filters = c(
    "inrange(time_coded, start, end)"
  )
  ) %>%
  AddAttributes() %>% 
  TemporalSplitter2()

# Get numeric start and end date of days without interventions
start <- as.numeric(as.POSIXct(strptime("2017-04-02 12:00:00", format = "%Y-%m-%d %H:%M:%S")))
end <- as.numeric(as.POSIXct(strptime("2017-05-17 23:00:00", format = "%Y-%m-%d %H:%M:%S")))

# First track of the temporally filtered tracks of the Veluwe 
Track2Filtered <- Track2 %>% 
  atl_filter_covariates(filters = c(
    "inrange(time_coded, start, end)"
  )
  ) %>% 
  AddAttributes() %>% 
  TemporalSplitter2()


## Filtering of track 3













