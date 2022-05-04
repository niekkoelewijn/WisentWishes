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


### I didn't recieve a specific logbook of the management interventions regarding
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
endhab <- as.numeric(as.POSIXct(strptime("2020-09-10 12:00:00", format = "%Y-%m-%d %H:%M:%S")))

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









