# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to set up SSF for all study areas and all times

# Test with 1 track
KraansvlakTrack1 <- AllTrackPoints[which(AllTrackPoints$track_ID == 1),]

KraansvlakTrack1xyt <- make_track(KraansvlakTrack1, .x = "X", .y = "Y", .t = "time",
                                  crs = 28992, all_cols = T)

KraansvlakTrack1yxtssfdat <- KraansvlakTrack1xyt %>% 
  track_resample(rate = hours(1), tolerance = seconds(10)) %>% 
  steps_by_burst(lonlat = F, keep_cols = "start" ) %>%
  random_steps() 

# Make sure you have rasters of distance to forest, road and water per study area
# and the landuse class codes. This needs to be joined to the LUTLanduseClasses

# Create lists distances
WaterDistanceList <- list()
ForestDistanceList <- list()
RoadDistanceList <- list()

# Iterate over elements of MaskedList to create distance raster for water,
# forest and roads. 
for(i in seq_along(MaskedList)){
  
  # Select water values 
  Water <- MaskedList[[i]] == 5
  
  # Put non-water values to NA
  Water[Water < 1] <- NA
  
  # Create water distance raster
  WaterDistanceList[[i]] <- raster::distance(Water)
  
  # Select forest values 
  Forest <- MaskedList[[i]] %in% c(3, 4)
  
  # Put non-forest values to NA
  Forest[Forest < 1] <- NA
  
  # Create forest distance raster
  ForestDistanceList[[i]] <- raster::distance(Forest)
  
  # Select road values 
  Road <- MaskedList[[i]] == 8
  
  # Put non-road values to NA
  Road[Road < 1] <- NA
  
  # Create road distance raster
  RoadDistanceList[[i]] <- raster::distance(Road)
  
}

# Name distance lists
names(WaterDistanceList) <- c("Kraansvlak", "Maashorst2016", "Maashorst20172021",
                       "Maashorst2022", "SlikkenvdHeenHabituate",
                       "SlikkenvdHeen", "VeluweHabituate", "Veluwe")
names(ForestDistanceList) <- c("Kraansvlak", "Maashorst2016", "Maashorst20172021",
                       "Maashorst2022", "SlikkenvdHeenHabituate",
                       "SlikkenvdHeen", "VeluweHabituate", "Veluwe")
names(RoadDistanceList) <- c("Kraansvlak", "Maashorst2016", "Maashorst20172021",
                       "Maashorst2022", "SlikkenvdHeenHabituate",
                       "SlikkenvdHeen", "VeluweHabituate", "Veluwe")

## Create SSF with multiple animals

# Make nested dataset out of AllTrackPoints
NestedTrackPoints <- AllTrackPoints %>% 
  nest(-track_ID) %>% 
  mutate(track = map(data, ~ mk_track(., X, Y, time, crs = 28992, all_cols = T)))

# Add step lenght as coumn
step_lengths <- unlist(map(NestedTrackPoints$track, step_lengths))

# View step lengths
hist(step_lengths, breaks = 20, main="Histogram of step lengths", xlab="Step lengths")

# Create true steps
true_steps <- map(NestedTrackPoints$track, steps, keep_cols = 'start')

# Create random steps
true_random_steps <- map(true_steps, random_steps, n=10)

# Create 1 dataframe for all individuals
all_steps <- bind_rows(true_random_steps, .id="track_id")
all_steps
with(all_steps, hist(ta_[case_==T], breaks=180, main = "Histogram of turning angles", xlab="turning angle"))

# Get back to projected data form
steps_sp <- all_steps
coordinates(steps_sp) <- ~x2_+y2_
proj4string(steps_sp) <- CRS(proj4string(as_Spatial(KraansvlakStudyAreaRDNew)))


## Extract habitat covariate data

# Kraansvlak
LanduseKraansvlak <- raster::extract(MaskedList$Kraansvlak, steps_sp[which(steps_sp$track_id %in% c(1:4)),])


KraansvlakSteps <- steps_sp[which(steps_sp$track_id %in% c(1:4)),]
KraansvlakSteps$landuse_code <- LanduseKraansvlak
KraansvlakStepsLanduseClass <- inner_join(as_tibble(KraansvlakSteps), LUTLanduseClasses, by = "landuse_code")
KraansvlakSteps <- sp.na.omit(KraansvlakSteps, col.name = "landuse_code") 
KraansvlakSteps$landuse_class <-  KraansvlakStepsLanduseClass$landuse_class.y

WaterDistKraansvlak <- raster::extract(WaterDistanceList[[1]], KraansvlakSteps)
ForestDistKraansvlak <- raster::extract(ForestDistanceList[[1]], KraansvlakSteps)
RoadDistKraansvlak <- raster::extract(RoadDistanceList[[1]], KraansvlakSteps)

KraansvlakSteps$WaterDistance <- WaterDistKraansvlak
KraansvlakSteps$ForestDistance <- ForestDistKraansvlak
KraansvlakSteps$RoadDistance <- RoadDistKraansvlak

KraansvlakStepsTibble <- as_tibble(KraansvlakSteps)

# Maashorst 2016
LanduseMaashorst2016 <- raster::extract(MaskedList$Maashorst2016, steps_sp[which(steps_sp$track_id %in% c(5, 8)),])


Maashorst2016Steps <- steps_sp[which(steps_sp$track_id %in% c(5, 8)),]
Maashorst2016Steps$landuse_code <- LanduseMaashorst2016
Maashorst2016StepsLanduseClass <- inner_join(as_tibble(Maashorst2016Steps), LUTLanduseClasses, by = "landuse_code")
Maashorst2016Steps <- sp.na.omit(Maashorst2016Steps, col.name = "landuse_code") 
Maashorst2016Steps$landuse_class <-  Maashorst2016StepsLanduseClass$landuse_class.y

WaterDistMaashorst2016 <- raster::extract(WaterDistanceList[[2]], Maashorst2016Steps)
ForestDistMaashorst2016 <- raster::extract(ForestDistanceList[[2]], Maashorst2016Steps)
RoadDistMaashorst2016 <- raster::extract(RoadDistanceList[[2]], Maashorst2016Steps)

Maashorst2016Steps$WaterDistance <- WaterDistMaashorst2016
Maashorst2016Steps$ForestDistance <- ForestDistMaashorst2016
Maashorst2016Steps$RoadDistance <- RoadDistMaashorst2016

Maashorst2016StepsTibble <- as_tibble(Maashorst2016Steps)

# Maashorst 2017 - 2021
LanduseMaashorst20172021 <- raster::extract(MaskedList$Maashorst20172021, steps_sp[which(steps_sp$track_id %in% c(7, 9:15)),])


Maashorst20172021Steps <- steps_sp[which(steps_sp$track_id %in% c(7, 9:15)),]
Maashorst20172021Steps$landuse_code <- LanduseMaashorst20172021
Maashorst20172021StepsLanduseClass <- inner_join(as_tibble(Maashorst20172021Steps), LUTLanduseClasses, by = "landuse_code")
Maashorst20172021Steps <- sp.na.omit(Maashorst20172021Steps, col.name = "landuse_code") 
Maashorst20172021Steps$landuse_class <-  Maashorst20172021StepsLanduseClass$landuse_class.y

WaterDistMaashorst20172021 <- raster::extract(WaterDistanceList[[3]], Maashorst20172021Steps)
ForestDistMaashorst20172021 <- raster::extract(ForestDistanceList[[3]], Maashorst20172021Steps)
RoadDistMaashorst20172021 <- raster::extract(RoadDistanceList[[3]], Maashorst20172021Steps)

Maashorst20172021Steps$WaterDistance <- WaterDistMaashorst20172021
Maashorst20172021Steps$ForestDistance <- ForestDistMaashorst20172021
Maashorst20172021Steps$RoadDistance <- RoadDistMaashorst20172021

Maashorst20172021StepsTibble <- as_tibble(Maashorst20172021Steps)

# Maashorst 2022
LanduseMaashorst2022 <- raster::extract(MaskedList$Maashorst2022, steps_sp[which(steps_sp$track_id %in% c(6)),])


Maashorst2022Steps <- steps_sp[which(steps_sp$track_id %in% c(6)),]
Maashorst2022Steps$landuse_code <- LanduseMaashorst2022
Maashorst2022StepsLanduseClass <- inner_join(as_tibble(Maashorst2022Steps), LUTLanduseClasses, by = "landuse_code")
Maashorst2022Steps <- sp.na.omit(Maashorst2022Steps, col.name = "landuse_code") 
Maashorst2022Steps$landuse_class <-  Maashorst2022StepsLanduseClass$landuse_class.y

WaterDistMaashorst2022 <- raster::extract(WaterDistanceList[[4]], Maashorst2022Steps)
ForestDistMaashorst2022 <- raster::extract(ForestDistanceList[[4]], Maashorst2022Steps)
RoadDistMaashorst2022 <- raster::extract(RoadDistanceList[[4]], Maashorst2022Steps)

Maashorst2022Steps$WaterDistance <- WaterDistMaashorst2022
Maashorst2022Steps$ForestDistance <- ForestDistMaashorst2022
Maashorst2022Steps$RoadDistance <- RoadDistMaashorst2022

Maashorst2022StepsTibble <- as_tibble(Maashorst2022Steps)

# Slikken vd Heen habituate area
LanduseSlikkenvdHeenHabituate <- raster::extract(MaskedList$SlikkenvdHeenHabituate, steps_sp[which(steps_sp$track_id %in% c(16:19)),])


SlikkenvdHeenHabituateSteps <- steps_sp[which(steps_sp$track_id %in% c(16:19)),]
SlikkenvdHeenHabituateSteps$landuse_code <- LanduseSlikkenvdHeenHabituate
SlikkenvdHeenHabituateStepsLanduseClass <- inner_join(as_tibble(SlikkenvdHeenHabituateSteps), LUTLanduseClasses, by = "landuse_code")
SlikkenvdHeenHabituateSteps <- sp.na.omit(SlikkenvdHeenHabituateSteps, col.name = "landuse_code") 
SlikkenvdHeenHabituateSteps$landuse_class <-  SlikkenvdHeenHabituateStepsLanduseClass$landuse_class.y

WaterDistSlikkenvdHeenHabituate <- raster::extract(WaterDistanceList[[5]], SlikkenvdHeenHabituateSteps)
ForestDistSlikkenvdHeenHabituate <- raster::extract(ForestDistanceList[[5]], SlikkenvdHeenHabituateSteps)
RoadDistSlikkenvdHeenHabituate <- raster::extract(RoadDistanceList[[5]], SlikkenvdHeenHabituateSteps)

SlikkenvdHeenHabituateSteps$WaterDistance <- WaterDistSlikkenvdHeenHabituate
SlikkenvdHeenHabituateSteps$ForestDistance <- ForestDistSlikkenvdHeenHabituate
SlikkenvdHeenHabituateSteps$RoadDistance <- RoadDistSlikkenvdHeenHabituate

SlikkenvdHeenHabituateStepsTibble <- as_tibble(SlikkenvdHeenHabituateSteps)

# Slikken vd Heen
LanduseSlikkenvdHeen <- raster::extract(MaskedList$SlikkenvdHeen, steps_sp[which(steps_sp$track_id %in% c(20:22)),])


SlikkenvdHeenSteps <- steps_sp[which(steps_sp$track_id %in% c(20:22)),]
SlikkenvdHeenSteps$landuse_code <- LanduseSlikkenvdHeen
SlikkenvdHeenStepsLanduseClass <- inner_join(as_tibble(SlikkenvdHeenSteps), LUTLanduseClasses, by = "landuse_code")
SlikkenvdHeenSteps <- sp.na.omit(SlikkenvdHeenSteps, col.name = "landuse_code") 
SlikkenvdHeenSteps$landuse_class <-  SlikkenvdHeenStepsLanduseClass$landuse_class.y

WaterDistSlikkenvdHeen <- raster::extract(WaterDistanceList[[6]], SlikkenvdHeenSteps)
ForestDistSlikkenvdHeen <- raster::extract(ForestDistanceList[[6]], SlikkenvdHeenSteps)
RoadDistSlikkenvdHeen <- raster::extract(RoadDistanceList[[6]], SlikkenvdHeenSteps)

SlikkenvdHeenSteps$WaterDistance <- WaterDistSlikkenvdHeen
SlikkenvdHeenSteps$ForestDistance <- ForestDistSlikkenvdHeen
SlikkenvdHeenSteps$RoadDistance <- RoadDistSlikkenvdHeen

SlikkenvdHeenStepsTibble <- as_tibble(SlikkenvdHeenSteps)

# Veluwe habituate area
LanduseVeluweHabituate <- raster::extract(MaskedList$VeluweHabituate, steps_sp[which(steps_sp$track_id %in% c(23:27)),])


VeluweHabituateSteps <- steps_sp[which(steps_sp$track_id %in% c(23:27)),]
VeluweHabituateSteps$landuse_code <- LanduseVeluweHabituate
VeluweHabituateStepsLanduseClass <- inner_join(as_tibble(VeluweHabituateSteps), LUTLanduseClasses, by = "landuse_code")
VeluweHabituateSteps <- sp.na.omit(VeluweHabituateSteps, col.name = "landuse_code") 
VeluweHabituateSteps$landuse_class <-  VeluweHabituateStepsLanduseClass$landuse_class.y

WaterDistVeluweHabituate <- raster::extract(WaterDistanceList[[7]], VeluweHabituateSteps)
ForestDistVeluweHabituate <- raster::extract(ForestDistanceList[[7]], VeluweHabituateSteps)
RoadDistVeluweHabituate <- raster::extract(RoadDistanceList[[7]], VeluweHabituateSteps)

VeluweHabituateSteps$WaterDistance <- WaterDistVeluweHabituate
VeluweHabituateSteps$ForestDistance <- ForestDistVeluweHabituate
VeluweHabituateSteps$RoadDistance <- RoadDistVeluweHabituate

VeluweHabituateStepsTibble <- as_tibble(VeluweHabituateSteps)

# Veluwe
LanduseVeluwe <- raster::extract(MaskedList$Veluwe, steps_sp[which(steps_sp$track_id %in% c(28:47)),])


VeluweSteps <- steps_sp[which(steps_sp$track_id %in% c(28:47)),]
VeluweSteps$landuse_code <- LanduseVeluwe
VeluweStepsLanduseClass <- inner_join(as_tibble(VeluweSteps), LUTLanduseClasses, by = "landuse_code")
VeluweSteps <- sp.na.omit(VeluweSteps, col.name = "landuse_code") 
VeluweSteps$landuse_class <-  VeluweStepsLanduseClass$landuse_class.y

WaterDistVeluwe <- raster::extract(WaterDistanceList[[8]], VeluweSteps)
ForestDistVeluwe <- raster::extract(ForestDistanceList[[8]], VeluweSteps)
RoadDistVeluwe <- raster::extract(RoadDistanceList[[8]], VeluweSteps)

VeluweSteps$WaterDistance <- WaterDistVeluwe
VeluweSteps$ForestDistance <- ForestDistVeluwe
VeluweSteps$RoadDistance <- RoadDistVeluwe

VeluweStepsTibble <- as_tibble(VeluweSteps)

# Bind all study area points to get 1 tibble
steps_tibble <- bind_rows(KraansvlakStepsTibble, Maashorst2016StepsTibble,
                          Maashorst20172021StepsTibble, Maashorst2022StepsTibble,
                          SlikkenvdHeenHabituateStepsTibble, SlikkenvdHeenStepsTibble,
                          VeluweHabituateStepsTibble, VeluweStepsTibble)

# Write steps_tibble to csv
write_csv(steps_tibble, file = "~/WisentWishes/MScThesisData/GPS location data/Old/steps_tibble.csv")

# Have a look at the data
boxplot(WaterDistance ~ case_, data = steps_tibble, ylab="Water distance")

# Subset the data
steps_summerafternoon <- steps_tibble %>% 
  filter(hms >= hms("12:00:00") & hms <= hms("16:00:00")) %>% 
  filter(season == "summer")

# Create SSF model
SSF_summerafternoon <- clogit(case_ ~ landuse_class, method = 'approximate', data = steps_summerafternoon)

# Interpret SSF
summary(SSF_summerafternoon)

# View SSF
plot_model(SSF_summerafternoon, title="SSF Coefficients")

