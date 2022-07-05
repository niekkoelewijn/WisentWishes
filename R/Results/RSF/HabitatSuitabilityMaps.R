# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to create habitat suitability maps with the definitive RSF model

# Create raster stacks for each study area with the landuse code and the water distance
KraansvlakStack <- stack(MaskedList$Kraansvlak, WaterDistanceList$Kraansvlak)
Maashorst2016Stack <- stack(MaskedList$Maashorst2016, WaterDistanceList$Maashorst2016)
Maashorst20172021Stack <- stack(MaskedList$Maashorst20172021, WaterDistanceList$Maashorst20172021)
Maashorst2022Stack <- stack(MaskedList$Maashorst2022, WaterDistanceList$Maashorst2022)
SlikkenvdHeenStack <- stack(MaskedList$SlikkenvdHeen, WaterDistanceList$SlikkenvdHeen)
SlikkenvdHeenHabStack <- stack(MaskedList$SlikkenvdHeenHabituate, WaterDistanceList$SlikkenvdHeenHabituate)
VeluweStack <- stack(MaskedList$Veluwe, WaterDistanceList$Veluwe)
VeluweHabStack <- stack(MaskedList$VeluweHabituate, WaterDistanceList$VeluweHabituate)

# Rename layers of stacks
names(KraansvlakStack) <- c("landuse_code", "WaterDistance")
names(Maashorst2016Stack) <- c("landuse_code", "WaterDistance")
names(Maashorst20172021Stack) <- c("landuse_code", "WaterDistance")
names(Maashorst2022Stack) <- c("landuse_code", "WaterDistance")
names(SlikkenvdHeenStack) <- c("landuse_code", "WaterDistance")
names(SlikkenvdHeenHabStack) <- c("landuse_code", "WaterDistance")
names(VeluweStack) <- c("landuse_code", "WaterDistance")
names(VeluweHabStack) <- c("landuse_code", "WaterDistance")


## General function to create habitat suitability map with input study area,
## CCI (degrees), time (time in hh:mm:ss) and season.
getHSM <- function(StudyArea, CCI, time, season){
  
  # Check whether the input for StudyArea is valid
  StudyArea <- match.arg(StudyArea, choices = c("Kraansvlak", "Maashorst2016", "Maashorst20172021",
                                                "Maashorst2022", "SlikkenvdHeenHabituate", "SlikkenvdHeen", 
                                                "VeluweHabituate", "Veluwe"))
  
  # Get stack with raster data from StudyArea argument
  StudyAreaData <- switch(StudyArea,
                          Kraansvlak = KraansvlakStack,
                          Maashorst2016 = Maashorst2016Stack,
                          Maashorst20172021 = Maashorst20172021Stack,
                          Maashorst2022 = Maashorst2022Stack,
                          SlikkenvdHeenHabituate = SlikkenvdHeenHabStack,
                          SlikkenvdHeen = SlikkenvdHeenStack,
                          VeluweHabituate = VeluweHabStack,
                          Veluwe = VeluweStack)
  
  # Get raster data from specified study area
  PredictDF <- as.data.frame(StudyAreaData) %>% 
    
    # Make it a tibble
    as_tibble() %>% 
    
    # Add covariates to the tibble 
    mutate(celID = row_number(),
           CCI = CCI,
           season = season,
           time = time) %>% 
    
    # Drop NA
    drop_na()
  
  # Get landuse classes from the landuse codes
  grassland <- ifelse(PredictDF$landuse_code == 1, 1, 0)
  deciduous_forest <- ifelse(PredictDF$landuse_code == 3, 1, 0)
  coniferous_forest <- ifelse(PredictDF$landuse_code == 4, 1, 0)
  fresh_water <- ifelse(PredictDF$landuse_code == 5, 1, 0)
  road <- ifelse(PredictDF$landuse_code == 8, 1, 0)
  bare_soil <- ifelse(PredictDF$landuse_code == 9, 1, 0)
  swamp <- ifelse(PredictDF$landuse_code == 10, 1, 0)
  shrubland <- ifelse(PredictDF$landuse_code == 11, 1, 0)
  heathland <- ifelse(PredictDF$landuse_code == 12, 1, 0)
  grassy_heathland <- ifelse(PredictDF$landuse_code == 13, 1, 0)
  
  # Get 0 or 1 for season
  spring <- ifelse(PredictDF$season == 'spring', 1, 0)
  summer <- ifelse(PredictDF$season == 'summer', 1, 0)
  autumn <- ifelse(PredictDF$season == 'autumm', 1, 0)
  winter <- ifelse(PredictDF$season == 'winter', 1, 0)
  
  # Get decimal time from user time input
  UserSpecifiedTimeDec <- as.numeric(
    difftime(
      as.POSIXct(
        x = paste0("01-01-2000 ", time), 
        tryFormats = "%d-%m-%Y %H:%M:%OS"), 
      floor_date(
        as.POSIXct(x ="01-01-2000 ", 
                   tryFormats = "%d-%m-%Y")), 
      units="days"))
  
  # Get time expressed as -1 (21:00) to 1 (9:00) 
  timeinnines <- sin(2*pi*UserSpecifiedTimeDec-(pi/4))
  
  # Get time expressed as -1 (15:00) to 1 (3:00) 
  timeinthrees <- -sin(2*pi*UserSpecifiedTimeDec+(pi/4))
  
  # Get time expressed as -1 (00:00) to 1 (12:00) 
  timeintwelves <- -cos(2*pi*UserSpecifiedTimeDec)
  
  # Determine the availability per landuse class
  if(StudyArea == "Kraansvlak"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[1, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[1, 4])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[1, 6])
    shrub_avail = as.numeric(ProportionAvailablePerClass[1, 12])
  }else if(StudyArea == "Maashorst2016"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[2, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[2, 4])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[2, 6])
    shrub_avail = as.numeric(ProportionAvailablePerClass[2, 12])
  }else if(StudyArea == "Maashorst20172021"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[3, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[3, 4])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[3, 6])
    shrub_avail = as.numeric(ProportionAvailablePerClass[3, 12])
  }else if(StudyArea == "Maashorst2022"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[4, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[4, 4])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[4, 6])
    shrub_avail = as.numeric(ProportionAvailablePerClass[4, 12])
  }else if(StudyArea == "SlikkenvdHeenHabituate"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[5, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[5, 4])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[5, 6])
    shrub_avail = as.numeric(ProportionAvailablePerClass[5, 12])
  }else if(StudyArea == "SlikkenvdHeen"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[6, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[6, 4])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[6, 6])
    shrub_avail = as.numeric(ProportionAvailablePerClass[6, 12])
  }else if(StudyArea == "VeluweHabituate"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[7, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[7, 4])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[7, 6])
    shrub_avail = as.numeric(ProportionAvailablePerClass[7, 12])
  }else if(StudyArea == "Veluwe"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[8, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[8, 4])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[8, 6])
    shrub_avail = as.numeric(ProportionAvailablePerClass[8, 12])
  }
  
  # Assign variables to the dataframe
  PredictDF <- PredictDF %>% 
    
    # Create columns for each variable
    mutate(grassland = grassland,
           deciduous_forest = deciduous_forest,
           coniferous_forest = coniferous_forest,
           fresh_water = fresh_water,
           road = road,
           bare_soil = bare_soil,
           swamp = swamp,
           shrubland = shrubland,
           heathland = heathland,
           grassy_heathland = grassy_heathland,
           spring = spring,
           summer = summer,
           autumn = autumn,
           winter = winter,
           peaknines = timeinnines,
           peakthrees = timeinthrees,
           peaktwelves = timeintwelves,
           grassland_avail = grassland_avail,
           decforest_avail = decforest_avail,
           freshwater_avail = freshwater_avail,
           shrub_avail = shrub_avail)
  
  # Create HabitatSuitabilityRaster with dimentions of landuse code raster of a specified study area
  HabitatSuitabilityRaster <- switch(StudyArea,
                                     Kraansvlak = KraansvlakStack[[1]],
                                     Maashorst2016 = Maashorst2016Stack[[1]],
                                     Maashorst20172021 = Maashorst20172021Stack[[1]],
                                     Maashorst2022 = Maashorst2022Stack[[1]],
                                     SlikkenvdHeenHabituate = SlikkenvdHeenHabStack[[1]],
                                     SlikkenvdHeen = SlikkenvdHeenStack[[1]],
                                     VeluweHabituate = VeluweHabStack[[1]],
                                     Veluwe = VeluweStack[[1]])
  
  # Set values of HSR to NA
  HabitatSuitabilityRaster[] <- NA
  
  # Get prediction for each row in the PredictDF 
  PredictDF$pred <- as.numeric(predict.glm(definitive_RSF, newdata = PredictDF, type= "response")[1:nrow(PredictDF)])
  
  # Replace NA values wiht prediction values
  HabitatSuitabilityRaster[PredictDF$celID] <- PredictDF$pred
  
  # Return the habitat suitability raster
  return(HabitatSuitabilityRaster)
}

KraansvlakSummerDay <- getHSM("Kraansvlak", getMeanCCI("Kraansvlak", "summer", "12:00:00"), "12:00:00", "summer")
plot(KraansvlakSummerDay)
KraansvlakSummerNight <- getHSM("Kraansvlak", getMeanCCI("Kraansvlak", "summer", "00:00:00"), "00:00:00", "summer")
plot(KraansvlakSummerNight)

KraansvlakWinterDay <- getHSM("Kraansvlak", getMeanCCI("Kraansvlak", "winter", "12:00:00"), "12:00:00", "winter")
plot(KraansvlakWinterDay)
KraansvlakWinterNight <- getHSM("Kraansvlak", getMeanCCI("Kraansvlak", "winter", "00:00:00"), "00:00:00", "winter")
plot(KraansvlakWinterNight)

writeRaster(KraansvlakSummerDay,'~/WisentWishes/ResultsRSF/Kraansvlak/KraansvlakSummerDay',options=c('TFW=YES'), overwrite = T)
writeRaster(KraansvlakSummerNight,'~/WisentWishes/ResultsRSF/Kraansvlak/KraansvlakSummerNight',options=c('TFW=YES'), overwrite = T)
writeRaster(KraansvlakWinterDay,'~/WisentWishes/ResultsRSF/Kraansvlak/KraansvlakWinterDay',options=c('TFW=YES'), overwrite = T)
writeRaster(KraansvlakWinterNight,'~/WisentWishes/ResultsRSF/Kraansvlak/KraansvlakWinterNight',options=c('TFW=YES'), overwrite = T)


MaashorstSummerDay <- getHSM("Maashorst20172021", getMeanCCI("Maashorst20172021", "summer", "12:00:00"), "12:00:00", "summer")
plot(MaashorstSummerDay)
MaashorstSummerNight <- getHSM("Maashorst20172021", getMeanCCI("Maashorst20172021", "summer", "00:00:00"), "00:00:00", "summer")
plot(MaashorstSummerNight)

MaashorstWinterDay <- getHSM("Maashorst20172021", getMeanCCI("Maashorst20172021", "winter", "12:00:00"), "12:00:00", "winter")
plot(MaashorstWinterDay)
MaashorstWinterNight <- getHSM("Maashorst20172021", getMeanCCI("Maashorst20172021", "winter", "00:00:00"), "00:00:00", "winter")
plot(MaashorstWinterNight)

SlikkenvdHeenSummerDay <- getHSM("SlikkenvdHeen", getMeanCCI("SlikkenvdHeen", "summer", "12:00:00"), "12:00:00", "summer")
plot(SlikkenvdHeenSummerDay)
SlikkenvdHeenSummerNight <- getHSM("SlikkenvdHeen", getMeanCCI("SlikkenvdHeen", "summer", "00:00:00"), "00:00:00", "summer")
plot(SlikkenvdHeenSummerNight)

SlikkenvdHeenWinterDay <- getHSM("Veluwe", getMeanCCI("Veluwe", "winter", "12:00:00"), "12:00:00", "winter")
plot(SlikkenvdHeenWinterDay)
SlikkenvdHeenWinterNight <- getHSM("Veluwe", getMeanCCI("Veluwe", "winter", "00:00:00"), "00:00:00", "winter")
plot(SlikkenvdHeenWinterNight)

VeluweSummerDay <- getHSM("Veluwe", getMeanCCI("Veluwe", "summer", "12:00:00"), "12:00:00", "summer")
plot(VeluweSummerDay)
VeluweSummerNight <- getHSM("Veluwe", getMeanCCI("Veluwe", "summer", "00:00:00"), "00:00:00", "summer")
plot(VeluweSummerNight)

VeluweWinterDay <- getHSM("Veluwe", getMeanCCI("Veluwe", "winter", "12:00:00"), "12:00:00", "winter")
plot(VeluweWinterDay)
VeluweWinterNight <- getHSM("Veluwe", getMeanCCI("Veluwe", "winter", "00:00:00"), "00:00:00", "winter")
plot(VeluweWinterNight)


VeluweWinterDay_scale <- 0 + (VeluweWinterDay - cellStats(VeluweWinterDay, "min")) * ((1 - 0) / (cellStats(VeluweWinterDay, "max") - cellStats(VeluweWinterDay, "min")))
plot(VeluweWinterDay_scale)

VeluweWinterNight_scale <- 0 + (VeluweWinterNight - cellStats(VeluweWinterNight, "min")) * ((1 - 0) / (cellStats(VeluweWinterNight, "max") - cellStats(VeluweWinterNight, "min")))
plot(VeluweWinterDay_scale, col = RColorBrewer::brewer.pal(n = "PiYG", name = "PiYG"))
















