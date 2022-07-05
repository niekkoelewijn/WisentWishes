# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to create habitat suitability maps with the definitive RSF model

# Create raster stacks for each study area with the landuse code and the water distance
KraansvlakStackComplexModel <- stack(MaskedList$Kraansvlak, WaterDistanceList$Kraansvlak, ForestDistanceList$Kraansvlak, RoadDistanceList$Kraansvlak)
Maashorst2016StackComplexModel <- stack(MaskedList$Maashorst2016, WaterDistanceList$Maashorst2016, ForestDistanceList$Maashorst2016, RoadDistanceList$Maashorst2016)
Maashorst20172021StackComplexModel <- stack(MaskedList$Maashorst20172021, WaterDistanceList$Maashorst20172021, ForestDistanceList$Maashorst20172021, RoadDistanceList$Maashorst20172021)
Maashorst2022StackComplexModel <- stack(MaskedList$Maashorst2022, WaterDistanceList$Maashorst2022, ForestDistanceList$Maashorst2022, RoadDistanceList$Maashorst2022)
SlikkenvdHeenStackComplexModel <- stack(MaskedList$SlikkenvdHeen, WaterDistanceList$SlikkenvdHeen, ForestDistanceList$SlikkenvdHeen, RoadDistanceList$SlikkenvdHeen)
SlikkenvdHeenHabStackComplexModel <- stack(MaskedList$SlikkenvdHeenHabituate, WaterDistanceList$SlikkenvdHeenHabituate, ForestDistanceList$SlikkenvdHeenHabituate, RoadDistanceList$SlikkenvdHeenHabituate)
VeluweStackComplexModel <- stack(MaskedList$Veluwe, WaterDistanceList$Veluwe, ForestDistanceList$Veluwe, RoadDistanceList$Veluwe)
VeluweHabStackComplexModel <- stack(MaskedList$VeluweHabituate, WaterDistanceList$VeluweHabituate, ForestDistanceList$VeluweHabituate, RoadDistanceList$VeluweHabituate)

# Rename layers of stacks
names(KraansvlakStackComplexModel) <- c("landuse_code", "WaterDistance", "ForestDistance", "RoadDistance")
names(Maashorst2016StackComplexModel) <- c("landuse_code", "WaterDistance", "ForestDistance", "RoadDistance")
names(Maashorst20172021StackComplexModel) <- c("landuse_code", "WaterDistance", "ForestDistance", "RoadDistance")
names(Maashorst2022StackComplexModel) <- c("landuse_code", "WaterDistance", "ForestDistance", "RoadDistance")
names(SlikkenvdHeenStackComplexModel) <- c("landuse_code", "WaterDistance", "ForestDistance", "RoadDistance")
names(SlikkenvdHeenHabStackComplexModel) <- c("landuse_code", "WaterDistance", "ForestDistance", "RoadDistance")
names(VeluweStackComplexModel) <- c("landuse_code", "WaterDistance", "ForestDistance", "RoadDistance")
names(VeluweHabStackComplexModel) <- c("landuse_code", "WaterDistance", "ForestDistance", "RoadDistance")


## General function to create habitat suitability map with input study area,
## CCI (degrees), time (time in hh:mm:ss) and season.
getHSMComplexModel <- function(StudyArea, CCI, time, season, day_night){
  
  # Check whether the input for StudyArea is valid
  StudyArea <- match.arg(StudyArea, choices = c("Kraansvlak", "Maashorst2016", "Maashorst20172021",
                                                "Maashorst2022", "SlikkenvdHeenHabituate", "SlikkenvdHeen", 
                                                "VeluweHabituate", "Veluwe"))
  
  # Get stack with raster data from StudyArea argument
  StudyAreaData <- switch(StudyArea,
                          Kraansvlak = KraansvlakStackComplexModel,
                          Maashorst2016 = Maashorst2016StackComplexModel,
                          Maashorst20172021 = Maashorst20172021StackComplexModel,
                          Maashorst2022 = Maashorst2022StackComplexModel,
                          SlikkenvdHeenHabituate = SlikkenvdHeenHabStackComplexModel,
                          SlikkenvdHeen = SlikkenvdHeenStackComplexModel,
                          VeluweHabituate = VeluweHabStackComplexModel,
                          Veluwe = VeluweStackComplexModel)
  
  # Get raster data from specified study area
  PredictDF <- as.data.frame(StudyAreaData) %>% 
    
    # Make it a tibble
    as_tibble() %>% 
    
    # Add covariates to the tibble 
    mutate(celID = row_number(),
           CCI = CCI,
           season = season,
           time = time,
           day_night = day_night) %>% 
    
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
  
  # Get 0 or 1 for day/night
  day <- ifelse(PredictDF$day_night == 'day', 1, 0)
  night <- ifelse(PredictDF$day_night == 'nigth', 1, 0)
  
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
  
  # Get time expressed as -1 (18:00) to 1 (6:00) 
  timeinsixes <- sin(2*pi*UserSpecifiedTimeDec)
  
  # Determine the availability per landuse class
  if(StudyArea == "Kraansvlak"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[1, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[1, 4])
    conforest_avail = as.numeric(ProportionAvailablePerClass[1, 5])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[1, 6])
    road_avail = as.numeric(ProportionAvailablePerClass[1, 9])
    baresoil_avail = as.numeric(ProportionAvailablePerClass[1, 10])
    swamp_avail = as.numeric(ProportionAvailablePerClass[1, 11])
    shrub_avail = as.numeric(ProportionAvailablePerClass[1, 12])
    grassheath_avail = as.numeric(ProportionAvailablePerClass[1, 14])
  }else if(StudyArea == "Maashorst2016"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[2, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[2, 4])
    conforest_avail = as.numeric(ProportionAvailablePerClass[2, 5])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[2, 6])
    road_avail = as.numeric(ProportionAvailablePerClass[2, 9])
    baresoil_avail = as.numeric(ProportionAvailablePerClass[2, 10])
    swamp_avail = as.numeric(ProportionAvailablePerClass[2, 11])
    shrub_avail = as.numeric(ProportionAvailablePerClass[2, 12])
    grassheath_avail = as.numeric(ProportionAvailablePerClass[2, 14])
  }else if(StudyArea == "Maashorst20172021"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[3, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[3, 4])
    conforest_avail = as.numeric(ProportionAvailablePerClass[3, 5])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[3, 6])
    road_avail = as.numeric(ProportionAvailablePerClass[3, 9])
    baresoil_avail = as.numeric(ProportionAvailablePerClass[3, 10])
    swamp_avail = as.numeric(ProportionAvailablePerClass[3, 11])
    shrub_avail = as.numeric(ProportionAvailablePerClass[3, 12])
    grassheath_avail = as.numeric(ProportionAvailablePerClass[3, 14])
  }else if(StudyArea == "Maashorst2022"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[4, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[4, 4])
    conforest_avail = as.numeric(ProportionAvailablePerClass[4, 5])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[4, 6])
    road_avail = as.numeric(ProportionAvailablePerClass[4, 9])
    baresoil_avail = as.numeric(ProportionAvailablePerClass[4, 10])
    swamp_avail = as.numeric(ProportionAvailablePerClass[4, 11])
    shrub_avail = as.numeric(ProportionAvailablePerClass[4, 12])
    grassheath_avail = as.numeric(ProportionAvailablePerClass[4, 14])
  }else if(StudyArea == "SlikkenvdHeenHabituate"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[5, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[5, 4])
    conforest_avail = as.numeric(ProportionAvailablePerClass[5, 5])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[5, 6])
    road_avail = as.numeric(ProportionAvailablePerClass[5, 9])
    baresoil_avail = as.numeric(ProportionAvailablePerClass[5, 10])
    swamp_avail = as.numeric(ProportionAvailablePerClass[5, 11])
    shrub_avail = as.numeric(ProportionAvailablePerClass[5, 12])
    grassheath_avail = as.numeric(ProportionAvailablePerClass[5, 14])
  }else if(StudyArea == "SlikkenvdHeen"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[6, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[6, 4])
    conforest_avail = as.numeric(ProportionAvailablePerClass[6, 5])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[6, 6])
    road_avail = as.numeric(ProportionAvailablePerClass[6, 9])
    baresoil_avail = as.numeric(ProportionAvailablePerClass[6, 10])
    swamp_avail = as.numeric(ProportionAvailablePerClass[6, 11])
    shrub_avail = as.numeric(ProportionAvailablePerClass[6, 12])
    grassheath_avail = as.numeric(ProportionAvailablePerClass[6, 14])
  }else if(StudyArea == "VeluweHabituate"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[7, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[7, 4])
    conforest_avail = as.numeric(ProportionAvailablePerClass[7, 5])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[7, 6])
    road_avail = as.numeric(ProportionAvailablePerClass[7, 9])
    baresoil_avail = as.numeric(ProportionAvailablePerClass[7, 10])
    swamp_avail = as.numeric(ProportionAvailablePerClass[7, 11])
    shrub_avail = as.numeric(ProportionAvailablePerClass[7, 12])
    grassheath_avail = as.numeric(ProportionAvailablePerClass[7, 14])
  }else if(StudyArea == "Veluwe"){
    grassland_avail = as.numeric(ProportionAvailablePerClass[8, 2])
    decforest_avail = as.numeric(ProportionAvailablePerClass[8, 4])
    conforest_avail = as.numeric(ProportionAvailablePerClass[8, 5])
    freshwater_avail = as.numeric(ProportionAvailablePerClass[8, 6])
    road_avail = as.numeric(ProportionAvailablePerClass[8, 9])
    baresoil_avail = as.numeric(ProportionAvailablePerClass[8, 10])
    swamp_avail = as.numeric(ProportionAvailablePerClass[8, 11])
    shrub_avail = as.numeric(ProportionAvailablePerClass[8, 12])
    grassheath_avail = as.numeric(ProportionAvailablePerClass[8, 14])
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
           day = day,
           night = night,
           peaknines = timeinnines,
           peakthrees = timeinthrees,
           peaktwelves = timeintwelves,
           peaksixes = timeinsixes,
           grassland_avail = grassland_avail,
           decforest_avail = decforest_avail,
           conforest_avail = conforest_avail,
           freshwater_avail = freshwater_avail,
           road_avail = road_avail,
           baresoil_avail = baresoil_avail,
           swamp_avail = swamp_avail,
           shrub_avail = shrub_avail,
           grassheath_avail = grassheath_avail)
  
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
  PredictDF$pred <- as.numeric(predict.glm(weighted_RSF, newdata = PredictDF, type= "response")[1:nrow(PredictDF)])
  
  # Replace NA values wiht prediction values
  HabitatSuitabilityRaster[PredictDF$celID] <- PredictDF$pred # rpred is raster van je study area, waardes zijn NA
  
  # Return the habitat suitability raster
  return(HabitatSuitabilityRaster)
}

KraansvlakSummerNoon20 = getHSMComplexModel("Kraansvlak", KraansvlakStack, 20, "12:00:00", "summer", "day")
KraansvlakSummerMidnight20 = getHSM("Kraansvlak", KraansvlakStack, 20, "00:00:00", "summer")
writeRaster(HabitatSuitabilityRasternight,'~/WisentWishes/MScThesisData/EnvironmentalVariables/HabitatSuitabilityRasternight.tif',options=c('TFW=YES'), overwrite = T)

HabitatSuitabilityRaster # This is made by the loop
HabitatSuitabilityRasternight # This is made by the loop


KraansvlakSummerMidnightCCI = getHSM("Maashorst2016",-1, "00:00:00", "winter")
plot(KraansvlakSummerMidnightCCI)

writeRaster(KraansvlakSummerMidnightCCI,'~/WisentWishes/MScThesisData/EnvironmentalVariables/Maashorst2016SummerMidnight12.tif',options=c('TFW=YES'), overwrite = T)

KraansvlakSummerDayComplex <- getHSMComplexModel("Kraansvlak", getMeanCCI("Kraansvlak", "summer", "12:00:00"), "12:00:00", "summer", "day")
plot(KraansvlakSummerDayComplex)
KraansvlakSummerNightComplex <- getHSMComplexModel("Kraansvlak", getMeanCCI("Kraansvlak", "summer", "00:00:00"), "00:00:00", "summer", "night")
plot(KraansvlakSummerNightComplex)

KraansvlakWinterDayComplex <- getHSMComplexModel("Kraansvlak", getMeanCCI("Kraansvlak", "winter", "12:00:00"), "12:00:00", "winter", "day")
plot(KraansvlakWinterDayComplex)
KraansvlakWinterNightComplex <- getHSMComplexModel("Kraansvlak", getMeanCCI("Kraansvlak", "winter", "00:00:00"), "00:00:00", "winter", "night")
plot(KraansvlakWinterNightComplex)

MaashorstSummerDayComplex <- getHSMComplexModel("Maashorst20172021", getMeanCCI("Maashorst20172021", "summer", "12:00:00"), "12:00:00", "summer", "day")
plot(MaashorstSummerDayComplex)
MaashorstSummerNightComplex <- getHSMComplexModel("Maashorst20172021", getMeanCCI("Maashorst20172021", "summer", "00:00:00"), "00:00:00", "summer", "night")
plot(MaashorstSummerNightComplex)

MaashorstWinterDayComplex <- getHSMComplexModel("Maashorst20172021", getMeanCCI("Maashorst20172021", "winter", "12:00:00"), "12:00:00", "winter", "day")
plot(MaashorstWinterDayComplex)
MaashorstWinterNightComplex <- getHSMComplexModel("Maashorst20172021", getMeanCCI("Maashorst20172021", "winter", "00:00:00"), "00:00:00", "winter", "night")
plot(MaashorstWinterNightComplex)

SlikkenvdHeenSummerDayComplex <- getHSMComplexModel("SlikkenvdHeen", getMeanCCI("SlikkenvdHeen", "summer", "12:00:00"), "12:00:00", "summer", "day")
plot(SlikkenvdHeenSummerDayComplex)
SlikkenvdHeenSummerNightComplex <- getHSMComplexModel("SlikkenvdHeen", getMeanCCI("SlikkenvdHeen", "summer", "00:00:00"), "12:00:00", "summer", "night")
plot(SlikkenvdHeenSummerNightComplex)

SlikkenvdHeenWinterDayComplex <- getHSMComplexModel("SlikkenvdHeen", getMeanCCI("SlikkenvdHeen", "winter", "12:00:00"), "12:00:00", "winter", "day")
plot(SlikkenvdHeenWinterDayComplex)
SlikkenvdHeenWinterNightComplex <- getHSMComplexModel("SlikkenvdHeen", getMeanCCI("SlikkenvdHeen", "winter", "00:00:00"), "12:00:00", "winter", "night")
plot(SlikkenvdHeenWinterNightComplex)

VeluweSummerDayComplex <- getHSMComplexModel("Veluwe", getMeanCCI("Veluwe", "summer", "12:00:00"), "12:00:00", "summer", "day")
plot(VeluweSummerDayComplex)
VeluweSummerNightComplex <- getHSMComplexModel("Veluwe", getMeanCCI("Veluwe", "summer", "00:00:00"), "12:00:00", "summer", "night")
plot(VeluweSummerNightComplex)

VeluweWinterDayComplex <- getHSMComplexModel("Veluwe", getMeanCCI("Veluwe", "winter", "12:00:00"), "12:00:00", "winter", "day")
plot(VeluweWinterDayComplex)
VeluweWinterNightComplex <- getHSMComplexModel("Veluwe", getMeanCCI("Veluwe", "winter", "00:00:00"), "12:00:00", "winter", "night")
plot(VeluweWinterNightComplex)


VeluweWinterDay_scale <- 0 + (VeluweWinterDay - cellStats(VeluweWinterDay, "min")) * ((1 - 0) / (cellStats(VeluweWinterDay, "max") - cellStats(VeluweWinterDay, "min")))
plot(VeluweWinterDay_scale)

VeluweWinterNight_scale <- 0 + (VeluweWinterNight - cellStats(VeluweWinterNight, "min")) * ((1 - 0) / (cellStats(VeluweWinterNight, "max") - cellStats(VeluweWinterNight, "min")))
plot(VeluweWinterDay_scale, col = RColorBrewer::brewer.pal(n = "PiYG", name = "PiYG"))
















