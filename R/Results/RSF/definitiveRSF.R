# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to set up the definitive RSF model

## Create random points for each study area

# Create function to prepare data for RSF
getRandomPointsStudyArea <- function(TrackDataset, StudyAreasf, StudyAreaCharacter){
  
  # Get copy of input track dataset
  ObservedPoints <- TrackDataset
  
  # Add case attribute to ObservedPoints
  ObservedPoints <- ObservedPoints %>% 
    
    # Create case attribute
    mutate(case = 1) %>% 
    
    # Remove speed, angle, turning angle and step length attribute
    select(-c(ID, track_row_ID, speed_in, speed_out, angle, track_ID,
              time_interval, step_length))
  
  # Create 10 random points (pseudo-absences) for each observation point
  RandomPoints <- st_sample(StudyAreasf, 10*nrow(ObservedPoints), type = "random")
  
  # Get X and Y from RandomPoints
  RSFData <- RandomPoints %>% 
    
    # Transform dataset to tibble
    as_tibble() %>% 
    
    # Add attributes to the random points. Time attributes with rep, distance
    # and landuse with extract
    mutate(X = st_coordinates(RandomPoints$geometry)[,1],
           Y = st_coordinates(RandomPoints$geometry)[,2],
           case = 0,
           time = rep(ObservedPoints$time, each = 10),
           temp = rep(ObservedPoints$temp, each = 10),
           hdop = rep(ObservedPoints$hdop, each = 10),
           landuse_code = raster::extract(MaskedList[[StudyAreaCharacter]]),
           WaterDistance = raster::extract(WaterDistanceList[[StudyAreaCharacter]]),
           ForestDistance = raster::extract(ForestDistanceList[[StudyAreaCharacter]]),
           RoadDistance = raster::extract(RoadDistanceList[[StudyAreaCharacter]]),
           date = rep(ObservedPoints$date, each = 10),
           hms = rep(ObservedPoints$hms, each = 10),
           weekday = rep(ObservedPoints$weekday, each = 10),
           day_type = rep(ObservedPoints$day_type, each = 10),
           season = rep(ObservedPoints$season, each = 10),
           day_night = rep(ObservedPoints$day_night, each = 10),
           average_windspeed_day = rep(ObservedPoints$average_windspeed_day, each = 10),
           average_temperature_day = rep(ObservedPoints$average_temperature_day, each = 10),
           sunshine_duration_day = rep(ObservedPoints$sunshine_duration_day, each = 10),
           solar_radiation = rep(ObservedPoints$solar_radiation, each = 10),
           average_relative_humidity = rep(ObservedPoints$average_relative_humidity, each = 10),
           precipitation_duration_day = rep(ObservedPoints$precipitation_duration_day, each = 10),
           total_precipitation_day = rep(ObservedPoints$total_precipitation_day, each = 10),
           CCI = rep(ObservedPoints$CCI, each = 10)) %>% 
    
    # Join with the LUT to get landuse classes as names
    inner_join(LUTLanduseClasses, by = "landuse_code") %>% 
    
    # Now bind the rows of the observed points 
    bind_rows(ObservedPoints)
  
  #Return RSF data
  return(RSFData)
}

# Call getRandomPointsStudyArea per study area
RSFDataKraansvlak <- getRandomPointsStudyArea(KraansvlakPoints, KraansvlakStudyAreaRDNew, "Kraansvlak")
RSFDataMaashorst2016 <- getRandomPointsStudyArea(Maashorst2016Points, MaashorstStudyAreaRDNew2016, "Maashorst2016")
RSFDataMaashorst20172021 <- getRandomPointsStudyArea(Maashorst20172021Points, MaashorstStudyAreaRDNew20172021, "Maashorst20172021")
RSFDataMaashorst2022 <- getRandomPointsStudyArea(Maashorst2022Points, MaashorstStudyAreaRDNew2022, "Maashorst2022")
RSFDataSlikkenvdHeenHab <- getRandomPointsStudyArea(SlikkenvdHeenHabPoints, SlikkenvdHeenHabRDNew, "SlikkenvdHeenHabituate")
RSFDataSlikkenvdHeen <- getRandomPointsStudyArea(SlikkenvdHeenPoints, SlikkenvdHeenStudyAreaRDNew, "SlikkenvdHeen")
RSFDataVeluweHab <- getRandomPointsStudyArea(VeluweHabPoints, VeluweHabRDNew, "VeluweHabituate")
RSFDataVeluwe <- getRandomPointsStudyArea(VeluwePoints, VeluweStudyAreaRDNew, "Veluwe")
