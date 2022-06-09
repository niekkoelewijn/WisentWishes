# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to set up the definitive RSF model

## Create random points for each study area

# Create function to prepare data for RSF
getRandomPointsStudyArea <- function(TrackDataset, StudyAreasf, StudyRegion, StudyArea){
  
  # Get copy of input track dataset
  ObservedPoints <- TrackDataset
  
  # Add case attribute to ObservedPoints
  ObservedPoints <- ObservedPoints %>% 
    
    # Create case attribute
    mutate(case = T) %>% 
    
    # Remove speed, angle, turning angle and step length attribute
    select(-c(ID, track_row_ID, speed_in, speed_out, angle, track_ID,
              time_interval, step_length, time_coded))
  
  # Create 10 random points (pseudo-absences) for each observation point
  RandomPoints <- st_sample(StudyAreasf, 10*nrow(ObservedPoints), type = "random")
  
  # Create tibble with from Random Points
  RandomPointsTibble <- tibble(.rows = length(RandomPoints))
  
  # Get X and Y from RandomPoints
  RSFData <- RandomPointsTibble %>% 
    
    # Add attributes to the random points. Time attributes with rep, distance
    # and landuse with extract
    mutate(X = st_coordinates(RandomPoints)[,1],
           Y = st_coordinates(RandomPoints)[,2],
           case = F,
           time = rep(ObservedPoints$time, each = 10),
           temp = rep(ObservedPoints$temp, each = 10),
           hdop = rep(ObservedPoints$hdop, each = 10),
           landuse_code = raster::extract(x = LandUseList[[StudyRegion]], y = st_sf(RandomPoints)),
           WaterDistance = raster::extract(x = WaterDistanceList[[StudyArea]], y = st_sf(RandomPoints)),
           ForestDistance = raster::extract(x = ForestDistanceList[[StudyArea]], y = st_sf(RandomPoints)),
           RoadDistance = raster::extract(x = RoadDistanceList[[StudyArea]], y = st_sf(RandomPoints)),
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
RSFDataKraansvlak <- getRandomPointsStudyArea(KraansvlakPoints, KraansvlakStudyAreaRDNew, "KraansvlakLandUse","Kraansvlak")
RSFDataMaashorst2016 <- getRandomPointsStudyArea(Maashorst2016Points, MaashorstStudyAreaRDNew2016, "MaashorstLandUse", "Maashorst2016")
RSFDataMaashorst20172021 <- getRandomPointsStudyArea(Maashorst20172021Points, MaashorstStudyAreaRDNew20172021,"MaashorstLandUse", "Maashorst20172021")
RSFDataMaashorst2022 <- getRandomPointsStudyArea(Maashorst2022Points, MaashorstStudyAreaRDNew2022, "MaashorstLandUse", "Maashorst2022")
RSFDataSlikkenvdHeenHab <- getRandomPointsStudyArea(SlikkenvdHeenHabPoints, SlikkenvdHeenHabRDNew, "SlikkenvdHeenLandUse", "SlikkenvdHeenHabituate")
RSFDataSlikkenvdHeen <- getRandomPointsStudyArea(SlikkenvdHeenPoints, SlikkenvdHeenStudyAreaRDNew, "SlikkenvdHeenLandUse", "SlikkenvdHeen")
RSFDataVeluweHab <- getRandomPointsStudyArea(VeluweHabPoints, VeluweHabRDNew, "VeluweLandUse", "VeluweHabituate")
RSFDataVeluwe <- getRandomPointsStudyArea(VeluwePoints, VeluweStudyAreaRDNew, "VeluweLandUse", "Veluwe")

# Bind the different study areas to get 1 RSF dataset
RSFTotal <- bind_rows(RSFDataKraansvlak, RSFDataMaashorst2016, 
                      RSFDataMaashorst20172021, RSFDataMaashorst2022, 
                      RSFDataSlikkenvdHeenHab, RSFDataSlikkenvdHeen,
                      RSFDataVeluweHab, RSFDataVeluwe)


## Create RSF model with best fit

# Visualize proportion used per landuse class, used and available
RSFTotal %>% 
  group_by(case, landuse_class) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), 
         label = paste0(round(prop * 100, 1), "%")) %>% 
  ggplot(aes(landuse_class, prop, fill = case, group=case,label = label)) + 
  geom_col(position = position_dodge2()) +
  geom_text(size = 4, vjust = -0.25, position = position_dodge(width = 1)) +
  labs(x = "Land use class", y = "Proportion", fill = "case")+
  scale_fill_brewer(palette = "Paired", name="case", 
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used")) +
  theme_light()

# Fit first simple model
Landuse_class_RSF <- glm(case ~ landuse_class
                           , data = RSFTotal, family = binomial(link = "logit"),
                           offset = rep(qlogis(1/11), nrow(RSFTotal)))


# Interpret first simple model
summary(Landuse_class_RSF)

# View first simple model
plot_model(Landuse_class_RSF)

# Add landuse dummies to model
coniferous_forest <- ifelse(RSFTotal$landuse_class == 'coniferous forest', 1, 0)
deciduous_forest <- ifelse(RSFTotal$landuse_class == 'deciduous forest', 1, 0)
fresh_water <- ifelse(RSFTotal$landuse_class == 'fresh water', 1, 0)
grassland <- ifelse(RSFTotal$landuse_class == 'grassland', 1, 0)
grassy_heathland <- ifelse(RSFTotal$landuse_class == 'grassy heathland', 1, 0)
heathland <- ifelse(RSFTotal$landuse_class == 'heathland', 1, 0)
road <- ifelse(RSFTotal$landuse_class == 'road', 1, 0)
shrubland <- ifelse(RSFTotal$landuse_class == 'shrubland', 1, 0)
swamp <- ifelse(RSFTotal$landuse_class == 'swamp', 1, 0)

RSFClassDummy <- RSFTotal %>% 
  mutate(coniferous_forest = coniferous_forest,
         deciduous_forest = deciduous_forest,
         fresh_water = fresh_water,
         grassland = grassland,
         grassy_heathland = grassy_heathland,
         heathland = heathland,
         road = road,
         shrubland = shrubland,
         swamp = swamp)

# Fit model to dataset with dummies
Landuse_Dummy_class_RSF <- glm(case ~ coniferous_forest + deciduous_forest +
                               fresh_water + grassland + grassy_heathland +
                               grassy_heathland + heathland + road +
                               shrubland + swamp, 
                             data = RSFClassDummy, family = binomial(link = "logit"),
                             offset = rep(qlogis(1/11), nrow(RSFClassDummy)))

# Interpret model
summary(Landuse_Dummy_class_RSF)

# View first simple model
plot_model(Landuse_Dummy_class_RSF)

# Check wether AIC's are the same:
AIC(Landuse_Dummy_class_RSF) == AIC(Landuse_class_RSF) # TRUE, both 547134.3


## Expand the model with CCI (perceived temperature)

# Fit model to dataset with dummies for landuse classes, and add interaction with 
# CCI. With temperature as factor.
CCI_in_Model_RSF <- glm(case ~ coniferous_forest + deciduous_forest +
                               fresh_water + grassland + grassy_heathland +
                               grassy_heathland + heathland + road +
                               shrubland + swamp + scale(CCI) + coniferous_forest:scale(CCI) + 
                               deciduous_forest:scale(CCI) + fresh_water:scale(CCI) + 
                               grassland:scale(CCI) + grassy_heathland:scale(CCI) +
                               grassy_heathland:scale(CCI) + heathland:scale(CCI) + 
                               road:scale(CCI) + shrubland:scale(CCI) + swamp:scale(CCI), 
                             data = RSFClassDummy, family = binomial(link = "logit"),
                             offset = rep(qlogis(1/11), nrow(RSFClassDummy)))
# Interpret model
summary(CCI_in_Model_RSF)

# Get AIC and compare
AIC(CCI_in_Model_RSF) # 546050.1, 951 lower than Landuse_Dummy_class_RSF

# Fit model to dataset with dummies for landuse classes, and add interaction with 
# CCI. Without temperature as factor.
CCI_out_Model_RSF <- glm(case ~ coniferous_forest + deciduous_forest +
                          fresh_water + grassland + grassy_heathland +
                          grassy_heathland + heathland + road +
                          shrubland + swamp + coniferous_forest:scale(CCI) + 
                          deciduous_forest:scale(CCI) + fresh_water:scale(CCI) + 
                          grassland:scale(CCI) + grassy_heathland:scale(CCI) +
                          grassy_heathland:scale(CCI) + heathland:scale(CCI) + 
                          road:scale(CCI) + shrubland:scale(CCI) + swamp:scale(CCI), 
                        data = RSFClassDummy, family = binomial(link = "logit"),
                        offset = rep(qlogis(1/11), nrow(RSFClassDummy)))
# Interpret model
summary(CCI_out_Model_RSF)

# Get AIC and compare
AIC(CCI_out_Model_RSF, CCI_in_Model_RSF) # 546052.4, 2.3 higher than Landuse_Dummy_class_RSF
anova(CCI_out_Model_RSF, CCI_in_Model_RSF)


