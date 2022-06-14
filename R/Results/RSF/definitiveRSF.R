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
              time_interval, step_length, time_coded)) %>% 
    
    # Add proportional availability per landuse class to observed points dataset
    mutate(grassland_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),2]),
           conforest_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),4]),
           decforest_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),5]),
           freshwater_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),6]),
           freshwater_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),6]),
           road_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),9]),
           baresoil_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),10]),
           swamp_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),11]),
           shrub_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),12]),
           heathland_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),13]),
           grassheath_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),14]))
  
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
           CCI = rep(ObservedPoints$CCI, each = 10),
           grassland_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),2]),
           conforest_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),4]),
           decforest_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),5]),
           freshwater_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),6]),
           freshwater_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),6]),
           road_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),9]),
           baresoil_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),10]),
           swamp_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),11]),
           shrub_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),12]),
           heathland_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),13]),
           grassheath_avail = as.numeric(ProportionAvailablePerClass[which(ProportionAvailablePerClass$`Study area` == StudyArea),14])) %>%
    
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
# I will not include CCI as a factor, as this doesnt make sence from a ecological point of view


## Add rows with self determined landuse class and season to get intercept of 0

# Add bare soil column to RSF dataset
bare_soil <- ifelse(RSFTotal$landuse_class == 'bare soil', 1, 0)

RSFIntZero <- RSFClassDummy %>% 
  mutate(bare_soil = bare_soil)

# Create dummy variables for all seasons
spring <- ifelse(RSFTotal$season == 'spring', 1, 0)
summer <- ifelse(RSFTotal$season == 'summer', 1, 0)
autumn <- ifelse(RSFTotal$season == 'autumm', 1, 0)
winter <- ifelse(RSFTotal$season == 'winter', 1, 0)

RSFIntZero <- RSFIntZero %>% 
  mutate(spring = spring,
         summer = summer,
         autumn = autumn,
         winter = winter)

# Create dummy variables for all day types
weekend <- ifelse(RSFTotal$day_type == 'weekend', 1, 0)
business_day <- ifelse(RSFTotal$day_type == 'business day', 1, 0)

RSFIntZero <- RSFIntZero %>% 
  mutate(weekend = weekend,
         business_day = business_day)

# Create dummy variables for day and night
day <- ifelse(RSFTotal$day_night == 'day', 1, 0)
night <- ifelse(RSFTotal$day_night == 'night', 1, 0)

RSFIntZero <- RSFIntZero %>% 
  mutate(day = day,
         night = night)

# Create empty data.frame with size 11*43
UserDefinedExtraClasses <- tibble(data.frame(matrix(data = NA, nrow = 11, ncol = 53)))
for(i in seq_along(UserDefinedExtraClasses)){
  for(j in 1:nrow(UserDefinedExtraClasses)){
    if(i == 1){
      UserDefinedExtraClasses[j,i] <- sample(RSFClassDummy$X, 1)
    }
    if(i == 2){
      UserDefinedExtraClasses[j,i] <- sample(RSFClassDummy$Y, 1)
    }
    if(i == 3){
      if(j == 1){
        UserDefinedExtraClasses[j,i] <- T
      }else{
        UserDefinedExtraClasses[j,i] <- F
      }
    }
    if(i == 4){
      UserDefinedExtraClasses[j,i] <- sample(RSFClassDummy$time, 1)
    }
    if(i == 5){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$temp)
    }
    if(i == 6){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$hdop)
    }
    if(i == 7){
      UserDefinedExtraClasses[j,i] <- 14
    }
    if(i == 8){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$WaterDistance)
    }
    if(i == 9){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$ForestDistance)
    }
    if(i == 10){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$RoadDistance)
    }
    if(i == 11){
      UserDefinedExtraClasses[j,i] <- sample(RSFClassDummy$date, 1)
    }
    if(i == 12){
      UserDefinedExtraClasses[j,i] <- sample(RSFClassDummy$hms, 1)
    }
    if(i == 13){
      UserDefinedExtraClasses[j,i] <- "A"
    }
    if(i == 14){
      UserDefinedExtraClasses[j,i] <- "A"
    }
    if(i == 15){
      UserDefinedExtraClasses[j,i] <- "A"
    }
    if(i == 16){
      UserDefinedExtraClasses[j,i] <- "A"
    }
    if(i == 17){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$average_windspeed_day)
    }
    if(i == 18){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$average_temperature_day)
    }
    if(i == 19){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$sunshine_duration_day)
    }
    if(i == 20){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$solar_radiation)
    }
    if(i == 21){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$average_relative_humidity)
    }
    if(i == 22){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$precipitation_duration_day)
    }
    if(i == 23){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$total_precipitation_day)
    }
    if(i == 24){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$CCI)
    }
    if(i == 25){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$grassland_avail)
    }
    if(i == 26){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$conforest_avail)
    }
    if(i == 27){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$decforest_avail)
    }
    if(i == 28){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$freshwater_avail)
    }
    if(i == 29){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$road_avail)
    }
    if(i == 30){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$baresoil_avail)
    }
    if(i == 31){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$swamp_avail)
    }
    if(i == 32){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$shrub_avail)
    }
    if(i == 33){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$heathland_avail)
    }
    if(i == 34){
      UserDefinedExtraClasses[j,i] <- mean(RSFClassDummy$grassheath_avail)
    }
    if(i == 35){
      UserDefinedExtraClasses[j,i] <- "A"
    }
    if(i >= 36){
      UserDefinedExtraClasses[j,i] <- 0
    }
  }
}
colnames(UserDefinedExtraClasses) <- colnames(RSFIntZero)


# Bind rows to RSFIntZero
RSFIntZeroUD <- RSFIntZero %>% 
  bind_rows(UserDefinedExtraClasses)

# Make factors out of the category columns
RSFIntZeroUD <- RSFIntZeroUD %>% 
  mutate(weekday = factor(weekday, levels = c(sort(unique(weekday)))),
         day_night = factor(day_night, levels = c(sort(unique(day_night)))),
         season = factor(season, levels = c(sort(unique(season)))),
         landuse_class = factor(landuse_class, levels = c(sort(unique(landuse_class)))),
         day_type = factor(day_type, levels = c(sort(unique(day_type)))))


## Reproduce earlier models to see effect

# Fit model that compares landuse and case
Landuse_class_RSFIntZeroUD <- glm(case ~ landuse_class, 
                                  data = RSFIntZeroUD, family = binomial(link = "logit"),
                                  offset = rep(qlogis(1/11), nrow(RSFIntZeroUD)))


# Interpret first simple model
summary(Landuse_class_RSFIntZeroUD)

# Fit model that compares each user defined dummy of landuse class with case
Landuse_Dummy_class_RSFIntZeroUD <- glm(case ~ bare_soil + coniferous_forest + deciduous_forest +
                                          fresh_water + grassland + grassy_heathland +
                                          grassy_heathland + heathland + road +
                                          shrubland + swamp, 
                                        data = RSFIntZeroUD, family = binomial(link = "logit"),
                                        offset = rep(qlogis(1/11), nrow(RSFIntZeroUD)))

# Interpret model
summary(Landuse_Dummy_class_RSFIntZeroUD)

# Compare model
AIC(Landuse_class_RSFIntZeroUD) == AIC(Landuse_Dummy_class_RSFIntZeroUD) # TRUE, models are equal to eachother

# Fit model that compares each user defined dummy of landuse class with case, 
# and the interaction of landuse class and perveived temperature (CCI)
Landuse_CCI_RSFIntZeroUD <- glm(case ~  bare_soil + coniferous_forest + deciduous_forest +
                                        fresh_water + grassland + grassy_heathland +
                                        grassy_heathland + heathland + road +
                                        shrubland + swamp + bare_soil:scale(CCI) + coniferous_forest:scale(CCI) + 
                                        deciduous_forest:scale(CCI) + fresh_water:scale(CCI) + 
                                        grassland:scale(CCI) + grassy_heathland:scale(CCI) +
                                        grassy_heathland:scale(CCI) + heathland:scale(CCI) + 
                                        road:scale(CCI) + shrubland:scale(CCI) + swamp:scale(CCI), 
                                        data = RSFIntZeroUD, family = binomial(link = "logit"),
                                        offset = rep(qlogis(1/11), nrow(RSFIntZeroUD)))



# Interpret model
summary(Landuse_CCI_RSFIntZeroUD)

# Compare model
AIC(Landuse_Dummy_class_RSFIntZeroUD, Landuse_CCI_RSFIntZeroUD) 
# AIC reduced by 951.7 compared to first model


## Add interaction with season to observe effect influence of season on land use class selection

# Model with interactions between all seasons and land use classes
Landuse_CCI_season_RSFIntZeroUD <- glm(case ~  bare_soil + coniferous_forest + deciduous_forest +
                                  fresh_water + grassland + grassy_heathland +
                                  grassy_heathland + heathland + road +
                                  shrubland + swamp + bare_soil:scale(CCI) + coniferous_forest:scale(CCI) + 
                                  deciduous_forest:scale(CCI) + fresh_water:scale(CCI) + 
                                  grassland:scale(CCI) + grassy_heathland:scale(CCI) +
                                  grassy_heathland:scale(CCI) + heathland:scale(CCI) + 
                                  road:scale(CCI) + shrubland:scale(CCI) + swamp:scale(CCI) +
                                  bare_soil:summer + bare_soil:spring + bare_soil:autumn + bare_soil:winter+
                                  coniferous_forest:summer + coniferous_forest:spring + coniferous_forest:autumn + coniferous_forest:winter +
                                  deciduous_forest:summer + deciduous_forest:spring + deciduous_forest:autumn + deciduous_forest:winter + 
                                  fresh_water:summer + fresh_water:spring + fresh_water:autumn + fresh_water:winter +
                                  grassland:summer + grassland:spring + grassland:autumn + grassland:winter +
                                  heathland:summer + heathland:spring + heathland:autumn + heathland:winter +
                                  grassy_heathland:summer + grassy_heathland:spring + grassy_heathland:autumn + grassy_heathland:winter +
                                  road:summer + road:spring + road:autumn + road:winter +
                                  shrubland:summer + shrubland:spring + shrubland:autumn + shrubland:winter +
                                  swamp:summer + swamp:spring + swamp:autumn + swamp:winter, 
                                data = RSFIntZeroUD, family = binomial(link = "logit"),
                                offset = rep(qlogis(1/11), nrow(RSFIntZeroUD)))

# Remove influence seasons on selection bare soil
#Landuse_CCI_season_noBS_RSFIntZeroUD <- update(Landuse_CCI_season_RSFIntZeroUD, formula = ~ . - bare_soil:summer - bare_soil:spring - bare_soil:autumn - bare_soil:winter)

# Interpret model
#summary(Landuse_CCI_season_noBS_RSFIntZeroUD)

# Interpret model
summary(Landuse_CCI_season_RSFIntZeroUD)

# Compare models
AIC(Landuse_CCI_RSFIntZeroUD, Landuse_CCI_season_RSFIntZeroUD) 
# AIC reduced by 1077.9 compared to model with landuse classes and interaction with CCI
# AIC can be reduced by 4 if the interactions with bare soil are excluded. Safe that for later


## Add time of the day to the model

# Generate sequence with time (5min spacing)
times <- RSFIntZeroUD$time

# Convert to decimal time (0.0 = 0:00, 0.5 = 12:00, 1.0 = 24:00)
times_decimal <- as.numeric(difftime(times, floor_date(times, "days"), units="days"))

# Compute waveforms (decimal time converted to 2pi radians)
peakthrees <- sin(2*pi*times_decimal+(pi/4))                                    # 1 = 3:00, -1 = 15:00
peaksixes <- sin(2*pi*times_decimal)                                            # 1 = 6:00, -1 = 18:00
peaknines <- sin(2*pi*times_decimal-(pi/4))                                     # 1 = 9:00, -1 = 21:00
peaktwelves <- -cos(2*pi*times_decimal)                                         # 1 = 12:00, -1 = 00:00

# Add waveforms of time input data
RSFdaytime <- RSFIntZeroUD %>% 
  mutate(peakthrees = peakthrees,
         peaksixes = peaksixes,
         peaknines = peaknines,
         peaktwelves = peaktwelves)

# Create model with influence time of day on land use class selection
Landuse_CCI_season_daytime_RSFdaytime <- glm(case ~  bare_soil + coniferous_forest + deciduous_forest +
                                           fresh_water + grassland + grassy_heathland +
                                           grassy_heathland + heathland + road +
                                           shrubland + swamp + bare_soil:scale(CCI) + coniferous_forest:scale(CCI) + 
                                           deciduous_forest:scale(CCI) + fresh_water:scale(CCI) + 
                                           grassland:scale(CCI) + grassy_heathland:scale(CCI) +
                                           grassy_heathland:scale(CCI) + heathland:scale(CCI) + 
                                           road:scale(CCI) + shrubland:scale(CCI) + swamp:scale(CCI) +
                                           bare_soil:summer + bare_soil:spring + bare_soil:autumn + bare_soil:winter+
                                           coniferous_forest:summer + coniferous_forest:spring + coniferous_forest:autumn + coniferous_forest:winter +
                                           deciduous_forest:summer + deciduous_forest:spring + deciduous_forest:autumn + deciduous_forest:winter + 
                                           fresh_water:summer + fresh_water:spring + fresh_water:autumn + fresh_water:winter +
                                           grassland:summer + grassland:spring + grassland:autumn + grassland:winter +
                                           heathland:summer + heathland:spring + heathland:autumn + heathland:winter +
                                           grassy_heathland:summer + grassy_heathland:spring + grassy_heathland:autumn + grassy_heathland:winter +
                                           road:summer + road:spring + road:autumn + road:winter +
                                           shrubland:summer + shrubland:spring + shrubland:autumn + shrubland:winter +
                                           swamp:summer + swamp:spring + swamp:autumn + swamp:winter +
                                           bare_soil:peakthrees + bare_soil:peaksixes + bare_soil:peaknines + bare_soil:peaktwelves+
                                           coniferous_forest:peakthrees + coniferous_forest:peaksixes + coniferous_forest:peaknines + coniferous_forest:peaktwelves +
                                           deciduous_forest:peakthrees + deciduous_forest:peaksixes + deciduous_forest:peaknines + deciduous_forest:peaktwelves + 
                                           fresh_water:peakthrees + fresh_water:peaksixes + fresh_water:peaknines + fresh_water:peaktwelves +
                                           grassland:peakthrees + grassland:peaksixes + grassland:peaknines + grassland:peaktwelves +
                                           heathland:peakthrees + heathland:peaksixes + heathland:peaknines + heathland:peaktwelves +
                                           grassy_heathland:peakthrees + grassy_heathland:peaksixes + grassy_heathland:peaknines + grassy_heathland:peaktwelves +
                                           road:peakthrees + road:peaksixes + road:peaknines + road:peaktwelves +
                                           shrubland:peakthrees + shrubland:peaksixes + shrubland:peaknines + shrubland:peaktwelves +
                                           swamp:peakthrees + swamp:peaksixes + swamp:peaknines + swamp:peaktwelves, 
                                       data = RSFdaytime, family = binomial(link = "logit"),
                                       offset = rep(qlogis(1/11), nrow(RSFdaytime)))

# Interpret model
summary(Landuse_CCI_season_daytime_RSFdaytime)

# Compare models
AIC(Landuse_CCI_season_RSFIntZeroUD, Landuse_CCI_season_daytime_RSFdaytime) 
# AIC reduced by 3757.8 compared to model with landuse classes and interaction with CCI

# Remove previous models to create more disk space
rm(Landuse_class_RSF, Landuse_Dummy_class_RSF, CCI_in_Model_RSF, CCI_out_Model_RSF, 
   Landuse_class_RSFIntZeroUD, Landuse_Dummy_class_RSFIntZeroUD, Landuse_CCI_RSFIntZeroUD,
   Landuse_CCI_season_RSFIntZeroUD)

# Create model in which 30 variables with singularities are removed
Landuse_CCI_season_daytime_singrm_RSFdaytime <- glm(case ~  bare_soil + coniferous_forest + deciduous_forest +
                                               fresh_water + grassland + grassy_heathland +
                                               grassy_heathland + heathland + road +
                                               shrubland + swamp + bare_soil:scale(CCI) + coniferous_forest:scale(CCI) + 
                                               deciduous_forest:scale(CCI) + fresh_water:scale(CCI) + 
                                               grassland:scale(CCI) + grassy_heathland:scale(CCI) +
                                               grassy_heathland:scale(CCI) + heathland:scale(CCI) + 
                                               road:scale(CCI) + shrubland:scale(CCI) + swamp:scale(CCI) +
                                               bare_soil:summer + bare_soil:spring + bare_soil:winter+
                                               coniferous_forest:summer + coniferous_forest:spring + coniferous_forest:winter +
                                               deciduous_forest:summer + deciduous_forest:spring + deciduous_forest:winter + 
                                               fresh_water:summer + fresh_water:spring + fresh_water:winter +
                                               grassland:summer + grassland:spring + grassland:winter +
                                               heathland:summer + heathland:spring + heathland:winter +
                                               grassy_heathland:summer + grassy_heathland:spring + grassy_heathland:winter +
                                               road:summer + road:spring + road:winter +
                                               shrubland:summer + shrubland:spring + shrubland:winter +
                                               swamp:summer + swamp:spring + swamp:winter +
                                               bare_soil:peaksixes + bare_soil:peaktwelves+
                                               coniferous_forest:peaksixes + coniferous_forest:peaktwelves +
                                               deciduous_forest:peaksixes + deciduous_forest:peaktwelves + 
                                               fresh_water:peaksixes + fresh_water:peaktwelves +
                                               grassland:peaksixes + grassland:peaktwelves +
                                               heathland:peaksixes + heathland:peaktwelves +
                                               grassy_heathland:peaksixes + grassy_heathland:peaktwelves +
                                               road:peaksixes + road:peaktwelves +
                                               shrubland:peaksixes + shrubland:peaktwelves +
                                               swamp:peaksixes + swamp:peaktwelves, 
                                             data = RSFdaytime, family = binomial(link = "logit"),
                                             offset = rep(qlogis(1/11), nrow(RSFdaytime)))

# Interpret model
summary(Landuse_CCI_season_daytime_singrm_RSFdaytime)

# Compare models
AIC(Landuse_CCI_season_daytime_singrm_RSFdaytime, Landuse_CCI_season_daytime_RSFdaytime) 
# AIC is the same
rm(Landuse_CCI_season_daytime_RSFdaytime)

# Create model in which non significant variables are removed
significant_variables_RSF <- glm(case ~ coniferous_forest:scale(CCI) +
                                  deciduous_forest:scale(CCI) + fresh_water:scale(CCI) + 
                                  grassland:scale(CCI) + grassy_heathland:scale(CCI) +
                                  grassy_heathland:scale(CCI) + heathland:scale(CCI) + 
                                  road:scale(CCI) + swamp:scale(CCI) +
                                  coniferous_forest:summer + coniferous_forest:winter +
                                  deciduous_forest:summer + deciduous_forest:spring + deciduous_forest:winter + 
                                  fresh_water:summer + fresh_water:spring + 
                                  grassland:spring + grassland:winter +
                                  heathland:summer + heathland:spring + heathland:winter +
                                  grassy_heathland:summer + grassy_heathland:spring + grassy_heathland:winter +
                                  road:summer + road:winter +
                                  shrubland:summer + shrubland:spring + shrubland:winter +
                                  swamp:summer + swamp:spring + swamp:winter +
                                  bare_soil:peaktwelves+
                                  coniferous_forest:peaksixes + coniferous_forest:peaktwelves +
                                  deciduous_forest:peaksixes + deciduous_forest:peaktwelves + 
                                  fresh_water:peaksixes + fresh_water:peaktwelves +
                                  grassland:peaksixes + grassland:peaktwelves +
                                  heathland:peaksixes + heathland:peaktwelves +
                                  grassy_heathland:peaktwelves +
                                  road:peaktwelves +
                                  shrubland:peaksixes + shrubland:peaktwelves +
                                  swamp:peaksixes + swamp:peaktwelves, 
                                  data = RSFdaytime, family = binomial(link = "logit"),
                                  offset = rep(qlogis(1/11), nrow(RSFdaytime)))

# Interpret model
summary(significant_variables_RSF)

# Compare AIC
AIC(Landuse_CCI_season_daytime_singrm_RSFdaytime, significant_variables_RSF) 
# AIC is 7805.7 higher, so keep non-significant relations in
rm(significant_variables_RSF)

# Expand model with distances to water, forest and road
Landuse_CCI_season_daytime_singrm_distance_RSFdaytime <- glm(case ~  bare_soil + coniferous_forest + deciduous_forest +
                                                      fresh_water + grassland + grassy_heathland +
                                                      grassy_heathland + heathland + road +
                                                      shrubland + swamp + bare_soil:scale(CCI) + coniferous_forest:scale(CCI) + 
                                                      deciduous_forest:scale(CCI) + fresh_water:scale(CCI) + 
                                                      grassland:scale(CCI) + grassy_heathland:scale(CCI) +
                                                      grassy_heathland:scale(CCI) + heathland:scale(CCI) + 
                                                      road:scale(CCI) + shrubland:scale(CCI) + swamp:scale(CCI) +
                                                      bare_soil:summer + bare_soil:spring + bare_soil:winter+
                                                      coniferous_forest:summer + coniferous_forest:spring + coniferous_forest:winter +
                                                      deciduous_forest:summer + deciduous_forest:spring + deciduous_forest:winter + 
                                                      fresh_water:summer + fresh_water:spring + fresh_water:winter +
                                                      grassland:summer + grassland:spring + grassland:winter +
                                                      heathland:summer + heathland:spring + heathland:winter +
                                                      grassy_heathland:summer + grassy_heathland:spring + grassy_heathland:winter +
                                                      road:summer + road:spring + road:winter +
                                                      shrubland:summer + shrubland:spring + shrubland:winter +
                                                      swamp:summer + swamp:spring + swamp:winter +
                                                      bare_soil:peaksixes + bare_soil:peaktwelves+
                                                      coniferous_forest:peaksixes + coniferous_forest:peaktwelves +
                                                      deciduous_forest:peaksixes + deciduous_forest:peaktwelves + 
                                                      fresh_water:peaksixes + fresh_water:peaktwelves +
                                                      grassland:peaksixes + grassland:peaktwelves +
                                                      heathland:peaksixes + heathland:peaktwelves +
                                                      grassy_heathland:peaksixes + grassy_heathland:peaktwelves +
                                                      road:peaksixes + road:peaktwelves +
                                                      shrubland:peaksixes + shrubland:peaktwelves +
                                                      swamp:peaksixes + swamp:peaktwelves +
                                                      scale(log(1+ForestDistance)) + scale(log(1+WaterDistance)) +
                                                      scale(log(1+RoadDistance)), 
                                                    data = RSFdaytime, family = binomial(link = "logit"),
                                                    offset = rep(qlogis(1/11), nrow(RSFdaytime)))

# Interpret model
summary(Landuse_CCI_season_daytime_singrm_distance_RSFdaytime)

# Compare models
AIC(Landuse_CCI_season_daytime_singrm_RSFdaytime, Landuse_CCI_season_daytime_singrm_distance_RSFdaytime) 
# AIC is 271.2 lower

# Expand model with interactions between distances to water and forest with CCI
Landuse_CCI_season_daytime_singrm_distanceint_RSFdaytime <- glm(case ~  bare_soil + coniferous_forest + deciduous_forest +
                                                               fresh_water + grassland + grassy_heathland +
                                                               grassy_heathland + heathland + road +
                                                               shrubland + swamp + bare_soil:scale(CCI) + coniferous_forest:scale(CCI) + 
                                                               deciduous_forest:scale(CCI) + fresh_water:scale(CCI) + 
                                                               grassland:scale(CCI) + grassy_heathland:scale(CCI) +
                                                               grassy_heathland:scale(CCI) + heathland:scale(CCI) + 
                                                               road:scale(CCI) + shrubland:scale(CCI) + swamp:scale(CCI) +
                                                               bare_soil:summer + bare_soil:spring + bare_soil:winter+
                                                               coniferous_forest:summer + coniferous_forest:spring + coniferous_forest:winter +
                                                               deciduous_forest:summer + deciduous_forest:spring + deciduous_forest:winter + 
                                                               fresh_water:summer + fresh_water:spring + fresh_water:winter +
                                                               grassland:summer + grassland:spring + grassland:winter +
                                                               heathland:summer + heathland:spring + heathland:winter +
                                                               grassy_heathland:summer + grassy_heathland:spring + grassy_heathland:winter +
                                                               road:summer + road:spring + road:winter +
                                                               shrubland:summer + shrubland:spring + shrubland:winter +
                                                               swamp:summer + swamp:spring + swamp:winter +
                                                               bare_soil:peaksixes + bare_soil:peaktwelves+
                                                               coniferous_forest:peaksixes + coniferous_forest:peaktwelves +
                                                               deciduous_forest:peaksixes + deciduous_forest:peaktwelves + 
                                                               fresh_water:peaksixes + fresh_water:peaktwelves +
                                                               grassland:peaksixes + grassland:peaktwelves +
                                                               heathland:peaksixes + heathland:peaktwelves +
                                                               grassy_heathland:peaksixes + grassy_heathland:peaktwelves +
                                                               road:peaksixes + road:peaktwelves +
                                                               shrubland:peaksixes + shrubland:peaktwelves +
                                                               swamp:peaksixes + swamp:peaktwelves +
                                                               scale(log(1+ForestDistance)) + scale(log(1+WaterDistance)) +
                                                               scale(log(1+RoadDistance)) + scale(log(1+ForestDistance)):scale(CCI) +
                                                               scale(log(1+WaterDistance)):scale(CCI), 
                                                             data = RSFdaytime, family = binomial(link = "logit"),
                                                             offset = rep(qlogis(1/11), nrow(RSFdaytime)))

# Interpret model
summary(Landuse_CCI_season_daytime_singrm_distanceint_RSFdaytime)

# Compare models
AIC(Landuse_CCI_season_daytime_singrm_distance_RSFdaytime, Landuse_CCI_season_daytime_singrm_distanceint_RSFdaytime) 
# AIC is 637.1 lower
rm(Landuse_CCI_season_daytime_singrm_distance_RSFdaytime, Landuse_CCI_season_daytime_singrm_RSFdaytime)

# Expand model with interactions between landuse class and proportional availability
Landuse_CCI_season_daytime_singrm_distanceint_avail_RSFdaytime <- glm(case ~  bare_soil + coniferous_forest + deciduous_forest +
                                                                  fresh_water + grassland + grassy_heathland + heathland + road +
                                                                  shrubland + swamp + bare_soil:scale(CCI) + coniferous_forest:scale(CCI) + 
                                                                  deciduous_forest:scale(CCI) + fresh_water:scale(CCI) + 
                                                                  grassland:scale(CCI) + grassy_heathland:scale(CCI) +
                                                                  grassy_heathland:scale(CCI) + heathland:scale(CCI) + 
                                                                  road:scale(CCI) + shrubland:scale(CCI) + swamp:scale(CCI) +
                                                                  bare_soil:summer + bare_soil:spring + bare_soil:winter+
                                                                  coniferous_forest:summer + coniferous_forest:spring + coniferous_forest:winter +
                                                                  deciduous_forest:summer + deciduous_forest:spring + deciduous_forest:winter + 
                                                                  fresh_water:summer + fresh_water:spring + fresh_water:winter +
                                                                  grassland:summer + grassland:spring + grassland:winter +
                                                                  heathland:summer + heathland:spring + heathland:winter +
                                                                  grassy_heathland:summer + grassy_heathland:spring + grassy_heathland:winter +
                                                                  road:summer + road:spring + road:winter +
                                                                  shrubland:summer + shrubland:spring + shrubland:winter +
                                                                  swamp:summer + swamp:spring + swamp:winter +
                                                                  bare_soil:peaksixes + bare_soil:peaktwelves+
                                                                  coniferous_forest:peaksixes + coniferous_forest:peaktwelves +
                                                                  deciduous_forest:peaksixes + deciduous_forest:peaktwelves + 
                                                                  fresh_water:peaksixes + fresh_water:peaktwelves +
                                                                  grassland:peaksixes + grassland:peaktwelves +
                                                                  heathland:peaksixes + heathland:peaktwelves +
                                                                  grassy_heathland:peaksixes + grassy_heathland:peaktwelves +
                                                                  road:peaksixes + road:peaktwelves +
                                                                  shrubland:peaksixes + shrubland:peaktwelves +
                                                                  swamp:peaksixes + swamp:peaktwelves +
                                                                  scale(log(1+ForestDistance)) + scale(log(1+WaterDistance)) +
                                                                  scale(log(1+RoadDistance)) + scale(log(1+ForestDistance)):scale(CCI) +
                                                                  scale(log(1+WaterDistance)):scale(CCI) +
                                                                  bare_soil:baresoil_avail + coniferous_forest:conforest_avail + deciduous_forest:decforest_avail +
                                                                  fresh_water:freshwater_avail + grassland:grassland_avail + grassy_heathland:grassheath_avail +
                                                                  heathland:heathland_avail + road:road_avail +
                                                                  shrubland:shrub_avail + swamp:swamp_avail , 
                                                                data = RSFdaytime, family = binomial(link = "logit"),
                                                                offset = rep(qlogis(1/11), nrow(RSFdaytime)))


# Interpret model
summary(Landuse_CCI_season_daytime_singrm_distanceint_avail_RSFdaytime)

# Compare models
AIC(Landuse_CCI_season_daytime_singrm_distanceint_RSFdaytime, Landuse_CCI_season_daytime_singrm_distanceint_avail_RSFdaytime) 
# AIC is 6648.7 lower
rm(Landuse_CCI_season_daytime_singrm_distanceint_RSFdaytime)


## Updating last model to get best possible model
final_RSF <- update(Landuse_CCI_season_daytime_singrm_distanceint_avail_RSFdaytime, formula = ~ . -heathland:heathland_avail)
# AIC: 533674.1
final_RSF <- update(final_RSF, formula = ~ . -coniferous_forest:conforest_avail)
# AIC: 533674.8, less then 2 higher than the one before this one
final_RSF <- update(final_RSF, formula = ~ . -road:peaksixes)
# AIC: 533672.9, 1.2 lower than original model
final_RSF <- update(final_RSF, formula = ~ . -grassy_heathland:peaksixes)
# AIC: 533670.9, 2 lower than original model
final_RSF <- update(final_RSF, formula = ~ . -swamp:peaksixes)
# AIC: 533672.9, so I keep this variable in
final_RSF <- update(final_RSF, formula = ~ . +swamp:peaksixes)
# AIC: 533670.9
final_RSF <- update(final_RSF, formula = ~ . -coniferous_forest:peaksixes)
# 533674.2, so I should keep this variable in
final_RSF <- update(final_RSF, formula = ~ . +coniferous_forest:peaksixes)
# AIC: 533670.9
final_RSF <- update(final_RSF, formula = ~ . -bare_soil:peaksixes)
# AIC: 533669, 1.9 lower than previous model
final_RSF <- update(final_RSF, formula = ~ . -swamp:spring)
# AIC: 533669.3, which is slightly worse, but less than 2 difference, so I keep the variable out
final_RSF <- update(final_RSF, formula = ~ . -road:spring)
# AIC: 533670.5, which is slightly worse, but less than 2 difference, so I keep the variable out
final_RSF <- update(final_RSF, formula = ~ . -fresh_water:winter)
# AIC: 533669.1, so I keep the variable out
final_RSF <- update(final_RSF, formula = ~ . -deciduous_forest:spring)
# AIC: 533667.5, so I keep the variable out
final_RSF <- update(final_RSF, formula = ~ . -coniferous_forest:spring)
# AIC: 533665.8, so I keep the variable out
final_RSF <- update(final_RSF, formula = ~ . - bare_soil:summer - bare_soil:spring - bare_soil:winter)
# AIC: 533662.6, so I keep the variables out
final_RSF <- update(final_RSF, formula = ~ . -shrubland:scale(CCI))
# AIC: 533662.6, so I keep the variable out
final_RSF <- update(final_RSF, formula = ~ . -heathland:scale(CCI))
# AIC: 533662.1, so I keep the variable out
final_RSF <- update(final_RSF, formula = ~ . -deciduous_forest)
# AIC: 533660.1, so I keep the variable out
final_RSF <- update(final_RSF, formula = ~ . -road)
# AIC: 533658.2, so I keep the variable out
final_RSF <- update(final_RSF, formula = ~ . -shrubland)
# AIC: 533664.6, so I keep the variable in
final_RSF <- update(final_RSF, formula = ~ . +shrubland)
# AIC:533658.2
final_RSF <- update(final_RSF, formula = ~ . -coniferous_forest)
# AIC: 533716.2, so I need to keep it in
final_RSF <- update(final_RSF, formula = ~ . +coniferous_forest)
# AIC: 533658.2
final_RSF <- update(final_RSF, formula = ~ . +road:spring + swamp:spring + coniferous_forest:conforest_avail)
# AIC: 533656.8, together, these variables do not increase the model with 2 points, so I keep them out.
final_RSF <- update(final_RSF, formula = ~ . -road:spring - swamp:spring + coniferous_forest:conforest_avail)
# AIC: 533658.2
final_RSF <- update(final_RSF, formula = ~ . + bare_soil:day + coniferous_forest:day + deciduous_forest:day +
                      fresh_water:day + grassland:day + grassy_heathland:day +
                      heathland:day + road:day +
                      shrubland:day + swamp:day +
                      bare_soil:night + coniferous_forest:night + deciduous_forest:night +
                      fresh_water:night + grassland:night + grassy_heathland:night +
                      heathland:night + road:night +
                      shrubland:night + swamp:night)
# AIC: 533643.8
final_RSF <- update(final_RSF, formula = ~ . - bare_soil:night - coniferous_forest:night - fresh_water:night -
                      grassland:night - grassy_heathland:night - heathland:night -
                      shrubland:night - swamp:night)
# AIC: 533643.8
final_RSF <- update(final_RSF, formula = ~ . - bare_soil:day)
# AIC: 533641.8, so I keep the variable out
final_RSF <- update(final_RSF, formula = ~ . - grassland:day)
# AIC: 533639.8, so I keep the variable out
final_RSF <- update(final_RSF, formula = ~ . - road:night)
# AIC: 533637.8, so I keep the variable out
final_RSF <- update(final_RSF, formula = ~ . - deciduous_forest:day - grassy_heathland:day)
# AIC: 533634.2, so I keep the variables out
final_RSF <- update(final_RSF, formula = ~ . - road:day - swamp:day)
# AIC: 533631.4, so I keep the variables out
final_RSF <- update(final_RSF, formula = ~ . - coniferous_forest:peaksixes - fresh_water:day)
# AIC: 533628.4, so I keep the variables out
final_RSF <- update(final_RSF, formula = ~ . - shrubland:day)
# AIC: 533629.4, so I keep the variable out
final_RSF <- update(final_RSF, formula = ~ . - deciduous_forest:night + deciduous_forest:day)
# AIC: 533629.4, the same
final_RSF <- update(final_RSF, formula = ~ .  - deciduous_forest:day)
# AIC: 533630.1
final_RSF <- update(final_RSF, formula = ~ .  + deciduous_forest:day + shrubland:day)
# AIC: 533628.9, difference not big enough to keep variables in
final_RSF <- update(final_RSF, formula = ~ . - deciduous_forest:day - shrubland:day)
# AIC: 533630.1
final_RSF <- update(final_RSF, formula = ~ . + road:weekend)
# AIC: 533629.7, not improved enough
final_RSF <- update(final_RSF, formula = ~ . - road:weekend + scale(log(1+RoadDistance)):weekend)
# AIC: 533632.1, not improved
final_RSF <- update(final_RSF, formula = ~ . + road:weekend - scale(log(1+RoadDistance)):weekend +
                      + deciduous_forest:day + shrubland:day +road:spring + swamp:spring 
                    + coniferous_forest:conforest_avail)
# AIC: 533628, so difference not big enough. 6 variables improve the AIC with only 4 points
final_RSF <- update(final_RSF, formula = ~ . - road:weekend -
                    deciduous_forest:day - shrubland:day - road:spring - swamp:spring 
                    - coniferous_forest:conforest_avail)
# AIC: 533630.1







