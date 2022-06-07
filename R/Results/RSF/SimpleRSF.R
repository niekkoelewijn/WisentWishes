# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to set up first, simple, RSF for a specific time of the day (12:00 -
### 16:00) and a specific season (summer)


### Create random points for each study area


## Kraansvlak

# Observations
KraansvlakObs <- make_track(KraansvlakPoints, .x = "X", .y = "Y", .t = "time",
                            crs = 28992, all_cols = T)

# Select summer, between 12:00 and 16:00
KraansvlakSummerAfternoon <- KraansvlakObs %>% 
  filter(hms >= hms("12:00:00") & hms <= hms("16:00:00")) %>% 
  filter(season == "summer")

# RSF data
KraansvlakSummerAfternoonData <- KraansvlakSummerAfternoon %>% 
  
  # Create randomly 100 points per observation point
  random_points(n = 100*nrow(KraansvlakSummerAfternoon)) %>% 
  
  # Assign landuse class to the sampled points
  extract_covariates(MaskedList$Kraansvlak) %>% 
  
  # Remove points outside study area
  drop_na(LGN2020_5m) %>% 
  
  # Give the column with the landuse class the same name as the LUT
  dplyr::rename(landuse_code = LGN2020_5m) %>%
  
  # Join with the LUT, to get names of landuse classes
  inner_join(LUTLanduseClasses, by = "landuse_code")

# Visualize difference in preference per landuse class
KraansvlakSummerAfternoonData %>% 
  group_by(case_, landuse_class) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), 
         label = paste0(round(prop * 100, 1), "%")) %>% 
  ggplot(aes(landuse_class, prop, fill = case_, group=case_,label = label)) + 
  geom_col(position = position_dodge2()) +
  geom_text(size = 4, vjust = -0.25, position = position_dodge(width = 1)) +
  labs(x = "Land use class", y = "Proportion", fill = "case_")+
  scale_fill_brewer(palette = "Paired", name="case_", 
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used")) +
  theme_light()

# Create RSF
KraansvlakSummerAfternoonData$w <- ifelse(KraansvlakSummerAfternoonData$case_, 1, 5000)
KraansvlakSummerAfternoonRSF <- glm(case_ ~ landuse_class, 
                 data = KraansvlakSummerAfternoonData, weight = w,
                 family = binomial(link = "logit"))

# Interpret RSF
summary(KraansvlakSummerAfternoonRSF)

# View RSF
plot_model(KraansvlakSummerAfternoonRSF)


## Veluwe

# Sort on time
VeluweSorted <- VeluwePoints %>% 
  arrange(time)

# Observations
VeluweObs <- make_track(VeluweSorted, .x = "X", .y = "Y", .t = "time",
                            crs = 28992, all_cols = T)

# Select summer, between 12:00 and 16:00
VeluweSummerAfternoon <- VeluweObs %>% 
  filter(hms >= hms("12:00:00") & hms <= hms("16:00:00")) %>% 
  filter(season == "summer")

# RSF data
VeluweSummerAfternoonData <- VeluweSummerAfternoon %>% 
  
  # Create randomly 100 points per observation point
  random_points(n = 100*nrow(VeluweSummerAfternoon)) %>% 
  
  # Assign landuse class to the sampled points
  extract_covariates(MaskedList$Veluwe) %>% 
  
  # Remove points outside study area
  drop_na(LGN2020_5m) %>% 
  
  # Give the column with the landuse class the same name as the LUT
  dplyr::rename(landuse_code = LGN2020_5m) %>%
  
  # Join with the LUT, to get names of landuse classes
  inner_join(LUTLanduseClasses, by = "landuse_code")

# Visualize difference in preference per landuse class
VeluweSummerAfternoonData %>% 
  group_by(case_, landuse_class) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), 
         label = paste0(round(prop * 100, 1), "%")) %>% 
  ggplot(aes(landuse_class, prop, fill = case_, group=case_,label = label)) + 
  geom_col(position = position_dodge2()) +
  geom_text(size = 4, vjust = -0.25, position = position_dodge(width = 1)) +
  labs(x = "Land use class", y = "Proportion", fill = "case_")+
  scale_fill_brewer(palette = "Paired", name="case_", 
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used")) +
  theme_light()

# Create RSF
VeluweSummerAfternoonData$w <- ifelse(VeluweSummerAfternoonData$case_, 1, 5000)
VeluweSummerAfternoonRSF <- glm(case_ ~ landuse_class, 
                                    data = VeluweSummerAfternoonData, weight = w,
                                    family = binomial(link = "logit"))

# Interpret RSF
summary(VeluweSummerAfternoonRSF)

# View RSF
plot_model(VeluweSummerAfternoonRSF)

## VeluweHab

# Sort on time
VeluweHabSorted <- VeluweHabPoints %>% 
  arrange(time)

# Observations
VeluweHabObs <- make_track(VeluweHabSorted, .x = "X", .y = "Y", .t = "time",
                           crs = 28992, all_cols = T)

# Select summer, between 12:00 and 16:00
VeluweHabSummerAfternoon <- VeluweHabObs %>% 
  filter(hms >= hms("12:00:00") & hms <= hms("16:00:00")) %>% 
  filter(season == "summer")

# RSF data
VeluweHabSummerAfternoonData <- VeluweHabSummerAfternoon %>% 
  
  # Create randomly 100 points per observation point
  random_points(n = 100*nrow(VeluweHabSummerAfternoon)) %>% 
  
  # Assign landuse class to the sampled points
  extract_covariates(MaskedList$VeluweHab) %>% 
  
  # Remove points outside study area
  drop_na(LGN2020_5m) %>% 
  
  # Give the column with the landuse class the same name as the LUT
  dplyr::rename(landuse_code = LGN2020_5m) %>%
  
  # Join with the LUT, to get names of landuse classes
  inner_join(LUTLanduseClasses, by = "landuse_code")

# Visualize difference in preference per landuse class
VeluweHabSummerAfternoonData %>% 
  group_by(case_, landuse_class) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), 
         label = paste0(round(prop * 100, 1), "%")) %>% 
  ggplot(aes(landuse_class, prop, fill = case_, group=case_,label = label)) + 
  geom_col(position = position_dodge2()) +
  geom_text(size = 4, vjust = -0.25, position = position_dodge(width = 1)) +
  labs(x = "Land use class", y = "Proportion", fill = "case_")+
  scale_fill_brewer(palette = "Paired", name="case_", 
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used")) +
  theme_light()

# Create RSF
VeluweHabSummerAfternoonData$w <- ifelse(VeluweHabSummerAfternoonData$case_, 1, 5000)
VeluweHabSummerAfternoonRSF <- glm(case_ ~ landuse_class, 
                                   data = VeluweHabSummerAfternoonData, weight = w,
                                   family = binomial(link = "logit"))

# Interpret RSF
summary(VeluweHabSummerAfternoonRSF)

# View RSF
plot_model(VeluweHabSummerAfternoonRSF)


## Maashorst20172021

# Sort on time
Maashorst20172021Sorted <- Maashorst20172021Points %>% 
  arrange(time)

# Observations
Maashorst20172021Obs <- make_track(Maashorst20172021Sorted, .x = "X", .y = "Y", .t = "time",
                                   crs = 28992, all_cols = T)

# Select summer, between 12:00 and 16:00
Maashorst20172021SummerAfternoon <- Maashorst20172021Obs %>% 
  filter(hms >= hms("12:00:00") & hms <= hms("16:00:00")) %>% 
  filter(season == "summer")

# RSF data
Maashorst20172021SummerAfternoonData <- Maashorst20172021SummerAfternoon %>% 
  
  # Create randomly 100 points per observation point
  random_points(n = 100*nrow(Maashorst20172021SummerAfternoon)) %>% 
  
  # Assign landuse class to the sampled points
  extract_covariates(MaskedList$Maashorst20172021) %>% 
  
  # Remove points outside study area
  drop_na(LGN2020_5m) %>% 
  
  # Give the column with the landuse class the same name as the LUT
  dplyr::rename(landuse_code = LGN2020_5m) %>%
  
  # Join with the LUT, to get names of landuse classes
  inner_join(LUTLanduseClasses, by = "landuse_code")

# Visualize difference in preference per landuse class
Maashorst20172021SummerAfternoonData %>% 
  group_by(case_, landuse_class) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), 
         label = paste0(round(prop * 100, 1), "%")) %>% 
  ggplot(aes(landuse_class, prop, fill = case_, group=case_,label = label)) + 
  geom_col(position = position_dodge2()) +
  geom_text(size = 4, vjust = -0.25, position = position_dodge(width = 1)) +
  labs(x = "Land use class", y = "Proportion", fill = "case_")+
  scale_fill_brewer(palette = "Paired", name="case_", 
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used")) +
  theme_light()

# Create RSF
Maashorst20172021SummerAfternoonData$w <- ifelse(Maashorst20172021SummerAfternoonData$case_, 1, 5000)
Maashorst20172021SummerAfternoonRSF <- glm(case_ ~ landuse_class, 
                                           data = Maashorst20172021SummerAfternoonData, weight = w,
                                           family = binomial(link = "logit"))

# Interpret RSF
summary(Maashorst20172021SummerAfternoonRSF)

# View RSF
plot_model(Maashorst20172021SummerAfternoonRSF)

## SlikkenvdHeenHab

# Sort on time
SlikkenvdHeenHabSorted <- SlikkenvdHeenHabPoints %>% 
  arrange(time)

# Observations
SlikkenvdHeenHabObs <- make_track(SlikkenvdHeenHabSorted, .x = "X", .y = "Y", .t = "time",
                                  crs = 28992, all_cols = T)

# Select summer, between 12:00 and 16:00
SlikkenvdHeenHabSummerAfternoon <- SlikkenvdHeenHabObs %>% 
  filter(hms >= hms("12:00:00") & hms <= hms("16:00:00")) %>% 
  filter(season == "summer")

# RSF data
SlikkenvdHeenHabSummerAfternoonData <- SlikkenvdHeenHabSummerAfternoon %>% 
  
  # Create randomly 100 points per observation point
  random_points(n = 100*nrow(SlikkenvdHeenHabSummerAfternoon)) %>% 
  
  # Assign landuse class to the sampled points
  extract_covariates(MaskedList$SlikkenvdHeenHab) %>% 
  
  # Remove points outside study area
  drop_na(LGN2020_5m) %>% 
  
  # Give the column with the landuse class the same name as the LUT
  dplyr::rename(landuse_code = LGN2020_5m) %>%
  
  # Join with the LUT, to get names of landuse classes
  inner_join(LUTLanduseClasses, by = "landuse_code")

# Visualize difference in preference per landuse class
SlikkenvdHeenHabSummerAfternoonData %>% 
  group_by(case_, landuse_class) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), 
         label = paste0(round(prop * 100, 1), "%")) %>% 
  ggplot(aes(landuse_class, prop, fill = case_, group=case_,label = label)) + 
  geom_col(position = position_dodge2()) +
  geom_text(size = 4, vjust = -0.25, position = position_dodge(width = 1)) +
  labs(x = "Land use class", y = "Proportion", fill = "case_")+
  scale_fill_brewer(palette = "Paired", name="case_", 
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used")) +
  theme_light()

# Create RSF
SlikkenvdHeenHabSummerAfternoonData$w <- ifelse(SlikkenvdHeenHabSummerAfternoonData$case_, 1, 5000)
SlikkenvdHeenHabSummerAfternoonRSF <- glm(case_ ~ landuse_class, 
                                          data = SlikkenvdHeenHabSummerAfternoonData, weight = w,
                                          family = binomial(link = "logit"))

# Interpret RSF
summary(SlikkenvdHeenHabSummerAfternoonRSF)

# View RSF
plot_model(SlikkenvdHeenHabSummerAfternoonRSF)

## SlikkenvdHeen

# Sort on time
SlikkenvdHeenSorted <- SlikkenvdHeenPoints %>% 
  arrange(time)

# Observations
SlikkenvdHeenObs <- make_track(SlikkenvdHeenSorted, .x = "X", .y = "Y", .t = "time",
                               crs = 28992, all_cols = T)

# Select summer, between 12:00 and 16:00
SlikkenvdHeenSummerAfternoon <- SlikkenvdHeenObs %>% 
  filter(hms >= hms("12:00:00") & hms <= hms("16:00:00")) %>% 
  filter(season == "summer")

# RSF data
SlikkenvdHeenSummerAfternoonData <- SlikkenvdHeenSummerAfternoon %>% 
  
  # Create randomly 100 points per observation point
  random_points(n = 100*nrow(SlikkenvdHeenSummerAfternoon)) %>% 
  
  # Assign landuse class to the sampled points
  extract_covariates(MaskedList$SlikkenvdHeen) %>% 
  
  # Remove points outside study area
  drop_na(LGN2020_5m) %>% 
  
  # Give the column with the landuse class the same name as the LUT
  dplyr::rename(landuse_code = LGN2020_5m) %>%
  
  # Join with the LUT, to get names of landuse classes
  inner_join(LUTLanduseClasses, by = "landuse_code")

# Visualize difference in preference per landuse class
SlikkenvdHeenSummerAfternoonData %>% 
  group_by(case_, landuse_class) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), 
         label = paste0(round(prop * 100, 1), "%")) %>% 
  ggplot(aes(landuse_class, prop, fill = case_, group=case_,label = label)) + 
  geom_col(position = position_dodge2()) +
  geom_text(size = 4, vjust = -0.25, position = position_dodge(width = 1)) +
  labs(x = "Land use class", y = "Proportion", fill = "case_")+
  scale_fill_brewer(palette = "Paired", name="case_", 
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used")) +
  theme_light()

# Create RSF
SlikkenvdHeenSummerAfternoonData$w <- ifelse(SlikkenvdHeenSummerAfternoonData$case_, 1, 5000)
SlikkenvdHeenSummerAfternoonRSF <- glm(case_ ~ landuse_class, 
                                       data = SlikkenvdHeenSummerAfternoonData, weight = w,
                                       family = binomial(link = "logit"))

# Interpret RSF
summary(SlikkenvdHeenSummerAfternoonRSF)

# View RSF
plot_model(SlikkenvdHeenSummerAfternoonRSF)


## Create overall RSF dataset

# Bind study areas
AllPointsSummerAfternoonRSFdata <- rbind(KraansvlakSummerAfternoonData, VeluweSummerAfternoonData,
                                     VeluweHabSummerAfternoonData, SlikkenvdHeenHabSummerAfternoonData,
                                     SlikkenvdHeenSummerAfternoonData, Maashorst20172021SummerAfternoonData)

# Create RSF
AllPointsSummerAfternoonRSF <- glm(case_ ~ landuse_class, 
                                       data = AllPointsSummerAfternoonRSFdata, weight = w,
                                       family = "binomial")

# Interpret RSF
summary(AllPointsSummerAfternoonRSF)

# View RSF
plot_model(AllPointsSummerAfternoonRSF)

