# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to visualize the results of the Jacob index calculations


## Jacobs index overall

# Make the data suitable for the visualization
Class <- colnames(JIOverall)
JI_value <- as.numeric(JIOverall)
JIOveralltibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JIOverallVis <- ggplot(data = JIOveralltibble, aes(x = Class, y = JI_value, 
                                                   fill = Class, 
                                                   label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class for all points") +
  theme(plot.title = element_text(size = 15, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JIOverallVis


## Jacobs index per study area

# Make the data suitable for the visualization
JIPerSAVis <- JacobsIndexPerStudyArea %>%
  select(`Study area`, grassland, `deciduous forest`,
         `coniferous forest`, `fresh water`, road,
         `bare soil`, shrubland, heathland,
         `grassy heathland`, swamp)

rownames(JIPerSAVis) = JIPerSAVis$`Study area`

# remove Study Area from JIPerSAVis
JIPerSAV <- JIPerSAVis %>% 
  select(-`Study area`) 

# Add rownames to JIPerSAV
rownames(JIPerSAV) <- JIPerSAVis$`Study area`

# Adapt data to Study Areas ready for ggplot
StudyAreaData <- t(JIPerSAV)
StudyAreaDataMelt <- melt(StudyAreaData, id = "Class")
names(StudyAreaDataMelt) <- c("Class", "StudyArea", "JI")

# Visualization of all study areas JI in 1 plot
StudyAreasJI <- ggplot(data = StudyAreaDataMelt, aes(x = Class, y = JI,
                                                     fill = StudyArea)) +
  geom_point(stat = "identity", shape = 16, size = 2,
             aes(color = StudyArea)) +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class per study area") +
  theme(plot.title = element_text(size = 10, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.title = element_text(size = 8))
StudyAreasJI

# Visualization of all study areas JI in 1 plot
StudyAreasJIBox <- ggplot(data = StudyAreaDataMelt, aes(x = Class, y = JI,
                                                     fill = Class)) +
  geom_boxplot(stat = "boxplot") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class per study area") +
  theme(plot.title = element_text(size = 10, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "none")
StudyAreasJIBox

# Adapt data to Kraansvlak
KraansvlakData <- as.numeric(JIPerSAVis[1,2:ncol(JIPerSAVis)])
Class <- colnames(JIPerSAVis)[2:ncol(JIPerSAVis)]
KraansvlakVisTibble <- tibble(KraansvlakData, Class)

# Kraansvlak visualization
KraansvlakVis <- ggplot(data = KraansvlakVisTibble, aes(x = Class, y = KraansvlakData, 
                                          fill = Class, 
                                          label=sprintf("%0.2f", round(KraansvlakData, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class in the Kraansvlak") +
  theme(plot.title = element_text(size = 15, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
KraansvlakVis

# Adapt data to Maashorst 2016
Maashorst2016Data <- as.numeric(JIPerSAVis[2,2:ncol(JIPerSAVis)])
Class <- colnames(JIPerSAVis)[2:ncol(JIPerSAVis)]
Maashorst2016VisTibble <- tibble(Maashorst2016Data, Class)

# Maashorst 2016 visualisation
Maashorst2016Vis <- ggplot(data = Maashorst2016VisTibble, aes(x = Class, y = Maashorst2016Data, 
                                                        fill = Class, 
                                                        label=sprintf("%0.2f", round(Maashorst2016Data, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class in the Maashorst (2016)") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
Maashorst2016Vis

# Adapt data to Maashorst 2017 - 2021
Maashorst20172021Data <- as.numeric(JIPerSAVis[3,2:ncol(JIPerSAVis)])
Class <- colnames(JIPerSAVis)[2:ncol(JIPerSAVis)]
Maashorst20172021VisTibble <- tibble(Maashorst20172021Data, Class)

# Maashorst 2017 - 2021 visualisation
Maashorst20172021Vis <- ggplot(data = Maashorst20172021VisTibble, aes(x = Class, y = Maashorst20172021Data, 
                                                                      fill = Class, 
                                                                      label=sprintf("%0.2f", round(Maashorst20172021Data, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class in the Maashorst (2017 - 2021)") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
Maashorst20172021Vis

# Adapt data to Maashorst 2022
Maashorst2022Data <- as.numeric(JIPerSAVis[4,2:ncol(JIPerSAVis)])
Class <- colnames(JIPerSAVis)[2:ncol(JIPerSAVis)]
Maashorst2022VisTibble <- tibble(Maashorst2022Data, Class)

# Maashorst 2022 visualisation
Maashorst2022Vis <- ggplot(data = Maashorst2022VisTibble, aes(x = Class, y = Maashorst2022Data, 
                                                              fill = Class, 
                                                              label=sprintf("%0.2f", round(Maashorst2022Data, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class in the Maashorst (2022)") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
Maashorst2022Vis

# Adapt data to Slikken vd Heen habituate area
SlikkenvdHeenHabData <- as.numeric(JIPerSAVis[5,2:ncol(JIPerSAVis)])
Class <- colnames(JIPerSAVis)[2:ncol(JIPerSAVis)]
SlikkenvdHeenHabVisTibble <- tibble(SlikkenvdHeenHabData, Class)

# Slikken vd Heen habituate area visualisation
SlikkenvdHeenHabVis <- ggplot(data = SlikkenvdHeenHabVisTibble, aes(x = Class, y = SlikkenvdHeenHabData, 
                                                                    fill = Class, 
                                                                    label=sprintf("%0.2f", round(SlikkenvdHeenHabData, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class in in the Slikken vd Heen habituate area") +
  theme(plot.title = element_text(size = 10, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
SlikkenvdHeenHabVis

# Adapt data to Slikken vd Heen
SlikkenvdHeenData <- as.numeric(JIPerSAVis[6,2:ncol(JIPerSAVis)])
Class <- colnames(JIPerSAVis)[2:ncol(JIPerSAVis)]
SlikkenvdHeenVisTibble <- tibble(SlikkenvdHeenData, Class)

# Slikken vd Heen visualisation
SlikkenvdHeenVis <- ggplot(data = SlikkenvdHeenVisTibble, aes(x = Class, y = SlikkenvdHeenData, 
                                                              fill = Class, 
                                                              label=sprintf("%0.2f", round(SlikkenvdHeenData, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class in the Slikken vd Heen") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
SlikkenvdHeenVis

# Adapt data to Veluwe habituate area
VeluweHabData <- as.numeric(JIPerSAVis[7,2:ncol(JIPerSAVis)])
Class <- colnames(JIPerSAVis)[2:ncol(JIPerSAVis)]
VeluweHabVisTibble <- tibble(VeluweHabData, Class)

# Veluwe habituate area visualisation
VeluweHabVis <- ggplot(data = VeluweHabVisTibble, aes(x = Class, y = VeluweHabData, 
                                                      fill = Class, 
                                                      label=sprintf("%0.2f", round(VeluweHabData, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class in the Veluwe habituate area") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
VeluweHabVis

# Adapt data to Veluwe
VeluweData <- as.numeric(JIPerSAVis[8,2:ncol(JIPerSAVis)])
Class <- colnames(JIPerSAVis)[2:ncol(JIPerSAVis)]
VeluweVisTibble <- tibble(VeluweData, Class)

# Veluwe visualisation
VeluweVis <- ggplot(data = VeluweVisTibble, aes(x = Class, y = VeluweData, 
                                                fill = Class, 
                                                label=sprintf("%0.2f", round(VeluweData, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class in the Veluwe") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
VeluweVis


## Plot relationship Jacobs index and the proportional availability of a landuse class

JI_PropAvail <- JI_PropAvail %>% 
  mutate(`PA woodland` = `PA deciduous forest` + `PA shrubland` + `PA coniferous forest`,
         `PA open land (dry)` = `PA grass` + `PA bare soil` + `PA heathland` + `PA grassy heathland`,
         `PA open land (wet)` = `PA fresh water` + `PA swamp`)


# Grass
grass_JIAvail <- ggplot(data = JI_PropAvail, aes(x = `PA open land (dry)`, y = `JI grass`)) +
  geom_point(stat = "identity", shape = 16, size = 2,
             aes(color = factor(StudyAreaNames))) +
  geom_text(label = StudyAreaNames, vjust = 2, size = 3) +
  geom_smooth(method="lm", se=F, fullrange=T, level=0.95,
              color = "black", size = 0.5) +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Proportional availability grassland") +
  ggtitle("Relation Jacobs index and availability of grassland") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        legend.position = "none")
grass_JIAvail

# Deciduous forest
deciduous_forestJIAvail <- ggplot(data = JI_PropAvail, aes(x = `PA deciduous forest`, y = `JI deciduous forest`)) +
  geom_point(stat = "identity", shape = 16, size = 2,
             aes(color = factor(StudyAreaNames))) +
  geom_text(label = StudyAreaNames, vjust = 2, size = 3) +
  geom_smooth(method="lm", se=F, fullrange=T, level=0.95,
              color = "black", size = 0.5) +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Proportional availability deciduous forest") +
  ggtitle("Relation Jacobs index and availability of deciduous forest") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        legend.position = "none")
deciduous_forestJIAvail

StudyAreaNames <- c("Overall", "Kraansvlak", "Maashorst 2016", "Maashorst 2017-2021", "Maashorst 2022",
                    "Slikken vd Heen Habituate", "Slikken vd Heen", "Veluwe Habituate",
                    "Veluwe")

# Coniferous forest
coniferous_forestJIAvail <- ggplot(data = JI_PropAvail, aes(x = `PA woodland`, y = `JI coniferous forest`)) +
  geom_point(stat = "identity", shape = 16, size = 2,
             aes(color = factor(StudyAreaNames))) +
  geom_text(label = StudyAreaNames, vjust = 2, size = 3) +
  geom_smooth(method="lm", se=F, fullrange=T, level=0.95,
              color = "black", size = 0.5) +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Proportional availability coniferous forest") +
  ggtitle("Relation Jacobs index and availability of coniferous forest") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        legend.position = "none")
coniferous_forestJIAvail

# Fresh water
freshwater_JIAvail <- ggplot(data = JI_PropAvail, aes(x = car::logit(`PA open land (wet)`), y = `JI fresh water`)) +
  geom_point(stat = "identity", shape = 16, size = 2,
             aes(color = factor(StudyAreaNames))) +
  geom_text(label = StudyAreaNames, vjust = 2, size = 3) +
  geom_smooth(method="lm", se=F, fullrange=T, level=0.95,
              color = "black", size = 0.5) +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Proportional availability fresh water (logit)") +
  ggtitle("Relation Jacobs index and availability of fresh water") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        legend.position = "none")
freshwater_JIAvail

# Road
roadJIAvail <- ggplot(data = JI_PropAvail, aes(x = `PA road`, y = `JI road`)) +
  geom_point(stat = "identity", shape = 16, size = 2,
             aes(color = factor(rownames(JI_PropAvail)))) +
  geom_text(label = rownames(JI_PropAvail), vjust = 2, size = 3) +
  geom_smooth(method="lm", se=F, fullrange=T, level=0.95,
              color = "black", size = 0.5) +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Proportional availability road") +
  ggtitle("Relation Jacobs index and availability of road") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        legend.position = "none")
roadJIAvail

# Bare soil
bare_soilJIAvail <- ggplot(data = JI_PropAvail, aes(x = `PA bare soil`, y = `JI bare soil`)) +
  geom_point(stat = "identity", shape = 16, size = 2,
             aes(color = factor(rownames(JI_PropAvail)))) +
  geom_text(label = rownames(JI_PropAvail), vjust = 2, size = 3) +
  geom_smooth(method="lm", se=F, fullrange=T, level=0.95,
              color = "black", size = 0.5) +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Proportional availability bare soil") +
  ggtitle("Relation Jacobs index and availability of bare soil") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        legend.position = "none")
bare_soilJIAvail

# Swamp
swamp_JIAvail <- ggplot(data = JI_PropAvail, aes(x = `PA open land (wet)`, y = `JI swamp`)) +
  geom_point(stat = "identity", shape = 16, size = 2,
             aes(color = factor(StudyAreaNames))) +
  geom_text(label = StudyAreaNames, vjust = 2, size = 3) +
  geom_smooth(method="lm", se=F, fullrange=T, level=0.95,
              color = "black", size = 0.5) +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Proportional availability swamp (logit)") +
  ggtitle("Relation Jacobs index and availability of swamp") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        legend.position = "none")
swamp_JIAvail

# Shrubland
shrublandJIAvail <- ggplot(data = JI_PropAvail, aes(x = `PA shrubland`, y = `JI shrubland`)) +
  geom_point(stat = "identity", shape = 16, size = 2,
             aes(color = factor(rownames(JI_PropAvail)))) +
  geom_text(label = rownames(JI_PropAvail), vjust = 2, size = 3) +
  geom_smooth(method="lm", se=F, fullrange=T, level=0.95,
              color = "black", size = 0.5) +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Proportional availability shrubland") +
  ggtitle("Relation Jacobs index and availability of shrubland") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        legend.position = "none")
shrublandJIAvail

# Heathland
heathlandJIAvail <- ggplot(data = JI_PropAvail, aes(x = `PA heathland`, y = `JI heathland`)) +
  geom_point(stat = "identity", shape = 16, size = 2,
             aes(color = factor(rownames(JI_PropAvail)))) +
  geom_text(label = rownames(JI_PropAvail), vjust = 2, size = 3) +
  geom_smooth(method="lm", se=F, fullrange=T, level=0.95,
              color = "black", size = 0.5) +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Proportional availability heathland") +
  ggtitle("Relation Jacobs index and availability of heathland") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        legend.position = "none")
heathlandJIAvail

# Grassy heathland
grassy_heathlandJIAvail <- ggplot(data = JI_PropAvail, aes(x = `PA grassy heathland`, y = `JI grassy heathland`)) +
  geom_point(stat = "identity", shape = 16, size = 2,
             aes(color = factor(rownames(JI_PropAvail)))) +
  geom_text(label = rownames(JI_PropAvail), vjust = 2, size = 3) +
  geom_smooth(method="lm", se=F, fullrange=T, level=0.95,
              color = "black", size = 0.5) +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Proportional availability grassy heathland") +
  ggtitle("Relation Jacobs index and availability of grassy heathland") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        legend.position = "none")
grassy_heathlandJIAvail

plot(x = JI_PropAvail$`PA grass`, y = JI_PropAvail$`JI grass`,
     xlab = "Proportion available", ylab = "Jacobs index", 
     ylim = c(-1, 1),
     main = "Relation Jacobs index and availability of grass",
     cex.main = 1)
text(JI_PropAvail$`PA grass`, JI_PropAvail$`JI grass`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Deciduous forest
plot(x = JI_PropAvail$`PA deciduous forest`, y = JI_PropAvail$`JI deciduous forest`,
     xlab = "Proportion available", ylab = "Jacobs index", 
     ylim = c(-1, 1),
     main = "Relation Jacobs index and availability of deciduous forest",
     cex.main = 1)
text(JI_PropAvail$`PA deciduous forest`, JI_PropAvail$`JI deciduous forest`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Coniferous forest
plot(x = JI_PropAvail$`PA coniferous forest`, y = JI_PropAvail$`JI coniferous forest`,
     xlab = "Proportion available", ylab = "Jacobs index", 
     ylim = c(-1, 1),
     main = "Relation Jacobs index and availability of coniferous forest",
     cex.main = 1)
text(JI_PropAvail$`PA coniferous forest`, JI_PropAvail$`JI coniferous forest`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Fresh water
plot(x = JI_PropAvail$`PA fresh water`, y = JI_PropAvail$`JI fresh water`,
     ylim = c(-1, 1),
     xlab = "Proportion available", ylab = "Jacobs index", 
     main = "Relation Jacobs index and availability of fresh water",
     cex.main = 1)
text(JI_PropAvail$`PA fresh water`, JI_PropAvail$`JI fresh water`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Road
plot(x = JI_PropAvail$`PA road`, y = JI_PropAvail$`JI road`,
     ylim = c(-1, 1),
     xlab = "Proportion available", ylab = "Jacobs index", 
     main = "Relation Jacobs index and availability of road",
     cex.main = 1)
text(JI_PropAvail$`PA road`, JI_PropAvail$`JI road`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Bare soil
plot(x = JI_PropAvail$`PA bare soil`, y = JI_PropAvail$`JI bare soil`,
     ylim = c(-1, 1),
     xlab = "Proportion available", ylab = "Jacobs index", 
     main = "Relation Jacobs index and availability of bare soil",
     cex.main = 1)
text(JI_PropAvail$`PA bare soil`, JI_PropAvail$`JI bare soil`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Swamp
plot(x = JI_PropAvail$`PA swamp`, y = JI_PropAvail$`JI swamp`,
     ylim = c(-1, 1),
     xlab = "Proportion available", ylab = "Jacobs index", 
     main = "Relation Jacobs index and availability of swamp",
     cex.main = 1)
text(JI_PropAvail$`PA swamp`, JI_PropAvail$`JI swamp`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Shrubland
plot(x = JI_PropAvail$`PA shrubland`, y = JI_PropAvail$`JI shrubland`,
     ylim = c(-1, 1),
     xlab = "Proportion available", ylab = "Jacobs index", 
     main = "Relation Jacobs index and availability of shrubland",
     cex.main = 1)
text(JI_PropAvail$`PA shrubland`, JI_PropAvail$`JI shrubland`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Heathland
plot(x = JI_PropAvail$`PA heathland`, y = JI_PropAvail$`JI heathland`,
     ylim = c(-1, 1),
     xlab = "Proportion available", ylab = "Jacobs index", 
     main = "Relation Jacobs index and availability of heathland",
     cex.main = 1)
text(JI_PropAvail$`PA heathland`, JI_PropAvail$`JI heathland`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Grassy heathland
plot(x = JI_PropAvail$`PA grassy heathland`, y = JI_PropAvail$`JI grassy heathland`,
     ylim = c(-1, 1),
     xlab = "Proportion available", ylab = "Jacobs index", 
     main = "Relation Jacobs index and availability of grassy heathland",
     cex.main = 1)
text(JI_PropAvail$`PA grassy heathland`, JI_PropAvail$`JI grassy heathland`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

