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
         `grassy heathland`)

# Adapt data to Kraansvlak
KraansvlakData <- as.numeric(JIPerSAVis[1,2:ncol(JIPerSAVis)])
Class <- colnames(JIPerSAVis)[2:ncol(JIPerSAVis)]
KraansvlakVisTibble <- tibble(KraansvlakData, Class)

# Kraansvlak visualisation
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

## Plot relations

# Grass
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

