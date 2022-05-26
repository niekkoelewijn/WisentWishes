# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to determine the influence of environmental variables on the Jacob's 
### index  of a landuse class.


## Temperature

# filter out track 38 and 39, as these tracks have no temp attribute
TempDataset <- AllTrackPoints %>% 
  filter(track_ID != 38 &
           track_ID != 39)

# get the lowest and highest 10% of the data
lowest10 <- as.numeric(quantile(TempDataset$temp, probs = seq(0, 1, 0.1))["10%"])
highest10 <- as.numeric(quantile(TempDataset$temp, probs = seq(0, 1, 0.1))["90%"])

# Filter TempDataset to get only the points with the lowest 10% temperatures
Lowest10 <- TempDataset %>% 
  filter(temp <= lowest10)

# Filter TempDataset to get only the points with the higest 10% temperatures
Highest10 <- TempDataset %>% 
  filter(temp > highest10)


## Calculate Jacob's index for lowest 10% temperatures

# Determine landuse class used 
Lowest10ClassUsed <- table(Lowest10$landuse_code)
NumberOfPoints <- length(Lowest10$landuse_code)

# Create table with proportion used per of landuse class
PropUsedLowest10 <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(PropUsedLowest10) <- as.character(LUTLanduseClasses$landuse_class)

# Fill empty table PropUsedLowest10 with proportion used per landuse class
for(i in seq_along(PropUsedLowest10)){
  if(is.na(Lowest10ClassUsed[as.character(i)])){
    Lowest10ClassUsed[as.character(i)] <- 0
  }
  PropUsedLowest10[1,i] <- Lowest10ClassUsed[as.character(i)] / NumberOfPoints
}

# Create empty table to store result
JILowest10 <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(JILowest10) <- LUTLanduseClasses$landuse_class

# Iterate over elements of JILowest10
for(i in seq_along(JILowest10)){
  JILowest10[i] <- JacobsIndex(as.numeric(PropUsedLowest10[i]), as.numeric(PropAvail[i]))
}

## Calculate Jacob's index for lowest 10% temperatures

# Determine landuse class used 
Highest10ClassUsed <- table(Highest10$landuse_code)
NumberOfPoints <- length(Highest10$landuse_code)

# Create table with proportion used per of landuse class
PropUsedHighest10 <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(PropUsedHighest10) <- as.character(LUTLanduseClasses$landuse_class)

# Fill empty table PropUsedHighest10 with proportion used per landuse class
for(i in seq_along(PropUsedHighest10)){
  if(is.na(Lowest10ClassUsed[as.character(i)])){
    Lowest10ClassUsed[as.character(i)] <- 0
  }
  PropUsedHighest10[1,i] <- Lowest10ClassUsed[as.character(i)] / NumberOfPoints
}

# Create empty table to store result
JIHighest10 <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(JIHighest10) <- LUTLanduseClasses$landuse_class

# Iterate over elements of JIHighest10
for(i in seq_along(JIHighest10)){
  JIHighest10[i] <- JacobsIndex(as.numeric(PropUsedHighest10[i]), as.numeric(PropAvail[i]))
}


## Create bar plots to compare lowest and highest 10% degrees celsius

# Make the data suitable for the visualization
Class <- colnames(JILowest10)
JI_value <- as.numeric(JILowest10)
JILowest10tibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JILowest10Vis <- ggplot(data = JILowest10tibble, aes(x = Class, y = JI_value, 
                                                   fill = Class, 
                                                   label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class lowest 10% temperature") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JILowest10Vis

# Make the data suitable for the visualization
Class <- colnames(JIHighest10)
JI_value <- as.numeric(JIHighest10)
JIHighest10tibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JIHighest10Vis <- ggplot(data = JIHighest10tibble, aes(x = Class, y = JI_value, 
                                                       fill = Class, 
                                                       label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class highest 10% temperature") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JIHighest10Vis


## Explore influence temperature on distance to water

# Get mean distance to water per temperature class, step size 0.1
lowest10 <- as.numeric(quantile(TempDataset$temp, probs = seq(0, 1, 0.1))["10%"])
temp20 <- as.numeric(quantile(TempDataset$temp, probs = seq(0, 1, 0.1))["20%"])
temp30 <- as.numeric(quantile(TempDataset$temp, probs = seq(0, 1, 0.1))["30%"])
temp40 <- as.numeric(quantile(TempDataset$temp, probs = seq(0, 1, 0.1))["40%"])
temp50 <- as.numeric(quantile(TempDataset$temp, probs = seq(0, 1, 0.1))["50%"])
temp60 <- as.numeric(quantile(TempDataset$temp, probs = seq(0, 1, 0.1))["60%"])
temp70 <- as.numeric(quantile(TempDataset$temp, probs = seq(0, 1, 0.1))["70%"])
temp80 <- as.numeric(quantile(TempDataset$temp, probs = seq(0, 1, 0.1))["80%"])
highest10 <- as.numeric(quantile(TempDataset$temp, probs = seq(0, 1, 0.1))["90%"])

# Filter TempDataset to get datasets with other all quantiles than the 
# lowest 10 and highest 10
Temp20 <- TempDataset %>% 
  filter(temp > lowest10 &
           temp <= temp20)

Temp30 <- TempDataset %>% 
  filter(temp > temp20 &
           temp <= temp30)

Temp40 <- TempDataset %>% 
  filter(temp > temp30 &
           temp <= temp40)

Temp50 <- TempDataset %>% 
  filter(temp > temp40 &
           temp <= temp50)

Temp60 <- TempDataset %>% 
  filter(temp > temp50 &
           temp <= temp60)

Temp70 <- TempDataset %>% 
  filter(temp > temp60 &
           temp <= temp70)

Temp80 <- TempDataset %>% 
  filter(temp > temp70 &
           temp <= temp80)

Temp90 <- TempDataset %>% 
  filter(temp > temp80 &
           temp <= highest10)

# Get mean distance to water for each temperature quantile
Lowest10MeanWaterDistance <- mean(Lowest10$WaterDistance)
Temp20MeanWaterDistance <- mean(Temp20$WaterDistance)
Temp30MeanWaterDistance <- mean(Temp30$WaterDistance)
Temp40MeanWaterDistance <- mean(Temp40$WaterDistance)
Temp50MeanWaterDistance <- mean(Temp50$WaterDistance)
Temp60MeanWaterDistance <- mean(Temp60$WaterDistance)
Temp70MeanWaterDistance <- mean(Temp70$WaterDistance)
Temp80MeanWaterDistance <- mean(Temp80$WaterDistance)
Temp90MeanWaterDistance <- mean(Temp90$WaterDistance)
Highest10MeanWaterDistance <- mean(Highest10$WaterDistance)

## Plot development mean distance to water

# Create tibble to prepare plotting
Classes <- c("<= 6°C", "<= 8°C", "<= 10°C", "<= 12°C", "<= 14°C",
             "<= 16°C", "<= 18°C", "<= 20°C", "<= 23°C", "> 23°C")
Quantiles <- c("10%", "20%", "30%", "40%", "50%",
               "60%", "70%", "80%", "90%", "> 90%")
MeanWaterDistance <- c(Lowest10MeanWaterDistance, Temp20MeanWaterDistance,
                       Temp30MeanWaterDistance, Temp40MeanWaterDistance,
                       Temp50MeanWaterDistance, Temp60MeanWaterDistance,
                       Temp70MeanWaterDistance, Temp80MeanWaterDistance,
                       Temp90MeanWaterDistance, Highest10MeanWaterDistance)
TempDistWater <- tibble(Classes, Quantiles, MeanWaterDistance)

# Visualize influence temperature on mean distance to water
TempDistWaterVis <- ggplot(data = TempDistWater, aes(x = Quantiles, y = MeanWaterDistance)) +
  geom_point(stat = "identity", shape = 16, size = 2,
             aes(color = MeanWaterDistance)) +
  geom_text(label = MeanWaterDistance, vjust = 2, size = 3) +
  geom_smooth(method="lm", se=F, fullrange=T, level=0.95,
              color = "black", size = 0.5) +
  theme_bw() +
  ylab("Mean distance to water") +
  xlab("Temperature classes") +
  ggtitle("Relation temperature and distance to water") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        legend.position = "none")
TempDistWaterVis




