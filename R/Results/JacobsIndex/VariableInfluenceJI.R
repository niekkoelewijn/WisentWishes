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
  if(is.na(Highest10ClassUsed[as.character(i)])){
    Highest10ClassUsed[as.character(i)] <- 0
  }
  PropUsedHighest10[1,i] <- Highest10ClassUsed[as.character(i)] / NumberOfPoints
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


## Explore influence temperature on distances

# Get mean distances per temperature class, step size 0.1
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

# Get mean distance to forest for each temperature quantile
Lowest10MeanForestDistance <- mean(Lowest10$ForestDistance)
Temp20MeanForestDistance <- mean(Temp20$ForestDistance)
Temp30MeanForestDistance <- mean(Temp30$ForestDistance)
Temp40MeanForestDistance <- mean(Temp40$ForestDistance)
Temp50MeanForestDistance <- mean(Temp50$ForestDistance)
Temp60MeanForestDistance <- mean(Temp60$ForestDistance)
Temp70MeanForestDistance <- mean(Temp70$ForestDistance)
Temp80MeanForestDistance <- mean(Temp80$ForestDistance)
Temp90MeanForestDistance <- mean(Temp90$ForestDistance)
Highest10MeanForestDistance <- mean(Highest10$ForestDistance)

## Plot development mean distance to water

# Create tibble to prepare plotting
Classes <- c(6, 8, 10, 12, 14, 16, 18, 20, 23, 23)
Quantiles <- c("<= 10%", "<= 20%", "<= 30%", "<= 40%", "<= 50%",
               "<= 60%", "<= 70%", "<= 80%", "<= 90%", "> 90%")
MeanWaterDistance <- c(Lowest10MeanWaterDistance, Temp20MeanWaterDistance,
                       Temp30MeanWaterDistance, Temp40MeanWaterDistance,
                       Temp50MeanWaterDistance, Temp60MeanWaterDistance,
                       Temp70MeanWaterDistance, Temp80MeanWaterDistance,
                       Temp90MeanWaterDistance, Highest10MeanWaterDistance)
MeanForestDistance <- c(Lowest10MeanForestDistance, Temp20MeanForestDistance,
                        Temp30MeanForestDistance, Temp40MeanForestDistance,
                        Temp50MeanForestDistance, Temp60MeanForestDistance,
                        Temp70MeanForestDistance, Temp80MeanForestDistance,
                        Temp90MeanForestDistance, Highest10MeanForestDistance)
TempDist <- tibble(Classes, Quantiles, MeanWaterDistance, MeanForestDistance)

# Visualize influence temperature on mean distance to water
TempDistWaterVis <- ggplot(data = TempDist, aes(x = Classes, y = MeanWaterDistance)) +
  geom_point(stat = "identity", shape = 16, size = 2,
             aes(color = MeanWaterDistance)) +
  geom_text(label = sprintf("%0.2f", round(MeanWaterDistance, digits = 2)), vjust = 2, size = 3) +
  geom_smooth(method="lm", se=F, fullrange=T,
              color = "black", size = 0.5)+
  theme_bw() +
  ylab("Mean distance to water") +
  xlab("Temperature (°C)") +
  ggtitle("Relation temperature and distance to water") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        legend.position = "none")
TempDistWaterVis

# Visualize influence temperature on mean distance to forest
TempDistForestVis <- ggplot(data = TempDist, aes(x = Classes, y = MeanForestDistance)) +
  geom_point(stat = "identity", shape = 16, size = 2,
             aes(color = MeanForestDistance)) +
  geom_text(label = sprintf("%0.2f", round(MeanForestDistance, digits = 2)), vjust = 2, size = 3) +
  geom_smooth(method="lm", se=F, fullrange=T,
              color = "black", size = 0.5)+
  theme_bw() +
  ylab("Mean distance to forest") +
  xlab("Temperature (°C)") +
  ggtitle("Relation temperature and distance to forest") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        legend.position = "none")
TempDistForestVis


## Comparing JI per season

# Select spring
Spring <- AllTrackPoints %>% 
  filter(season == "spring")

# Select summer
Summer <- AllTrackPoints %>% 
  filter(season == "summer")

# Select autumn
Autumn <- AllTrackPoints %>% 
  filter(season == "autumm")

# Select winter
Winter <- AllTrackPoints %>% 
  filter(season == "winter")

## JI spring

# Determine landuse class used 
SpringClassUsed <- table(Spring$landuse_code)
NumberOfPoints <- length(Spring$landuse_code)

# Create table with proportion used per of landuse class
PropUsedSpring <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(PropUsedSpring) <- as.character(LUTLanduseClasses$landuse_class)

# Fill empty table PropUsedSpring with proportion used per landuse class
for(i in seq_along(PropUsedSpring)){
  if(is.na(SpringClassUsed[as.character(i)])){
    SpringClassUsed[as.character(i)] <- 0
  }
  PropUsedSpring[1,i] <- SpringClassUsed[as.character(i)] / NumberOfPoints
}

# Create empty table to store result
JISpring <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(JISpring) <- LUTLanduseClasses$landuse_class

# Iterate over elements of JISpring
for(i in seq_along(JISpring)){
  JISpring[i] <- JacobsIndex(as.numeric(PropUsedSpring[i]), as.numeric(PropAvail[i]))
}

## JI summer

# Determine landuse class used 
SummerClassUsed <- table(Summer$landuse_code)
NumberOfPoints <- length(Summer$landuse_code)

# Create table with proportion used per of landuse class
PropUsedSummer <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(PropUsedSummer) <- as.character(LUTLanduseClasses$landuse_class)

# Fill empty table PropUsedSummer with proportion used per landuse class
for(i in seq_along(PropUsedSummer)){
  if(is.na(SummerClassUsed[as.character(i)])){
    SummerClassUsed[as.character(i)] <- 0
  }
  PropUsedSummer[1,i] <- SummerClassUsed[as.character(i)] / NumberOfPoints
}

# Create empty table to store result
JISummer <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(JISummer) <- LUTLanduseClasses$landuse_class

# Iterate over elements of JISummer
for(i in seq_along(JISummer)){
  JISummer[i] <- JacobsIndex(as.numeric(PropUsedSummer[i]), as.numeric(PropAvail[i]))
}

## JI autumn

# Determine landuse class used 
AutumnClassUsed <- table(Autumn$landuse_code)
NumberOfPoints <- length(Autumn$landuse_code)

# Create table with proportion used per of landuse class
PropUsedAutumn <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(PropUsedAutumn) <- as.character(LUTLanduseClasses$landuse_class)

# Fill empty table PropUsedAutumn with proportion used per landuse class
for(i in seq_along(PropUsedAutumn)){
  if(is.na(AutumnClassUsed[as.character(i)])){
    AutumnClassUsed[as.character(i)] <- 0
  }
  PropUsedAutumn[1,i] <- AutumnClassUsed[as.character(i)] / NumberOfPoints
}

# Create empty table to store result
JIAutumn <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(JIAutumn) <- LUTLanduseClasses$landuse_class

# Iterate over elements of JIAutumn
for(i in seq_along(JIAutumn)){
  JIAutumn[i] <- JacobsIndex(as.numeric(PropUsedAutumn[i]), as.numeric(PropAvail[i]))
}

## JI winter

# Determine landuse class used 
WinterClassUsed <- table(Winter$landuse_code)
NumberOfPoints <- length(Winter$landuse_code)

# Create table with proportion used per of landuse class
PropUsedWinter <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(PropUsedWinter) <- as.character(LUTLanduseClasses$landuse_class)

# Fill empty table PropUsedWinter with proportion used per landuse class
for(i in seq_along(PropUsedWinter)){
  if(is.na(WinterClassUsed[as.character(i)])){
    WinterClassUsed[as.character(i)] <- 0
  }
  PropUsedWinter[1,i] <- WinterClassUsed[as.character(i)] / NumberOfPoints
}

# Create empty table to store result
JIWinter <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(JIWinter) <- LUTLanduseClasses$landuse_class

# Iterate over elements of JIWinter
for(i in seq_along(JIWinter)){
  JIWinter[i] <- JacobsIndex(as.numeric(PropUsedWinter[i]), as.numeric(PropAvail[i]))
}

## Visualize difference in JI per season

# Spring

# Make the data suitable for the visualization
Class <- colnames(JISpring)
JI_value <- as.numeric(JISpring)
JISpringtibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JISpringVis <- ggplot(data = JISpringtibble, aes(x = Class, y = JI_value, 
                                                       fill = Class, 
                                                       label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class in Spring") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JISpringVis

# Summer

# Make the data suitable for the visualization
Class <- colnames(JISummer)
JI_value <- as.numeric(JISummer)
JISummertibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JISummerVis <- ggplot(data = JISummertibble, aes(x = Class, y = JI_value, 
                                                 fill = Class, 
                                                 label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class in Summer") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JISummerVis

# Autumn

# Make the data suitable for the visualization
Class <- colnames(JIAutumn)
JI_value <- as.numeric(JIAutumn)
JIAutumntibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JIAutumnVis <- ggplot(data = JIAutumntibble, aes(x = Class, y = JI_value, 
                                                 fill = Class, 
                                                 label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class in Autumn") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JIAutumnVis

# Winter

# Make the data suitable for the visualization
Class <- colnames(JIWinter)
JI_value <- as.numeric(JIWinter)
JIWintertibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JIWinterVis <- ggplot(data = JIWintertibble, aes(x = Class, y = JI_value, 
                                                 fill = Class, 
                                                 label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class in Winter") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JIWinterVis

## Compare mean distances per season

# Distance to water
SpringMeanWaterDistance <- mean(Spring$WaterDistance)
SummerMeanWaterDistance <- mean(Summer$WaterDistance)
AutumnMeanWaterDistance <- mean(Autumn$WaterDistance)
WinterMeanWaterDistance <- mean(Winter$WaterDistance)

# Distance to forest
SpringMeanForestDistance <- mean(Spring$ForestDistance)
SummerMeanForestDistance <- mean(Summer$ForestDistance)
AutumnMeanForestDistance <- mean(Autumn$ForestDistance)
WinterMeanForestDistance <- mean(Winter$ForestDistance)

# Create tibble to prepare plotting
Classes <- c("Spring", "Summer", "Autumn", "Winter")
MeanWaterDistance <- c(SpringMeanWaterDistance, SummerMeanWaterDistance,
                       AutumnMeanWaterDistance, WinterMeanWaterDistance)
MeanForestDistance <- c(SpringMeanForestDistance, SummerMeanForestDistance,
                        AutumnMeanForestDistance, WinterMeanForestDistance)
SeasonDist <- tibble(Classes, MeanWaterDistance, MeanForestDistance)
SeasonDist$Classes <- factor(SeasonDist$Classes, levels = unique(SeasonDist$Classes))

# Plot the data via ggplot2
SeasonWaterDistVis <- ggplot(data = SeasonDist, aes(x = Classes, y = MeanWaterDistance, 
                                                           fill = Classes, 
                                                           label=sprintf("%0.2f", round(MeanWaterDistance, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  coord_cartesian(ylim = c(180, 250))+
  ylab("Average distance to water (m)") +
  xlab("Season") +
  ggtitle("Average distance to water per season") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
SeasonWaterDistVis

SeasonForestDistVis <- ggplot(data = SeasonDist, aes(x = Classes, y = MeanForestDistance, 
                                                      fill = Classes, 
                                                      label=sprintf("%0.2f", round(MeanForestDistance, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  coord_cartesian(ylim = c(50, 120))+
  ylab("Average distance to forest (m)") +
  xlab("Season") +
  ggtitle("Average distance to forest per season") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
SeasonForestDistVis

## Comparing JI weekend vs businessday

# Select weekend
Weekend <- AllTrackPoints %>% 
  filter(day_type == "weekend")

# Select business day
Businessday <- AllTrackPoints %>% 
  filter(day_type == "business day")


## JI weekend

# Determine landuse class used 
WeekendClassUsed <- table(Weekend$landuse_code)
NumberOfPoints <- length(Weekend$landuse_code)

# Create table with proportion used per of landuse class
PropUsedWeekend <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(PropUsedWeekend) <- as.character(LUTLanduseClasses$landuse_class)

# Fill empty table PropUsedWeekend with proportion used per landuse class
for(i in seq_along(PropUsedWeekend)){
  if(is.na(WeekendClassUsed[as.character(i)])){
    WeekendClassUsed[as.character(i)] <- 0
  }
  PropUsedWeekend[1,i] <- WeekendClassUsed[as.character(i)] / NumberOfPoints
}

# Create empty table to store result
JIWeekend <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(JIWeekend) <- LUTLanduseClasses$landuse_class

# Iterate over elements of JIWeekend
for(i in seq_along(JIWeekend)){
  JIWeekend[i] <- JacobsIndex(as.numeric(PropUsedWeekend[i]), as.numeric(PropAvail[i]))
}


## JI business day

# Determine landuse class used 
BusinessdayClassUsed <- table(Businessday$landuse_code)
NumberOfPoints <- length(Businessday$landuse_code)

# Create table with proportion used per of landuse class
PropUsedBusinessday <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(PropUsedBusinessday) <- as.character(LUTLanduseClasses$landuse_class)

# Fill empty table PropUsedBusinessday with proportion used per landuse class
for(i in seq_along(PropUsedBusinessday)){
  if(is.na(BusinessdayClassUsed[as.character(i)])){
    BusinessdayClassUsed[as.character(i)] <- 0
  }
  PropUsedBusinessday[1,i] <- BusinessdayClassUsed[as.character(i)] / NumberOfPoints
}

# Create empty table to store result
JIBusinessday <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(JIBusinessday) <- LUTLanduseClasses$landuse_class

# Iterate over elements of JIBusinessday
for(i in seq_along(JIBusinessday)){
  JIBusinessday[i] <- JacobsIndex(as.numeric(PropUsedBusinessday[i]), as.numeric(PropAvail[i]))
}

## Visualize difference in JI per day type

# Weekend

# Make the data suitable for the visualization
Class <- colnames(JIWeekend)
JI_value <- as.numeric(JIWeekend)
JIWeekendtibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JIWeekendVis <- ggplot(data = JIWeekendtibble, aes(x = Class, y = JI_value, 
                                                   fill = Class, 
                                                   label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class on weekend days") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JIWeekendVis

# Business day

# Make the data suitable for the visualization
Class <- colnames(JIBusinessday)
JI_value <- as.numeric(JIBusinessday)
JIBusinessdaytibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JIBusinessdayVis <- ggplot(data = JIBusinessdaytibble, aes(x = Class, y = JI_value, 
                                                           fill = Class, 
                                                           label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class on business days") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JIBusinessdayVis


## Compare mean distance to road business days and weekend days

# Only 1 meter difference, both ~110 meter
MeanRoadDistanceWeekend <- mean(Weekend$RoadDistance)
MeanRoadDistanceBusinessday <- mean(Businessday$RoadDistance)


## Comparing JI day vs night

# Select day
Day <- AllTrackPoints %>% 
  filter(day_night == "day")

# Select night
Night <- AllTrackPoints %>% 
  filter(day_night == "night")


## JI day

# Determine landuse class used 
DayClassUsed <- table(Day$landuse_code)
NumberOfPoints <- length(Day$landuse_code)

# Create table with proportion used per of landuse class
PropUsedDay <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(PropUsedDay) <- as.character(LUTLanduseClasses$landuse_class)

# Fill empty table PropUsedDay with proportion used per landuse class
for(i in seq_along(PropUsedDay)){
  if(is.na(DayClassUsed[as.character(i)])){
    DayClassUsed[as.character(i)] <- 0
  }
  PropUsedDay[1,i] <- DayClassUsed[as.character(i)] / NumberOfPoints
}

# Create empty table to store result
JIDay <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(JIDay) <- LUTLanduseClasses$landuse_class

# Iterate over elements of JIDay
for(i in seq_along(JIDay)){
  JIDay[i] <- JacobsIndex(as.numeric(PropUsedDay[i]), as.numeric(PropAvail[i]))
}


## JI night

# Determine landuse class used 
NightClassUsed <- table(Night$landuse_code)
NumberOfPoints <- length(Night$landuse_code)

# Create table with proportion used per of landuse class
PropUsedNight <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(PropUsedNight) <- as.character(LUTLanduseClasses$landuse_class)

# Fill empty table PropUsedNight with proportion used per landuse class
for(i in seq_along(PropUsedNight)){
  if(is.na(NightClassUsed[as.character(i)])){
    NightClassUsed[as.character(i)] <- 0
  }
  PropUsedNight[1,i] <- NightClassUsed[as.character(i)] / NumberOfPoints
}

# Create empty table to store result
JINight <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(JINight) <- LUTLanduseClasses$landuse_class

# Iterate over elements of JINight
for(i in seq_along(JINight)){
  JINight[i] <- JacobsIndex(as.numeric(PropUsedNight[i]), as.numeric(PropAvail[i]))
}

## Visualize difference day / night

# Day

# Make the data suitable for the visualization
Class <- colnames(JIDay)
JI_value <- as.numeric(JIDay)
JIDaytibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JIDayVis <- ggplot(data = JIDaytibble, aes(x = Class, y = JI_value, 
                                           fill = Class, 
                                           label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class during daylight") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JIDayVis

# Night

# Make the data suitable for the visualization
Class <- colnames(JINight)
JI_value <- as.numeric(JINight)
JINighttibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JINightVis <- ggplot(data = JINighttibble, aes(x = Class, y = JI_value, 
                                               fill = Class, 
                                               label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class during night") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JINightVis


## Compare distances day / night

# Distances during day
MeanRoadDistDay <- mean(Day$RoadDistance)
MeanWaterDistDay <- mean(Day$WaterDistance)
MeanForestDistDay <- mean(Day$ForestDistance)

# Distances during night
MeanRoadDistNight <- mean(Night$RoadDistance)
MeanWaterDistNight <- mean(Night$WaterDistance)
MeanForestDistNight <- mean(Night$ForestDistance)
# No spectacular differences. Distance to forest higher during night, which seems contrary


## Determine influence of sunshine

# get the lowest and highest 10% sunshine hours of the Day dataset
lowest10 <- as.numeric(quantile(Day$sunshine_duration_day, probs = seq(0, 1, 0.1))["10%"])
highest10 <- as.numeric(quantile(Day$sunshine_duration_day, probs = seq(0, 1, 0.1))["90%"])

# Filter Day to get only the points with the lowest 10% sunshine hours
Lowest10 <- Day %>% 
  filter(sunshine_duration_day <= lowest10)

# Filter Day to get only the points with the highest 10% sunshine hours
Highest10 <- Day %>% 
  filter(sunshine_duration_day > highest10)


## Calculate Jacob's index for lowest 10% sunshine hours

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

## Calculate Jacob's index for lowest 10% sunshine hours

# Determine landuse class used 
Highest10ClassUsed <- table(Highest10$landuse_code)
NumberOfPoints <- length(Highest10$landuse_code)

# Create table with proportion used per of landuse class
PropUsedHighest10 <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(PropUsedHighest10) <- as.character(LUTLanduseClasses$landuse_class)

# Fill empty table PropUsedHighest10 with proportion used per landuse class
for(i in seq_along(PropUsedHighest10)){
  if(is.na(Highest10ClassUsed[as.character(i)])){
    Highest10ClassUsed[as.character(i)] <- 0
  }
  PropUsedHighest10[1,i] <- Highest10ClassUsed[as.character(i)] / NumberOfPoints
}

# Create empty table to store result
JIHighest10 <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(JIHighest10) <- LUTLanduseClasses$landuse_class

# Iterate over elements of JIHighest10
for(i in seq_along(JIHighest10)){
  JIHighest10[i] <- JacobsIndex(as.numeric(PropUsedHighest10[i]), as.numeric(PropAvail[i]))
}


## Create bar plots to compare lowest and highest 10% sunshine hours

# Make the data suitable for the visualization
Class <- colnames(JILowest10)
JI_value <- as.numeric(JILowest10)
JILowest10tibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JILowest10SunVis <- ggplot(data = JILowest10tibble, aes(x = Class, y = JI_value, 
                                                     fill = Class, 
                                                     label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class lowest 10% sunshine hours") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JILowest10SunVis

# Make the data suitable for the visualization
Class <- colnames(JIHighest10)
JI_value <- as.numeric(JIHighest10)
JIHighest10tibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JIHighest10SunVis <- ggplot(data = JIHighest10tibble, aes(x = Class, y = JI_value, 
                                                       fill = Class, 
                                                       label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class highest 10% sunshine hours") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JIHighest10SunVis


## Determine influence of rain

# get the lowest and highest 10% precipitation hours of the AllTracksPoints dataset
lowest10 <- as.numeric(quantile(AllTrackPoints$precipitation_duration_day, probs = seq(0, 1, 0.1))["10%"])
highest10 <- as.numeric(quantile(AllTrackPoints$precipitation_duration_day, probs = seq(0, 1, 0.1))["90%"])

# Filter AllTrackPoints to get only the points with the lowest 10% precipitation hours
Lowest10 <- AllTrackPoints %>% 
  filter(precipitation_duration_day <= lowest10)

# Filter AllTrackPoints to get only the points with the highest 10% precipitation hours
Highest10 <- AllTrackPoints %>% 
  filter(precipitation_duration_day > highest10)


## Calculate Jacob's index for lowest 10% precipitation hours

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

## Calculate Jacob's index for highest 10% precipitation hours

# Determine landuse class used 
Highest10ClassUsed <- table(Highest10$landuse_code)
NumberOfPoints <- length(Highest10$landuse_code)

# Create table with proportion used per of landuse class
PropUsedHighest10 <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(PropUsedHighest10) <- as.character(LUTLanduseClasses$landuse_class)

# Fill empty table PropUsedHighest10 with proportion used per landuse class
for(i in seq_along(PropUsedHighest10)){
  if(is.na(Highest10ClassUsed[as.character(i)])){
    Highest10ClassUsed[as.character(i)] <- 0
  }
  PropUsedHighest10[1,i] <- Highest10ClassUsed[as.character(i)] / NumberOfPoints
}

# Create empty table to store result
JIHighest10 <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(JIHighest10) <- LUTLanduseClasses$landuse_class

# Iterate over elements of JIHighest10
for(i in seq_along(JIHighest10)){
  JIHighest10[i] <- JacobsIndex(as.numeric(PropUsedHighest10[i]), as.numeric(PropAvail[i]))
}


## Create bar plots to compare lowest and highest 10% precipitation hours

# Make the data suitable for the visualization
Class <- colnames(JILowest10)
JI_value <- as.numeric(JILowest10)
JILowest10tibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JILowest10PrecVis <- ggplot(data = JILowest10tibble, aes(x = Class, y = JI_value, 
                                                        fill = Class, 
                                                        label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class lowest 10% precipitation hours") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JILowest10PrecVis

# Make the data suitable for the visualization
Class <- colnames(JIHighest10)
JI_value <- as.numeric(JIHighest10)
JIHighest10tibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JIHighest10PrecVis <- ggplot(data = JIHighest10tibble, aes(x = Class, y = JI_value, 
                                                          fill = Class, 
                                                          label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class highest 10% precipitation hours") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JIHighest10PrecVis


## Determine influence of wind

# get the lowest and highest 10% precipitation hours of the AllTracksPoints dataset
lowest10 <- as.numeric(quantile(AllTrackPoints$average_windspeed_day, probs = seq(0, 1, 0.1))["10%"])
highest10 <- as.numeric(quantile(AllTrackPoints$average_windspeed_day, probs = seq(0, 1, 0.1))["90%"])

# Filter AllTrackPoints to get only the points with the lowest 10% precipitation hours
Lowest10 <- AllTrackPoints %>% 
  filter(average_windspeed_day <= lowest10)

# Filter AllTrackPoints to get only the points with the highest 10% precipitation hours
Highest10 <- AllTrackPoints %>% 
  filter(average_windspeed_day > highest10)


## Calculate Jacob's index for lowest 10% average daily windspeed 

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

## Calculate Jacob's index for highest 10% average daily windspeed

# Determine landuse class used 
Highest10ClassUsed <- table(Highest10$landuse_code)
NumberOfPoints <- length(Highest10$landuse_code)

# Create table with proportion used per of landuse class
PropUsedHighest10 <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(PropUsedHighest10) <- as.character(LUTLanduseClasses$landuse_class)

# Fill empty table PropUsedHighest10 with proportion used per landuse class
for(i in seq_along(PropUsedHighest10)){
  if(is.na(Highest10ClassUsed[as.character(i)])){
    Highest10ClassUsed[as.character(i)] <- 0
  }
  PropUsedHighest10[1,i] <- Highest10ClassUsed[as.character(i)] / NumberOfPoints
}

# Create empty table to store result
JIHighest10 <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(JIHighest10) <- LUTLanduseClasses$landuse_class

# Iterate over elements of JIHighest10
for(i in seq_along(JIHighest10)){
  JIHighest10[i] <- JacobsIndex(as.numeric(PropUsedHighest10[i]), as.numeric(PropAvail[i]))
}


## Create bar plots to compare lowest and highest 10% average daily windspeed

# Make the data suitable for the visualization
Class <- colnames(JILowest10)
JI_value <- as.numeric(JILowest10)
JILowest10tibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JILowest10WindVis <- ggplot(data = JILowest10tibble, aes(x = Class, y = JI_value, 
                                                         fill = Class, 
                                                         label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class lowest 10% windspeed") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JILowest10WindVis

# Make the data suitable for the visualization
Class <- colnames(JIHighest10)
JI_value <- as.numeric(JIHighest10)
JIHighest10tibble <- tibble(Class, JI_value) %>% 
  drop_na()

# Plot the data via ggplot2
JIHighest10WindVis <- ggplot(data = JIHighest10tibble, aes(x = Class, y = JI_value, 
                                                           fill = Class, 
                                                           label=sprintf("%0.2f", round(JI_value, digits = 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Landuse class") +
  ggtitle("Jacobs index per landuse class highest 10% windspeed") +
  theme(plot.title = element_text(size = 12, face = "bold",
                                  margin = margin(10,0,10,0),
                                  hjust = 0.5),
        axis.title.x = element_text(vjust = -0.35, face = "plain"),
        axis.title.y = element_text(vjust = 0.35, face = "plain"),
        axis.text.x = element_text(angle = 50, size = 10, vjust = 0.5),
        legend.position = "none") +
  geom_text(vjust = -0.2, size = 2)
JIHighest10WindVis




