# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to determine the influence of environmental variables on the Jacob's 
### index  of a landuse class.


## CCI

# get the lowest and highest 10% of the data
lowest10 <- as.numeric(quantile(AllTrackPoints$CCI, probs = seq(0, 1, 0.1))["10%"])
highest10 <- as.numeric(quantile(AllTrackPoints$CCI, probs = seq(0, 1, 0.1))["90%"])

# Filter AllTrackPoints to get only the points with the lowest 10% temperatures
Lowest10 <- AllTrackPoints %>% 
  filter(CCI <= lowest10)

# Filter TempDataset to get only the points with the higest 10% temperatures
Highest10 <- TempDataset %>% 
  filter(CCI > highest10)


## Calculate Jacob's index for lowest 10% CCI

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

## Calculate Jacob's index for highest 10% CCI

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
lowest10 <- as.numeric(quantile(AllTrackPoints$CCI, probs = seq(0, 1, 0.1))["10%"])
temp20 <- as.numeric(quantile(AllTrackPoints$CCI, probs = seq(0, 1, 0.1))["20%"])
temp30 <- as.numeric(quantile(AllTrackPoints$CCI, probs = seq(0, 1, 0.1))["30%"])
temp40 <- as.numeric(quantile(AllTrackPoints$CCI, probs = seq(0, 1, 0.1))["40%"])
temp50 <- as.numeric(quantile(AllTrackPoints$CCI, probs = seq(0, 1, 0.1))["50%"])
temp60 <- as.numeric(quantile(AllTrackPoints$CCI, probs = seq(0, 1, 0.1))["60%"])
temp70 <- as.numeric(quantile(AllTrackPoints$CCI, probs = seq(0, 1, 0.1))["70%"])
temp80 <- as.numeric(quantile(AllTrackPoints$CCI, probs = seq(0, 1, 0.1))["80%"])
highest10 <- as.numeric(quantile(AllTrackPoints$CCI, probs = seq(0, 1, 0.1))["90%"])

# Filter TempDataset to get datasets with other all quantiles than the 
# lowest 10 and highest 10
Temp20 <- AllTrackPoints %>% 
  filter(CCI > lowest10 &
           CCI <= temp20)

Temp30 <- AllTrackPoints %>% 
  filter(CCI > temp20 &
           CCI <= temp30)

Temp40 <- AllTrackPoints %>% 
  filter(CCI > temp30 &
           CCI <= temp40)

Temp50 <- AllTrackPoints %>% 
  filter(CCI > temp40 &
           CCI <= temp50)

Temp60 <- AllTrackPoints %>% 
  filter(CCI > temp50 &
           CCI <= temp60)

Temp70 <- AllTrackPoints %>% 
  filter(CCI > temp60 &
           CCI <= temp70)

Temp80 <- AllTrackPoints %>% 
  filter(CCI > temp70 &
           CCI <= temp80)

Temp90 <- AllTrackPoints %>% 
  filter(CCI > temp80 &
           CCI <= highest10)

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


## Comparing JI per season and time of the day

# Create function to efficiently calculate JI per time frame
JIperTimeframe <- function(TimeframeList){
  
  # Create empty result table
  Result <- data.frame(matrix(data = NA, nrow = 12, ncol = 10))
  
  # Name rows and columns
  rownames(Result) <- c("01:00:00", "03:00:00", "05:00:00", "07:00:00",
                        "09:00:00", "11:00:00", "13:00:00", "15:00:00",
                        "17:00:00", "19:00:00", "21:00:00", "23:00:00")
  colnames(Result) <- c("grassland", "deciduous forest", "coniferous forest",
                        "fresh water", "bare soil", "heathland", "road",
                        "grassy heathland", "swamp", "shrubland")
  
  # Calculate JI per landuse class per time frame
  for(i in colnames(Result)){
    
    # Get availability of landuse class from the PropAvail table
    Available <- as.numeric(PropAvail[i])
    
    # Calculate JI per row
    for(j in 1:nrow(Result)){
      PointsList <- TimeframeList[[j]]
      FreqLanduseClass <- nrow(PointsList[which(PointsList$landuse_class == i),])
      TotalPoints <- nrow(PointsList)
      Used <- FreqLanduseClass / TotalPoints
      Result[j,i] <- JacobsIndex(Used, Available)
    }
  }
  
  # Return result table
  return(Result)
}

# Create general funcion to calculate JI per season
getJIVisualization <- function(StudyArea, season, landuse_class){
  
  # Check whether the input for StudyArea is valid
  StudyArea <- match.arg(StudyArea, choices = c("Kraansvlak", "Maashorst2016", "Maashorst20172021",
                                                "Maashorst2022", "SlikkenvdHeenHabituate", "SlikkenvdHeen", 
                                                "VeluweHabituate", "Veluwe", "all"))
  
  # Check  whether the input for season is valid
  season <- match.arg(season, choices = c("spring", "summer", "autumn", "winter", "all"))
  
  # Check  whether the input for landuse_class is valid
  landuse_class <- match.arg(landuse_class, choices = c("grassland", "deciduous forest", "coniferous forest", "fresh water",
                                          "bare soil", "shrubland", "swamp", "grassy heathland", "heathland",
                                          "road", "all"))
  
  # Get stack with raster data from StudyArea argument
  StudyAreaData <- switch(StudyArea,
                          Kraansvlak = KraansvlakPoints,
                          Maashorst2016 = Maashorst2016Points,
                          Maashorst20172021 = Maashorst20172021Points,
                          Maashorst2022 = Maashorst2022Points,
                          SlikkenvdHeenHabituate = SlikkenvdHeenHabPoints,
                          SlikkenvdHeen = SlikkenvdHeenPoints,
                          VeluweHabituate = VeluweHabPoints,
                          Veluwe = VeluwePoints,
                          all = AllTrackPoints)
  
  # Create subset of StudyAreaData based on input season
  if(season == "spring"){
    
    # Select spring
    StudyAreaData <- StudyAreaData %>% 
      filter(season == "spring")
    
  }else if(season == "summer"){
    
    # Select summer
    StudyAreaData <- StudyAreaData %>% 
      filter(season == "summer")
    
  }else if(season == "autumn"){
    
    # Select autumn
    StudyAreaData <- StudyAreaData %>% 
      filter(season == "autumn")
    
  }else if(season == "winter"){
    
    # Select winter
    StudyAreaData <- StudyAreaData %>% 
      filter(season == "winter")
    
  }else{
    StudyAreaDataSpring <- StudyAreaData %>% 
      filter(season == "spring")
    
    StudyAreaDataSummer <- StudyAreaData %>% 
      filter(season == "summer")
    
    StudyAreaDataAutumn <- StudyAreaData %>% 
      filter(season == "autumn")
    
    StudyAreaDataWinter <- StudyAreaData %>% 
      filter(season == "winter")
  }
  
  # Split StudyAreaData in 12 time classes
  if(landuse_class == "all"){
    
    # Select 00:00:00 - 02:00:00
    twelvetwo <- StudyAreaData %>% 
      filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))
    
    # Select 02:00:00 - 04:00:00
    twofour <- StudyAreaData %>% 
      filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))
    
    # Select 04:00:00 - 06:00:00
    foursix <- StudyAreaData %>% 
      filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))
    
    # Select 06:00:00 - 08:00:00
    sixeight <- StudyAreaData %>% 
      filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))
    
    # Select 08:00:00 - 10:00:00
    eightten <- StudyAreaData %>% 
      filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))
    
    # Select 10:00:00 - 12:00:00
    tentwelve <- StudyAreaData %>% 
      filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))
    
    # Select 12:00:00 - 14:00:00
    twelvetwoaft <- StudyAreaData %>% 
      filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))
    
    # Select 14:00:00 - 16:00:00
    twofouraft <- StudyAreaData %>% 
      filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))
    
    # Select 16:00:00 - 18:00:00
    foursixaft <- StudyAreaData %>% 
      filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))
    
    # Select 18:00:00 - 20:00:00
    sixeightaft <- StudyAreaData %>% 
      filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))
    
    # Select 20:00:00 - 22:00:00
    eighttenaft <- StudyAreaData %>% 
      filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))
    
    # Select 22:00:00 - 00:00:00
    tentwelveaft <- StudyAreaData %>% 
      filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))
    
    
    # Create lists of timeframes
    TimeframeList <- list(twelvetwo, twofour, foursix, sixeight,
                          eightten, tentwelve, twelvetwoaft, twofouraft,
                          foursixaft, sixeightaft, eighttenaft, tentwelveaft)
    
    # Call JIperTimeframe for created list
    TimeframeJITable <- JIperTimeframe(TimeframeList)
    
    # Melt the tables for visualization purposes
    TimeframeJITable$timeframe <- rownames(TimeframeJITable)
    TimeframeJIMelt <- melt(TimeframeJITable, id = "timeframe") %>% 
      mutate(season = season)
    colnames(TimeframeJIMelt) <- c("timeframe", "class", "JI", "season")
  }
  else if(season != "all" & landuse_class == "all"){
    
    # Select 00:00:00 - 02:00:00
    twelvetwo <- StudyAreaData %>% 
      filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))
    
    # Select 02:00:00 - 04:00:00
    twofour <- StudyAreaData %>% 
      filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))
    
    # Select 04:00:00 - 06:00:00
    foursix <- StudyAreaData %>% 
      filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))
    
    # Select 06:00:00 - 08:00:00
    sixeight <- StudyAreaData %>% 
      filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))
    
    # Select 08:00:00 - 10:00:00
    eightten <- StudyAreaData %>% 
      filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))
    
    # Select 10:00:00 - 12:00:00
    tentwelve <- StudyAreaData %>% 
      filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))
    
    # Select 12:00:00 - 14:00:00
    twelvetwoaft <- StudyAreaData %>% 
      filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))
    
    # Select 14:00:00 - 16:00:00
    twofouraft <- StudyAreaData %>% 
      filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))
    
    # Select 16:00:00 - 18:00:00
    foursixaft <- StudyAreaData %>% 
      filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))
    
    # Select 18:00:00 - 20:00:00
    sixeightaft <- StudyAreaData %>% 
      filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))
    
    # Select 20:00:00 - 22:00:00
    eighttenaft <- StudyAreaData %>% 
      filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))
    
    # Select 22:00:00 - 00:00:00
    tentwelveaft <- StudyAreaData %>% 
      filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))
    
    
    # Create lists of timeframes
    TimeframeList <- list(twelvetwo, twofour, foursix, sixeight,
                          eightten, tentwelve, twelvetwoaft, twofouraft,
                          foursixaft, sixeightaft, eighttenaft, tentwelveaft)
    
    # Call JIperTimeframe for created list
    TimeframeJITable <- JIperTimeframe(TimeframeList)
    
    # Melt the tables for visualization purposes
    TimeframeJITable$timeframe <- rownames(TimeframeJITable)
    TimeframeJIMelt <- melt(TimeframeJITable, id = "timeframe") %>% 
      mutate(season = season)
    colnames(TimeframeJIMelt) <- c("timeframe", "class", "JI", "season")
  }
  else if(season == "all" & landuse_class != "all"){
    
    ## Spring
    
    # Select 00:00:00 - 02:00:00
    twelvetwoSpring <- StudyAreaDataSpring %>% 
      filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))
    
    # Select 02:00:00 - 04:00:00
    twofourSpring <- StudyAreaDataSpring %>% 
      filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))
    
    # Select 04:00:00 - 06:00:00
    foursixSpring <- StudyAreaDataSpring %>% 
      filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))
    
    # Select 06:00:00 - 08:00:00
    sixeightSpring <- StudyAreaDataSpring %>% 
      filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))
    
    # Select 08:00:00 - 10:00:00
    eighttenSpring <- StudyAreaDataSpring %>% 
      filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))
    
    # Select 10:00:00 - 12:00:00
    tentwelveSpring <- StudyAreaDataSpring %>% 
      filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))
    
    # Select 12:00:00 - 14:00:00
    twelvetwoaftSpring <- StudyAreaDataSpring %>% 
      filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))
    
    # Select 14:00:00 - 16:00:00
    twofouraftSpring <- StudyAreaDataSpring %>% 
      filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))
    
    # Select 16:00:00 - 18:00:00
    foursixaftSpring <- StudyAreaDataSpring %>% 
      filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))
    
    # Select 18:00:00 - 20:00:00
    sixeightaftSpring <- StudyAreaDataSpring %>% 
      filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))
    
    # Select 20:00:00 - 22:00:00
    eighttenaftSpring <- StudyAreaDataSpring %>% 
      filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))
    
    # Select 22:00:00 - 00:00:00
    tentwelveaftSpring <- StudyAreaDataSpring %>% 
      filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))
    
    
    # Create lists of timeframes
    SpringTimeframeList <- list(twelvetwoSpring, twofourSpring, foursixSpring, sixeightSpring,
                                eighttenSpring, tentwelveSpring, twelvetwoaftSpring, twofouraftSpring,
                                foursixaftSpring, sixeightaftSpring, eighttenaftSpring, tentwelveaftSpring)
    
    # Call JIperTimeframe for created list
    SpringTimeframeJITable <- JIperTimeframe(SpringTimeframeList)
    
    # Melt the tables for visualization purposes
    SpringTimeframeJITable$timeframe <- rownames(SpringTimeframeJITable)
    SpringTimeframeJIMelt <- melt(SpringTimeframeJITable, id = "timeframe") %>% 
      mutate(season = "spring")
    colnames(SpringTimeframeJIMelt) <- c("timeframe", "class", "JI", "season")
    
    
    ## Summer
    
    # Select 00:00:00 - 02:00:00
    twelvetwoSummer <- StudyAreaDataSummer %>% 
      filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))
    
    # Select 02:00:00 - 04:00:00
    twofourSummer <- StudyAreaDataSummer %>% 
      filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))
    
    # Select 04:00:00 - 06:00:00
    foursixSummer <- StudyAreaDataSummer %>% 
      filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))
    
    # Select 06:00:00 - 08:00:00
    sixeightSummer <- StudyAreaDataSummer %>% 
      filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))
    
    # Select 08:00:00 - 10:00:00
    eighttenSummer <- StudyAreaDataSummer %>% 
      filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))
    
    # Select 10:00:00 - 12:00:00
    tentwelveSummer <- StudyAreaDataSummer %>% 
      filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))
    
    # Select 12:00:00 - 14:00:00
    twelvetwoaftSummer <- StudyAreaDataSummer %>% 
      filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))
    
    # Select 14:00:00 - 16:00:00
    twofouraftSummer <- StudyAreaDataSummer %>% 
      filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))
    
    # Select 16:00:00 - 18:00:00
    foursixaftSummer <- StudyAreaDataSummer %>% 
      filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))
    
    # Select 18:00:00 - 20:00:00
    sixeightaftSummer <- StudyAreaDataSummer %>% 
      filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))
    
    # Select 20:00:00 - 22:00:00
    eighttenaftSummer <- StudyAreaDataSummer %>% 
      filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))
    
    # Select 22:00:00 - 00:00:00
    tentwelveaftSummer <- StudyAreaDataSummer %>% 
      filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))
    
    
    # Create lists of timeframes
    SummerTimeframeList <- list(twelvetwoSummer, twofourSummer, foursixSummer, sixeightSummer,
                                eighttenSummer, tentwelveSummer, twelvetwoaftSummer, twofouraftSummer,
                                foursixaftSummer, sixeightaftSummer, eighttenaftSummer, tentwelveaftSummer)
    
    # Call JIperTimeframe for created list
    SummerTimeframeJITable <- JIperTimeframe(SummerTimeframeList)
    
    # Melt the tables for visualization purposes
    SummerTimeframeJITable$timeframe <- rownames(SummerTimeframeJITable)
    SummerTimeframeJIMelt <- melt(SummerTimeframeJITable, id = "timeframe") %>% 
      mutate(season = "summer")
    colnames(SummerTimeframeJIMelt) <- c("timeframe", "class", "JI", "season")
    
    
    ## Autumn
    
    # Select 00:00:00 - 02:00:00
    twelvetwoAutumn <- StudyAreaDataAutumn %>% 
      filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))
    
    # Select 02:00:00 - 04:00:00
    twofourAutumn <- StudyAreaDataAutumn %>% 
      filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))
    
    # Select 04:00:00 - 06:00:00
    foursixAutumn <- StudyAreaDataAutumn %>% 
      filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))
    
    # Select 06:00:00 - 08:00:00
    sixeightAutumn <- StudyAreaDataAutumn %>% 
      filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))
    
    # Select 08:00:00 - 10:00:00
    eighttenAutumn <- StudyAreaDataAutumn %>% 
      filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))
    
    # Select 10:00:00 - 12:00:00
    tentwelveAutumn <- StudyAreaDataAutumn %>% 
      filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))
    
    # Select 12:00:00 - 14:00:00
    twelvetwoaftAutumn <- StudyAreaDataAutumn %>% 
      filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))
    
    # Select 14:00:00 - 16:00:00
    twofouraftAutumn <- StudyAreaDataAutumn %>% 
      filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))
    
    # Select 16:00:00 - 18:00:00
    foursixaftAutumn <- StudyAreaDataAutumn %>% 
      filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))
    
    # Select 18:00:00 - 20:00:00
    sixeightaftAutumn <- StudyAreaDataAutumn %>% 
      filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))
    
    # Select 20:00:00 - 22:00:00
    eighttenaftAutumn <- StudyAreaDataAutumn %>% 
      filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))
    
    # Select 22:00:00 - 00:00:00
    tentwelveaftAutumn <- StudyAreaDataAutumn %>% 
      filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))
    
    
    # Create lists of timeframes
    AutumnTimeframeList <- list(twelvetwoAutumn, twofourAutumn, foursixAutumn, sixeightAutumn,
                                eighttenAutumn, tentwelveAutumn, twelvetwoaftAutumn, twofouraftAutumn,
                                foursixaftAutumn, sixeightaftAutumn, eighttenaftAutumn, tentwelveaftAutumn)
    
    # Call JIperTimeframe for created list
    AutumnTimeframeJITable <- JIperTimeframe(AutumnTimeframeList)
    
    # Melt the tables for visualization purposes
    AutumnTimeframeJITable$timeframe <- rownames(AutumnTimeframeJITable)
    AutumnTimeframeJIMelt <- melt(AutumnTimeframeJITable, id = "timeframe") %>% 
      mutate(season = "autumn")
    colnames(AutumnTimeframeJIMelt) <- c("timeframe", "class", "JI", "season")
    
    
    ## Winter
    
    # Select 00:00:00 - 02:00:00
    twelvetwoWinter <- StudyAreaDataWinter %>% 
      filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))
    
    # Select 02:00:00 - 04:00:00
    twofourWinter <- StudyAreaDataWinter %>% 
      filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))
    
    # Select 04:00:00 - 06:00:00
    foursixWinter <- StudyAreaDataWinter %>% 
      filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))
    
    # Select 06:00:00 - 08:00:00
    sixeightWinter <- StudyAreaDataWinter %>% 
      filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))
    
    # Select 08:00:00 - 10:00:00
    eighttenWinter <- StudyAreaDataWinter %>% 
      filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))
    
    # Select 10:00:00 - 12:00:00
    tentwelveWinter <- StudyAreaDataWinter %>% 
      filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))
    
    # Select 12:00:00 - 14:00:00
    twelvetwoaftWinter <- StudyAreaDataWinter %>% 
      filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))
    
    # Select 14:00:00 - 16:00:00
    twofouraftWinter <- StudyAreaDataWinter %>% 
      filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))
    
    # Select 16:00:00 - 18:00:00
    foursixaftWinter <- StudyAreaDataWinter %>% 
      filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))
    
    # Select 18:00:00 - 20:00:00
    sixeightaftWinter <- StudyAreaDataWinter %>% 
      filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))
    
    # Select 20:00:00 - 22:00:00
    eighttenaftWinter <- StudyAreaDataWinter %>% 
      filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))
    
    # Select 22:00:00 - 00:00:00
    tentwelveaftWinter <- StudyAreaDataWinter %>% 
      filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))
    
    
    # Create lists of timeframes
    WinterTimeframeList <- list(twelvetwoWinter, twofourWinter, foursixWinter, sixeightWinter,
                                eighttenWinter, tentwelveWinter, twelvetwoaftWinter, twofouraftWinter,
                                foursixaftWinter, sixeightaftWinter, eighttenaftWinter, tentwelveaftWinter)
    
    # Call JIperTimeframe for created list
    WinterTimeframeJITable <- JIperTimeframe(WinterTimeframeList)
    
    # Melt the tables for visualization purposes
    WinterTimeframeJITable$timeframe <- rownames(WinterTimeframeJITable)
    WinterTimeframeJIMelt <- melt(WinterTimeframeJITable, id = "timeframe") %>% 
      mutate(season = "winter")
    colnames(WinterTimeframeJIMelt) <- c("timeframe", "class", "JI", "season")
    
    # Create merged JI per time of day table
    TimeframeJI <- rbind(SpringTimeframeJIMelt, SummerTimeframeJIMelt,
                         AutumnTimeframeJIMelt, WinterTimeframeJIMelt)
  }
  else if(season != "all" & landuse_class != "all"){
    
    # Select high and low CCI
    LowCCI <- StudyAreaData %>% 
      filter(CCI <= as.numeric(quantile(StudyAreaData$CCI, 0.20)))
    
    HighCCI <- StudyAreaData %>% 
      filter(CCI >= as.numeric(quantile(StudyAreaData$CCI, 0.80)))
    
    
    ## Split spring low CCI datasets in time classes
    
    # Select 00:00:00 - 02:00:00
    LowCCI0002 <- LowCCI %>% 
      filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))
    
    # Select 02:00:00 - 04:00:00
    LowCCI0204 <- LowCCI %>% 
      filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))
    
    # Select 04:00:00 - 06:00:00
    LowCCI0406 <- LowCCI %>% 
      filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))
    
    # Select 06:00:00 - 08:00:00
    LowCCI0608 <- LowCCI %>% 
      filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))
    
    # Select 08:00:00 - 10:00:00
    LowCCI0810 <- LowCCI %>% 
      filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))
    
    # Select 10:00:00 - 12:00:00
    LowCCI1012 <- LowCCI %>% 
      filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))
    
    # Select 12:00:00 - 14:00:00
    LowCCI1214 <- LowCCI %>% 
      filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))
    
    # Select 14:00:00 - 16:00:00
    LowCCI1416 <- LowCCI %>% 
      filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))
    
    # Select 16:00:00 - 18:00:00
    LowCCI1618 <- LowCCI %>% 
      filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))
    
    # Select 18:00:00 - 20:00:00
    LowCCI1820 <- LowCCI %>% 
      filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))
    
    # Select 20:00:00 - 22:00:00
    LowCCI2022 <- LowCCI %>% 
      filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))
    
    # Select 22:00:00 - 00:00:00
    LowCCI2200 <- LowCCI %>% 
      filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))
    
    
    ## Split spring high CCI datasets in time classes
    
    # Select 00:00:00 - 02:00:00
    HighCCI0002 <- HighCCI %>% 
      filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))
    
    # Select 02:00:00 - 04:00:00
    HighCCI0204 <- HighCCI %>% 
      filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))
    
    # Select 04:00:00 - 06:00:00
    HighCCI0406 <- HighCCI %>% 
      filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))
    
    # Select 06:00:00 - 08:00:00
    HighCCI0608 <- HighCCI %>% 
      filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))
    
    # Select 08:00:00 - 10:00:00
    HighCCI0810 <- HighCCI %>% 
      filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))
    
    # Select 10:00:00 - 12:00:00
    HighCCI1012 <- HighCCI %>% 
      filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))
    
    # Select 12:00:00 - 14:00:00
    HighCCI1214 <- HighCCI %>% 
      filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))
    
    # Select 14:00:00 - 16:00:00
    HighCCI1416 <- HighCCI %>% 
      filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))
    
    # Select 16:00:00 - 18:00:00
    HighCCI1618 <- HighCCI %>% 
      filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))
    
    # Select 18:00:00 - 20:00:00
    HighCCI1820 <- HighCCI %>% 
      filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))
    
    # Select 20:00:00 - 22:00:00
    HighCCI2022 <- HighCCI %>% 
      filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))
    
    # Select 22:00:00 - 00:00:00
    HighCCI2200 <- HighCCI %>% 
      filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))
    
    ## Create lists of timeframes (low CCI)
    
    # Low CCI
    LowCCITimeframeList <- list(LowCCI0002, LowCCI0204, LowCCI0406, LowCCI0608,
                                LowCCI0810, LowCCI1012, LowCCI1214, LowCCI1416,
                                LowCCI1618, LowCCI1820, LowCCI2022, LowCCI2200)
    
    # High CCI
    HighCCITimeframeList <- list(HighCCI0002, HighCCI0204, HighCCI0406, HighCCI0608,
                                 HighCCI0810, HighCCI1012, HighCCI1214, HighCCI1416,
                                 HighCCI1618, HighCCI1820, HighCCI2022, HighCCI2200)
    
    # Call JIperTimeframe
    LowCCITimeframeJITable <- JIperTimeframe(LowCCITimeframeList)
    HighCCITimeframeJITable <- JIperTimeframe(HighCCITimeframeList)
    
    
    ## Melt the tables for visualization purposes
    
    # Low CCI
    LowCCITimeframeJITable$timeframe <- rownames(LowCCITimeframeJITable)
    LowCCITimeframeJIMelt <- melt(LowCCITimeframeJITable, id = "timeframe") %>% 
      mutate(season = "") %>% 
      mutate(CCI = "lowest CCI 20%")
    colnames(LowCCITimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "CCI class")
    
    # High CCI
    HighCCITimeframeJITable$timeframe <- rownames(HighCCITimeframeJITable)
    HighCCITimeframeJIMelt <- melt(HighCCITimeframeJITable, id = "timeframe") %>% 
      mutate(season = "") %>% 
      mutate(CCI = "Highest CCI 20%")
    colnames(HighCCITimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "CCI class")
    
    # Create merged JI per time of day table
    TimeframeJICCI <- rbind(LowCCITimeframeJIMelt, HighCCITimeframeJIMelt)
  }
  
  
  # Visualize development JI per landuse class over the day per season
  if(landuse_class == "all"){
    TimeframeJIVis <- ggplot(data = TimeframeJIMelt, aes(x = timeframe,
                                                         y = JI,
                                                         group = class,
                                                         colour = class)) +
      geom_line() +
      geom_point() +
      ylim(c(-1,1)) +
      ylab("Jacobs index") +
      xlab("Timeframe of day (hours)") +
      ggtitle(paste0("Diurnal variation in Jacobs index (", StudyArea, ", ", season, ")")) +
      theme_bw() +
      scale_fill_brewer(palette = "Set3") +
      theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
            legend.position = "right") +
      scale_color_discrete(name = "Landuse class")
    
    # Return ggplot result
    return(TimeframeJIVis)
  }
  else if(season == "all" & landuse_class != "all"){
    LanduseClassTimeframeJIVis <- ggplot(data = TimeframeJI[which(TimeframeJI$class == landuse_class),],
                                      aes(x = timeframe, y = JI, group = season, colour = season)) +
      geom_line() +
      geom_point() +
      ylim(c(-1,1)) +
      ylab("Jacobs index") +
      xlab("Timeframe of day (hours)") +
      ggtitle(paste0("Diurnal variation in Jacobs index (", StudyArea, ", ", landuse_class, ")")) +
      theme_bw() +
      scale_fill_brewer(palette = "Set3") +
      theme(plot.title = element_text(size = 10),
            axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
            legend.position = "right") +
      scale_color_discrete(name = "Landuse class")
    
    # Return ggplot result
    return(LanduseClassTimeframeJIVis)
  }
  else if(season != "all" & landuse_class != "all"){
    CCITimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == landuse_class),],
                                               aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
      geom_line() +
      geom_point() +
      ylim(c(-1,1)) +
      ylab("Jacobs index") +
      xlab("Timeframe of day (hours)") +
      ggtitle(paste0("Diurnal variation in Jacobs index in ", season, " (", StudyArea, ", ", landuse_class, ")")) +
      theme_bw() +
      theme(plot.title = element_text(size = 10),
            axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
            legend.position = "right") +
      scale_color_manual(values = c("#FF0033", "#3300FF" ))
    GrasslandCCISpringTimeframeJIVis
    
    # Return ggplot result
    return(CCITimeframeJIVis)
  }
}


### Create visualizations of development of JI under different circumstances

## All study areas

# All seasons, all landuse classes
AllStudyAreasAllSeasons <- getJIVisualization("all", "all", "all")

# Specific season, all landuse classes
AllStudyAreasSpring <- getJIVisualization("all", "spring", "all")
AllStudyAreasSummer <- getJIVisualization("all", "summer", "all")
AllStudyAreasAutumn <- getJIVisualization("all", "autumn", "all")
AllStudyAreasWinter <- getJIVisualization("all", "winter", "all")

# All seasons, specific landuse classes
AllStudyAreasAllSeasonsGrassland <- getJIVisualization("all", "all", "grassland")
AllStudyAreasAllSeasonsDeciduousForest <- getJIVisualization("all", "all", "deciduous forest")
AllStudyAreasAllSeasonsConiferousForest <- getJIVisualization("all", "all", "coniferous forest")
AllStudyAreasAllSeasonsFreshWater <- getJIVisualization("all", "all", "fresh water")
AllStudyAreasAllSeasonsBareSoil <- getJIVisualization("all", "all", "bare soil")
AllStudyAreasAllSeasonsGrassyHeathland <- getJIVisualization("all", "all", "grassy heathland")
AllStudyAreasAllSeasonsHeathland <- getJIVisualization("all", "all", "heathland")
AllStudyAreasAllSeasonsSwamp <- getJIVisualization("all", "all", "swamp")
AllStudyAreasAllSeasonsShrubland <- getJIVisualization("all", "all", "shrubland")
AllStudyAreasAllSeasonsRoad <- getJIVisualization("all", "all", "road")

# Specific season, specific landuse classes
AllStudyAreasSpringGrassland <- getJIVisualization("all", "spring", "grassland")
AllStudyAreasSpringDeciduousForest <- getJIVisualization("all", "spring", "deciduous forest")
AllStudyAreasSpringConiferousForest <- getJIVisualization("all", "spring", "coniferous forest")
AllStudyAreasSpringFreshWater <- getJIVisualization("all", "spring", "fresh water")
AllStudyAreasSpringBareSoil <- getJIVisualization("all", "spring", "bare soil")
AllStudyAreasSpringGrassyHeathland <- getJIVisualization("all", "spring", "grassy heathland")
AllStudyAreasSpringHeathland <- getJIVisualization("all", "spring", "heathland")
AllStudyAreasSpringSwamp <- getJIVisualization("all", "spring", "swamp")
AllStudyAreasSpringShrubland <- getJIVisualization("all", "spring", "shrubland")
AllStudyAreasSpringRoad <- getJIVisualization("all", "spring", "road")

AllStudyAreasSummerGrassland <- getJIVisualization("all", "summer", "grassland")
AllStudyAreasSummerDeciduousForest <- getJIVisualization("all", "summer", "deciduous forest")
AllStudyAreasSummerConiferousForest <- getJIVisualization("all", "summer", "coniferous forest")
AllStudyAreasSummerFreshWater <- getJIVisualization("all", "summer", "fresh water")
AllStudyAreasSummerBareSoil <- getJIVisualization("all", "summer", "bare soil")
AllStudyAreasSummerGrassyHeathland <- getJIVisualization("all", "summer", "grassy heathland")
AllStudyAreasSummerHeathland <- getJIVisualization("all", "summer", "heathland")
AllStudyAreasSummerSwamp <- getJIVisualization("all", "summer", "swamp")
AllStudyAreasSummerShrubland <- getJIVisualization("all", "summer", "shrubland")
AllStudyAreasSummerRoad <- getJIVisualization("all", "summer", "road")

AllStudyAreasAutumnGrassland <- getJIVisualization("all", "autumn", "grassland")
AllStudyAreasAutumnDeciduousForest <- getJIVisualization("all", "autumn", "deciduous forest")
AllStudyAreasAutumnConiferousForest <- getJIVisualization("all", "autumn", "coniferous forest")
AllStudyAreasAutumnFreshWater <- getJIVisualization("all", "autumn", "fresh water")
AllStudyAreasAutumnBareSoil <- getJIVisualization("all", "autumn", "bare soil")
AllStudyAreasAutumnGrassyHeathland <- getJIVisualization("all", "autumn", "grassy heathland")
AllStudyAreasAutumnHeathland <- getJIVisualization("all", "autumn", "heathland")
AllStudyAreasAutumnSwamp <- getJIVisualization("all", "autumn", "swamp")
AllStudyAreasAutumnShrubland <- getJIVisualization("all", "autumn", "shrubland")
AllStudyAreasAutumnRoad <- getJIVisualization("all", "autumn", "road")

AllStudyAreasWinterGrassland <- getJIVisualization("all", "winter", "grassland")
AllStudyAreasWinterDeciduousForest <- getJIVisualization("all", "winter", "deciduous forest")
AllStudyAreasWinterConiferousForest <- getJIVisualization("all", "winter", "coniferous forest")
AllStudyAreasWinterFreshWater <- getJIVisualization("all", "winter", "fresh water")
AllStudyAreasWinterBareSoil <- getJIVisualization("all", "winter", "bare soil")
AllStudyAreasWinterGrassyHeathland <- getJIVisualization("all", "winter", "grassy heathland")
AllStudyAreasWinterHeathland <- getJIVisualization("all", "winter", "heathland")
AllStudyAreasWinterSwamp <- getJIVisualization("all", "winter", "swamp")
AllStudyAreasWinterShrubland <- getJIVisualization("all", "winter", "shrubland")
AllStudyAreasWinterRoad <- getJIVisualization("all", "winter", "road")


# Kraansvlak
KraansvlakAllSeasonAll <- getJIVisualization("Kraansvlak", "all", "all")
KraansvlakSpringAll <- getJIVisualization("Kraansvlak", "spring", "all")
KraansvlakSummerAll <- getJIVisualization("Kraansvlak", "summer", "all")
KraansvlakAutumnAll <- getJIVisualization("Kraansvlak", "autumn", "all")
KraansvlakWinterAll <- getJIVisualization("Kraansvlak", "winter", "all")

KraansvlakGrassland <- getJIVisualization("Kraansvlak", "all", "grassland")
KraansvlakDeciduousForest <- getJIVisualization("Kraansvlak", "all", "deciduous forest")
KraansvlakConiferousForest <- getJIVisualization("Kraansvlak", "all", "coniferous forest")
KraansvlakFreshWater <- getJIVisualization("Kraansvlak", "all", "fresh water")

KraansvlakWinterGrassland <- getJIVisualization("Kraansvlak", "winter", "grassland")
KraansvlakWinterGrassland <- getJIVisualization("Kraansvlak", "winter", "deciduous forest")

# Maashorst20172021
Maashorst20172021AllSeasonAll <- getJIVisualization("Maashorst20172021", "all", "all")
Maashorst20172021SpringAll <- getJIVisualization("Maashorst20172021", "spring", "all")
Maashorst20172021SummerAll <- getJIVisualization("Maashorst20172021", "summer", "all")
Maashorst20172021AutumnAll <- getJIVisualization("Maashorst20172021", "autumn", "all")
Maashorst20172021WinterAll <- getJIVisualization("Maashorst20172021", "winter", "all")

# SlikkenvdHeen
SlikkenvdHeenAllSeasonAll <- getJIVisualization("SlikkenvdHeen", "all", "all")
SlikkenvdHeenSpringAll <- getJIVisualization("SlikkenvdHeen", "spring", "all")
SlikkenvdHeenSummerAll <- getJIVisualization("SlikkenvdHeen", "summer", "all")
SlikkenvdHeenAutumnAll <- getJIVisualization("SlikkenvdHeen", "autumn", "all")
SlikkenvdHeenWinterAll <- getJIVisualization("SlikkenvdHeen", "winter", "all")

# Veluwe
VeluweAllSeasonAll <- getJIVisualization("Veluwe", "all", "all")
VeluweSpringAll <- getJIVisualization("Veluwe", "spring", "all")
VeluweSummerAll <- getJIVisualization("Veluwe", "summer", "all")
VeluweAutumnAll <- getJIVisualization("Veluwe", "autumn", "all")
VeluweWinterAll <- getJIVisualization("Veluwe", "winter", "all")







