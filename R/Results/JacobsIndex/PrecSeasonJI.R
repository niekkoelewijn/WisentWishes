# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to determine the influence of precipitation on the Jacob's index
### of a landuse class per season.

## Comparing JI for dry and rainy days per season

# Select spring
SpringRainy <- AllTrackPoints %>% 
  filter(season == "spring",
         precipitation_duration_day >= as.numeric(quantile(Spring$precipitation_duration_day, 0.75)))

SpringDry <- AllTrackPoints %>% 
  filter(season == "spring",
         precipitation_duration_day <= as.numeric(quantile(Spring$precipitation_duration_day, 0.25)))

# Select summer
SummerRainy <- AllTrackPoints %>% 
  filter(season == "summer",
         precipitation_duration_day >= as.numeric(quantile(Summer$precipitation_duration_day, 0.75)))

SummerDry <- AllTrackPoints %>% 
  filter(season == "summer",
         precipitation_duration_day <= as.numeric(quantile(Summer$precipitation_duration_day, 0.25)))

# Select autumn
AutumnRainy <- AllTrackPoints %>% 
  filter(season == "autumm",
         precipitation_duration_day >= as.numeric(quantile(Autumn$precipitation_duration_day, 0.75)))

AutumnDry <- AllTrackPoints %>% 
  filter(season == "autumm",
         precipitation_duration_day <= as.numeric(quantile(Autumn$precipitation_duration_day, 0.25)))

# Select winter
WinterRainy <- AllTrackPoints %>% 
  filter(season == "winter",
         precipitation_duration_day >= as.numeric(quantile(Winter$precipitation_duration_day, 0.75)))

WinterDry <- AllTrackPoints %>% 
  filter(season == "winter",
         precipitation_duration_day <= as.numeric(quantile(Winter$precipitation_duration_day, 0.25)))


## Split spring Rainy datasets in time classes

# Select 00:00:00 - 02:00:00
SpringRainy0002 <- SpringRainy %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SpringRainy0204 <- SpringRainy %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SpringRainy0406 <- SpringRainy %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SpringRainy0608 <- SpringRainy %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SpringRainy0810 <- SpringRainy %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SpringRainy1012 <- SpringRainy %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SpringRainy1214 <- SpringRainy %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SpringRainy1416 <- SpringRainy %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SpringRainy1618 <- SpringRainy %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SpringRainy1820 <- SpringRainy %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SpringRainy2022 <- SpringRainy %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SpringRainy2200 <- SpringRainy %>% 
  filter(hms >= hms("22:00:00"), hms >= hms("23:59:59"))


## Split summer Rainy datasets in time classes

# Select 00:00:00 - 02:00:00
SummerRainy0002 <- SummerRainy %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SummerRainy0204 <- SummerRainy %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SummerRainy0406 <- SummerRainy %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SummerRainy0608 <- SummerRainy %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SummerRainy0810 <- SummerRainy %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SummerRainy1012 <- SummerRainy %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SummerRainy1214 <- SummerRainy %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SummerRainy1416 <- SummerRainy %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SummerRainy1618 <- SummerRainy %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SummerRainy1820 <- SummerRainy %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SummerRainy2022 <- SummerRainy %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SummerRainy2200 <- SummerRainy %>% 
  filter(hms >= hms("22:00:00"), hms >= hms("23:59:59"))


## Split autumn Rainy datasets in time classes

# Select 00:00:00 - 02:00:00
AutumnRainy0002 <- AutumnRainy %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
AutumnRainy0204 <- AutumnRainy %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
AutumnRainy0406 <- AutumnRainy %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
AutumnRainy0608 <- AutumnRainy %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
AutumnRainy0810 <- AutumnRainy %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
AutumnRainy1012 <- AutumnRainy %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
AutumnRainy1214 <- AutumnRainy %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
AutumnRainy1416 <- AutumnRainy %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
AutumnRainy1618 <- AutumnRainy %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
AutumnRainy1820 <- AutumnRainy %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
AutumnRainy2022 <- AutumnRainy %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
AutumnRainy2200 <- AutumnRainy %>% 
  filter(hms >= hms("22:00:00"), hms >= hms("23:59:59"))


## Split winter Rainy datasets in time classes

# Select 00:00:00 - 02:00:00
WinterRainy0002 <- WinterRainy %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
WinterRainy0204 <- WinterRainy %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
WinterRainy0406 <- WinterRainy %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
WinterRainy0608 <- WinterRainy %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
WinterRainy0810 <- WinterRainy %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
WinterRainy1012 <- WinterRainy %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
WinterRainy1214 <- WinterRainy %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
WinterRainy1416 <- WinterRainy %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
WinterRainy1618 <- WinterRainy %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
WinterRainy1820 <- WinterRainy %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
WinterRainy2022 <- WinterRainy %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
WinterRainy2200 <- WinterRainy %>% 
  filter(hms >= hms("22:00:00"), hms >= hms("23:59:59"))


## Split spring Dry datasets in time classes

# Select 00:00:00 - 02:00:00
SpringDry0002 <- SpringDry %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SpringDry0204 <- SpringDry %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SpringDry0406 <- SpringDry %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SpringDry0608 <- SpringDry %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SpringDry0810 <- SpringDry %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SpringDry1012 <- SpringDry %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SpringDry1214 <- SpringDry %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SpringDry1416 <- SpringDry %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SpringDry1618 <- SpringDry %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SpringDry1820 <- SpringDry %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SpringDry2022 <- SpringDry %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SpringDry2200 <- SpringDry %>% 
  filter(hms >= hms("22:00:00"), hms >= hms("23:59:59"))


## Split summer Dry datasets in time classes

# Select 00:00:00 - 02:00:00
SummerDry0002 <- SummerDry %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SummerDry0204 <- SummerDry %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SummerDry0406 <- SummerDry %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SummerDry0608 <- SummerDry %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SummerDry0810 <- SummerDry %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SummerDry1012 <- SummerDry %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SummerDry1214 <- SummerDry %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SummerDry1416 <- SummerDry %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SummerDry1618 <- SummerDry %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SummerDry1820 <- SummerDry %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SummerDry2022 <- SummerDry %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SummerDry2200 <- SummerDry %>% 
  filter(hms >= hms("22:00:00"), hms >= hms("23:59:59"))


## Split autumn Dry datasets in time classes

# Select 00:00:00 - 02:00:00
AutumnDry0002 <- AutumnDry %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
AutumnDry0204 <- AutumnDry %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
AutumnDry0406 <- AutumnDry %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
AutumnDry0608 <- AutumnDry %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
AutumnDry0810 <- AutumnDry %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
AutumnDry1012 <- AutumnDry %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
AutumnDry1214 <- AutumnDry %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
AutumnDry1416 <- AutumnDry %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
AutumnDry1618 <- AutumnDry %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
AutumnDry1820 <- AutumnDry %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
AutumnDry2022 <- AutumnDry %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
AutumnDry2200 <- AutumnDry %>% 
  filter(hms >= hms("22:00:00"), hms >= hms("23:59:59"))


## Split winter Dry datasets in time classes

# Select 00:00:00 - 02:00:00
WinterDry0002 <- WinterDry %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
WinterDry0204 <- WinterDry %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
WinterDry0406 <- WinterDry %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
WinterDry0608 <- WinterDry %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
WinterDry0810 <- WinterDry %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
WinterDry1012 <- WinterDry %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
WinterDry1214 <- WinterDry %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
WinterDry1416 <- WinterDry %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
WinterDry1618 <- WinterDry %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
WinterDry1820 <- WinterDry %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
WinterDry2022 <- WinterDry %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
WinterDry2200 <- WinterDry %>% 
  filter(hms >= hms("22:00:00"), hms >= hms("23:59:59"))


## Create lists of timeframes per season (rainy)

# Spring rainy
SpringRainyTimeframeList <- list(SpringRainy0002, SpringRainy0204, SpringRainy0406, SpringRainy0608,
                                  SpringRainy0810, SpringRainy1012, SpringRainy1214, SpringRainy1416,
                                  SpringRainy1618, SpringRainy1820, SpringRainy2022, SpringRainy2200)

# Summer rainy
SummerRainyTimeframeList <- list(SummerRainy0002, SummerRainy0204, SummerRainy0406, SummerRainy0608,
                                  SummerRainy0810, SummerRainy1012, SummerRainy1214, SummerRainy1416,
                                  SummerRainy1618, SummerRainy1820, SummerRainy2022, SummerRainy2200)

# Autumn rainy
AutumnRainyTimeframeList <- list(AutumnRainy0002, AutumnRainy0204, AutumnRainy0406, AutumnRainy0608,
                                  AutumnRainy0810, AutumnRainy1012, AutumnRainy1214, AutumnRainy1416,
                                  AutumnRainy1618, AutumnRainy1820, AutumnRainy2022, AutumnRainy2200)

# Winter rainy
WinterRainyTimeframeList <- list(WinterRainy0002, WinterRainy0204, WinterRainy0406, WinterRainy0608,
                                  WinterRainy0810, WinterRainy1012, WinterRainy1214, WinterRainy1416,
                                  WinterRainy1618, WinterRainy1820, WinterRainy2022, WinterRainy2200)


## Create lists of timeframes per season (dry)

# Spring dry
SpringDryTimeframeList <- list(SpringDry0002, SpringDry0204, SpringDry0406, SpringDry0608,
                                 SpringDry0810, SpringDry1012, SpringDry1214, SpringDry1416,
                                 SpringDry1618, SpringDry1820, SpringDry2022, SpringDry2200)

# Summer dry
SummerDryTimeframeList <- list(SummerDry0002, SummerDry0204, SummerDry0406, SummerDry0608,
                                 SummerDry0810, SummerDry1012, SummerDry1214, SummerDry1416,
                                 SummerDry1618, SummerDry1820, SummerDry2022, SummerDry2200)

# Autumn dry
AutumnDryTimeframeList <- list(AutumnDry0002, AutumnDry0204, AutumnDry0406, AutumnDry0608,
                                 AutumnDry0810, AutumnDry1012, AutumnDry1214, AutumnDry1416,
                                 AutumnDry1618, AutumnDry1820, AutumnDry2022, AutumnDry2200)

# Winter dry
WinterDryTimeframeList <- list(WinterDry0002, WinterDry0204, WinterDry0406, WinterDry0608,
                                 WinterDry0810, WinterDry1012, WinterDry1214, WinterDry1416,
                                 WinterDry1618, WinterDry1820, WinterDry2022, WinterDry2200)

# Call JIperTimeframe for each season (rainy)
SpringRainyTimeframeJITable <- JIperTimeframe(SpringRainyTimeframeList)
SummerRainyTimeframeJITable <- JIperTimeframe(SummerRainyTimeframeList)
AutumnRainyTimeframeJITable <- JIperTimeframe(AutumnRainyTimeframeList)
WinterRainyTimeframeJITable <- JIperTimeframe(WinterRainyTimeframeList)

# Call JIperTimeframe for each season (dry)
SpringDryTimeframeJITable <- JIperTimeframe(SpringDryTimeframeList)
SummerDryTimeframeJITable <- JIperTimeframe(SummerDryTimeframeList)
AutumnDryTimeframeJITable <- JIperTimeframe(AutumnDryTimeframeList)
WinterDryTimeframeJITable <- JIperTimeframe(WinterDryTimeframeList)


## Melt the tables for visualization purposes

# Spring
SpringRainyTimeframeJITable$timeframe <- rownames(SpringRainyTimeframeJITable)
SpringRainyTimeframeJIMelt <- melt(SpringRainyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Spring") %>% 
  mutate(precipitation_duration = "most rainy 25%")
colnames(SpringRainyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Precipitation class")

SpringDryTimeframeJITable$timeframe <- rownames(SpringDryTimeframeJITable)
SpringDryTimeframeJIMelt <- melt(SpringDryTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Spring") %>% 
  mutate(precipitation_duration = "most dry 25%")
colnames(SpringDryTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Precipitation class")

# Summer
SummerRainyTimeframeJITable$timeframe <- rownames(SummerRainyTimeframeJITable)
SummerRainyTimeframeJIMelt <- melt(SummerRainyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Summer") %>% 
  mutate(precipitation_duration = "most rainy 25%")
colnames(SummerRainyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Precipitation class")

SummerDryTimeframeJITable$timeframe <- rownames(SummerDryTimeframeJITable)
SummerDryTimeframeJIMelt <- melt(SummerDryTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Summer") %>% 
  mutate(precipitation_duration = "most dry 25%")
colnames(SummerDryTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Precipitation class")

# Autumn
AutumnRainyTimeframeJITable$timeframe <- rownames(AutumnRainyTimeframeJITable)
AutumnRainyTimeframeJIMelt <- melt(AutumnRainyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Autumn") %>% 
  mutate(precipitation_duration = "most rainy 25%")
colnames(AutumnRainyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Precipitation class")

AutumnDryTimeframeJITable$timeframe <- rownames(AutumnDryTimeframeJITable)
AutumnDryTimeframeJIMelt <- melt(AutumnDryTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Autumn")  %>% 
  mutate(precipitation_duration = "most dry 25%")
colnames(AutumnDryTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Precipitation class")

# Winter
WinterRainyTimeframeJITable$timeframe <- rownames(WinterRainyTimeframeJITable)
WinterRainyTimeframeJIMelt <- melt(WinterRainyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Winter") %>% 
  mutate(precipitation_duration = "most rainy 25%")
colnames(WinterRainyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Precipitation class")

WinterDryTimeframeJITable$timeframe <- rownames(WinterDryTimeframeJITable)
WinterDryTimeframeJIMelt <- melt(WinterDryTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Winter")  %>% 
  mutate(precipitation_duration = "most dry 25%")
colnames(WinterDryTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Precipitation class")

# Create merged JI per time of day table
TimeframeJIPrec <- rbind(SpringRainyTimeframeJIMelt, SpringDryTimeframeJIMelt,
                        SummerRainyTimeframeJIMelt, SummerDryTimeframeJIMelt,
                        AutumnRainyTimeframeJIMelt, AutumnDryTimeframeJIMelt,
                        WinterRainyTimeframeJIMelt, WinterDryTimeframeJIMelt)

# Grassland Spring
GrasslandPrecSpringTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "grassland" &
                                                                         TimeframeJIPrec$season == "Spring"),],
                                           aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassland, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
GrasslandPrecSpringTimeframeJIVis

# Grassland Summer
GrasslandPrecSummerTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "grassland" &
                                                                         TimeframeJIPrec$season == "Summer"),],
                                           aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassland, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
GrasslandPrecSummerTimeframeJIVis

# Grassland Autumn
GrasslandPrecAutumnTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "grassland" &
                                                                         TimeframeJIPrec$season == "Autumn"),],
                                           aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassland, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
GrasslandPrecAutumnTimeframeJIVis

# Grassland Winter
GrasslandPrecWinterTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "grassland" &
                                                                         TimeframeJIPrec$season == "Winter"),],
                                           aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassland, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
GrasslandPrecWinterTimeframeJIVis


# Deciduous forest Spring
DecForestPrecSpringTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "deciduous forest" &
                                                                         TimeframeJIPrec$season == "Spring"),],
                                           aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Deciduous forest, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
DecForestPrecSpringTimeframeJIVis

# Deciduous forest Summer
DecForestPrecSummerTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "deciduous forest" &
                                                                         TimeframeJIPrec$season == "Summer"),],
                                           aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Deciduous forest, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
DecForestPrecSummerTimeframeJIVis

# Deciduous forest Autumn
DecForestPrecAutumnTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "deciduous forest" &
                                                                         TimeframeJIPrec$season == "Autumn"),],
                                           aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Deciduous forest, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
DecForestPrecAutumnTimeframeJIVis

# Deciduous forest Winter
DecForestPrecWinterTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "deciduous forest" &
                                                                         TimeframeJIPrec$season == "Winter"),],
                                           aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Deciduous forest, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
DecForestPrecWinterTimeframeJIVis

# Coniferous forest Spring
ConForestPrecSpringTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "coniferous forest" &
                                                                         TimeframeJIPrec$season == "Spring"),],
                                           aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Coniferous forest, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
ConForestPrecSpringTimeframeJIVis

# Coniferous forest Summer
ConForestPrecSummerTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "coniferous forest" &
                                                                         TimeframeJIPrec$season == "Summer"),],
                                           aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Coniferous forest, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
ConForestPrecSummerTimeframeJIVis

# Coniferous forest Autumn
ConForestPrecAutumnTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "coniferous forest" &
                                                                         TimeframeJIPrec$season == "Autumn"),],
                                           aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Coniferous forest, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
ConForestPrecAutumnTimeframeJIVis

# Coniferous forest Winter
ConForestPrecWinterTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "coniferous forest" &
                                                                         TimeframeJIPrec$season == "Winter"),],
                                           aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Coniferous forest, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
ConForestPrecWinterTimeframeJIVis

# Fresh water Spring
FreshWaterPrecSpringTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "fresh water" &
                                                                          TimeframeJIPrec$season == "Spring"),],
                                            aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Fresh water, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
FreshWaterPrecSpringTimeframeJIVis

# Fresh water Summer
FreshWaterPrecSummerTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "fresh water" &
                                                                          TimeframeJIPrec$season == "Summer"),],
                                            aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Fresh water, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
FreshWaterPrecSummerTimeframeJIVis

# Fresh water Autumn
FreshWaterPrecAutumnTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "fresh water" &
                                                                          TimeframeJIPrec$season == "Autumn"),],
                                            aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Fresh water, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
FreshWaterPrecAutumnTimeframeJIVis

# Fresh water Winter
FreshWaterPrecWinterTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "fresh water" &
                                                                          TimeframeJIPrec$season == "Winter"),],
                                            aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Fresh water, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
FreshWaterPrecWinterTimeframeJIVis

# Bare soil Spring
BareSoilPrecSpringTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "bare soil" &
                                                                        TimeframeJIPrec$season == "Spring"),],
                                          aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Bare soil, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
BareSoilPrecSpringTimeframeJIVis

# Bare soil Summer
BareSoilPrecSummerTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "bare soil" &
                                                                        TimeframeJIPrec$season == "Summer"),],
                                          aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Bare soil, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
BareSoilPrecSummerTimeframeJIVis

# Bare soil Autumn
BareSoilPrecAutumnTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "bare soil" &
                                                                        TimeframeJIPrec$season == "Autumn"),],
                                          aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Bare soil, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
BareSoilPrecAutumnTimeframeJIVis

# Bare soil Winter
BareSoilPrecWinterTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "bare soil" &
                                                                        TimeframeJIPrec$season == "Winter"),],
                                          aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Bare soil, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
BareSoilPrecWinterTimeframeJIVis

# Heathland Spring
HeathPrecSpringTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "heathland" &
                                                                     TimeframeJIPrec$season == "Spring"),],
                                       aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Heathland, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
HeathPrecSpringTimeframeJIVis

# Heathland Summer
HeathPrecSummerTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "heathland" &
                                                                     TimeframeJIPrec$season == "Summer"),],
                                       aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Heathland, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
HeathPrecSummerTimeframeJIVis

# Heathland Autumn
HeathPrecAutumnTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "heathland" &
                                                                     TimeframeJIPrec$season == "Autumn"),],
                                       aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Heathland, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
HeathPrecAutumnTimeframeJIVis

# Heathland Winter
HeathPrecWinterTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "heathland" &
                                                                     TimeframeJIPrec$season == "Winter"),],
                                       aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Heathland, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
HeathPrecWinterTimeframeJIVis

# Shrubland Spring
ShrubPrecSpringTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "shrubland" &
                                                                     TimeframeJIPrec$season == "Spring"),],
                                       aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Shrubland, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
ShrubPrecSpringTimeframeJIVis

# Shrubland Summer
ShrubPrecSummerTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "shrubland" &
                                                                     TimeframeJIPrec$season == "Summer"),],
                                       aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Shrubland, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
ShrubPrecSummerTimeframeJIVis

# Shrubland Autumn
ShrubPrecAutumnTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "shrubland" &
                                                                     TimeframeJIPrec$season == "Autumn"),],
                                       aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Shrubland, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
ShrubPrecAutumnTimeframeJIVis

# Shrubland Winter
ShrubPrecWinterTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "shrubland" &
                                                                     TimeframeJIPrec$season == "Winter"),],
                                       aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Shrubland, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
ShrubPrecWinterTimeframeJIVis

# Swamp Spring
SwampPrecSpringTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "swamp" &
                                                                     TimeframeJIPrec$season == "Spring"),],
                                       aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Swamp, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
SwampPrecSpringTimeframeJIVis

# Swamp Summer
SwampPrecSummerTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "swamp" &
                                                                     TimeframeJIPrec$season == "Summer"),],
                                       aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Swamp, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
SwampPrecSummerTimeframeJIVis

# Swamp Autumn
SwampPrecAutumnTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "swamp" &
                                                                     TimeframeJIPrec$season == "Autumn"),],
                                       aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Swamp, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
SwampPrecAutumnTimeframeJIVis

# Swamp Winter
SwampPrecWinterTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "swamp" &
                                                                     TimeframeJIPrec$season == "Winter"),],
                                       aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Swamp, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
SwampPrecWinterTimeframeJIVis

# Grassy heathland Spring
GrassHeathPrecSpringTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "grassy heathland" &
                                                                          TimeframeJIPrec$season == "Spring"),],
                                            aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassy heathland, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
GrassHeathPrecSpringTimeframeJIVis

# Grassy heathland Summer
GrassHeathPrecSummerTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "grassy heathland" &
                                                                          TimeframeJIPrec$season == "Summer"),],
                                            aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassy heathland, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
GrassHeathPrecSummerTimeframeJIVis

# Grassy heathland Autumn
GrassHeathPrecAutumnTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "grassy heathland" &
                                                                          TimeframeJIPrec$season == "Autumn"),],
                                            aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassy heathland, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
GrassHeathPrecAutumnTimeframeJIVis

# Grassy heathland Winter
GrassHeathPrecWinterTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "grassy heathland" &
                                                                          TimeframeJIPrec$season == "Winter"),],
                                            aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassy heathland, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
GrassHeathPrecWinterTimeframeJIVis


# Road Spring
RoadPrecSpringTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "road" &
                                                                    TimeframeJIPrec$season == "Spring"),],
                                      aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Road, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
RoadPrecSpringTimeframeJIVis

# Road Summer
RoadPrecSummerTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "road" &
                                                                    TimeframeJIPrec$season == "Summer"),],
                                      aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Road, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
RoadPrecSummerTimeframeJIVis

# Road Autumn
RoadPrecAutumnTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "road" &
                                                                    TimeframeJIPrec$season == "Autumn"),],
                                      aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Road, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
RoadPrecAutumnTimeframeJIVis

# Road Winter
RoadPrecWinterTimeframeJIVis <- ggplot(data = TimeframeJIPrec[which(TimeframeJIPrec$class == "road" &
                                                                    TimeframeJIPrec$season == "Winter"),],
                                      aes(x = timeframe, y = JI, group = `Precipitation class`, colour = `Precipitation class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Road, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF"))
RoadPrecWinterTimeframeJIVis
