# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to determine the influence of wind on the Jacob's index
### of a landuse class per season.

## Comparing JI for windy and quiet days per season

# Select spring
SpringQuiet <- AllTrackPoints %>% 
  filter(season == "spring",
         average_windspeed_day <= as.numeric(quantile(Spring$average_windspeed_day, 0.25)))

SpringWindy <- AllTrackPoints %>% 
  filter(season == "spring",
         average_windspeed_day >= as.numeric(quantile(Spring$average_windspeed_day, 0.75)))

# Select summer
SummerQuiet <- AllTrackPoints %>% 
  filter(season == "summer",
         average_windspeed_day <= as.numeric(quantile(Summer$average_windspeed_day, 0.25)))

SummerWindy <- AllTrackPoints %>% 
  filter(season == "summer",
         average_windspeed_day >= as.numeric(quantile(Summer$average_windspeed_day, 0.75)))

# Select autumn
AutumnQuiet <- AllTrackPoints %>% 
  filter(season == "autumm",
         average_windspeed_day <= as.numeric(quantile(Autumn$average_windspeed_day, 0.25)))

AutumnWindy <- AllTrackPoints %>% 
  filter(season == "autumm",
         average_windspeed_day >= as.numeric(quantile(Autumn$average_windspeed_day, 0.75)))

# Select winter
WinterQuiet <- AllTrackPoints %>% 
  filter(season == "winter",
         average_windspeed_day <= as.numeric(quantile(Winter$average_windspeed_day, 0.25)))

WinterWindy <- AllTrackPoints %>% 
  filter(season == "winter",
         average_windspeed_day >= as.numeric(quantile(Winter$average_windspeed_day, 0.75)))


## Split spring quiet datasets in time classes

# Select 00:00:00 - 02:00:00
SpringQuiet0002 <- SpringQuiet %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SpringQuiet0204 <- SpringQuiet %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SpringQuiet0406 <- SpringQuiet %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SpringQuiet0608 <- SpringQuiet %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SpringQuiet0810 <- SpringQuiet %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SpringQuiet1012 <- SpringQuiet %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SpringQuiet1214 <- SpringQuiet %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SpringQuiet1416 <- SpringQuiet %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SpringQuiet1618 <- SpringQuiet %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SpringQuiet1820 <- SpringQuiet %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SpringQuiet2022 <- SpringQuiet %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SpringQuiet2200 <- SpringQuiet %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split summer quiet datasets in time classes

# Select 00:00:00 - 02:00:00
SummerQuiet0002 <- SummerQuiet %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SummerQuiet0204 <- SummerQuiet %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SummerQuiet0406 <- SummerQuiet %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SummerQuiet0608 <- SummerQuiet %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SummerQuiet0810 <- SummerQuiet %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SummerQuiet1012 <- SummerQuiet %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SummerQuiet1214 <- SummerQuiet %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SummerQuiet1416 <- SummerQuiet %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SummerQuiet1618 <- SummerQuiet %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SummerQuiet1820 <- SummerQuiet %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SummerQuiet2022 <- SummerQuiet %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SummerQuiet2200 <- SummerQuiet %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split autumn quiet datasets in time classes

# Select 00:00:00 - 02:00:00
AutumnQuiet0002 <- AutumnQuiet %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
AutumnQuiet0204 <- AutumnQuiet %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
AutumnQuiet0406 <- AutumnQuiet %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
AutumnQuiet0608 <- AutumnQuiet %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
AutumnQuiet0810 <- AutumnQuiet %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
AutumnQuiet1012 <- AutumnQuiet %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
AutumnQuiet1214 <- AutumnQuiet %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
AutumnQuiet1416 <- AutumnQuiet %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
AutumnQuiet1618 <- AutumnQuiet %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
AutumnQuiet1820 <- AutumnQuiet %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
AutumnQuiet2022 <- AutumnQuiet %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
AutumnQuiet2200 <- AutumnQuiet %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split winter quiet datasets in time classes

# Select 00:00:00 - 02:00:00
WinterQuiet0002 <- WinterQuiet %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
WinterQuiet0204 <- WinterQuiet %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
WinterQuiet0406 <- WinterQuiet %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
WinterQuiet0608 <- WinterQuiet %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
WinterQuiet0810 <- WinterQuiet %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
WinterQuiet1012 <- WinterQuiet %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
WinterQuiet1214 <- WinterQuiet %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
WinterQuiet1416 <- WinterQuiet %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
WinterQuiet1618 <- WinterQuiet %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
WinterQuiet1820 <- WinterQuiet %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
WinterQuiet2022 <- WinterQuiet %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
WinterQuiet2200 <- WinterQuiet %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split spring windy datasets in time classes

# Select 00:00:00 - 02:00:00
SpringWindy0002 <- SpringWindy %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SpringWindy0204 <- SpringWindy %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SpringWindy0406 <- SpringWindy %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SpringWindy0608 <- SpringWindy %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SpringWindy0810 <- SpringWindy %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SpringWindy1012 <- SpringWindy %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SpringWindy1214 <- SpringWindy %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SpringWindy1416 <- SpringWindy %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SpringWindy1618 <- SpringWindy %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SpringWindy1820 <- SpringWindy %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SpringWindy2022 <- SpringWindy %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SpringWindy2200 <- SpringWindy %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split summer windy datasets in time classes

# Select 00:00:00 - 02:00:00
SummerWindy0002 <- SummerWindy %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SummerWindy0204 <- SummerWindy %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SummerWindy0406 <- SummerWindy %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SummerWindy0608 <- SummerWindy %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SummerWindy0810 <- SummerWindy %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SummerWindy1012 <- SummerWindy %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SummerWindy1214 <- SummerWindy %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SummerWindy1416 <- SummerWindy %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SummerWindy1618 <- SummerWindy %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SummerWindy1820 <- SummerWindy %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SummerWindy2022 <- SummerWindy %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SummerWindy2200 <- SummerWindy %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split autumn windy datasets in time classes

# Select 00:00:00 - 02:00:00
AutumnWindy0002 <- AutumnWindy %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
AutumnWindy0204 <- AutumnWindy %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
AutumnWindy0406 <- AutumnWindy %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
AutumnWindy0608 <- AutumnWindy %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
AutumnWindy0810 <- AutumnWindy %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
AutumnWindy1012 <- AutumnWindy %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
AutumnWindy1214 <- AutumnWindy %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
AutumnWindy1416 <- AutumnWindy %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
AutumnWindy1618 <- AutumnWindy %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
AutumnWindy1820 <- AutumnWindy %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
AutumnWindy2022 <- AutumnWindy %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
AutumnWindy2200 <- AutumnWindy %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split winter windy datasets in time classes

# Select 00:00:00 - 02:00:00
WinterWindy0002 <- WinterWindy %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
WinterWindy0204 <- WinterWindy %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
WinterWindy0406 <- WinterWindy %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
WinterWindy0608 <- WinterWindy %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
WinterWindy0810 <- WinterWindy %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
WinterWindy1012 <- WinterWindy %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
WinterWindy1214 <- WinterWindy %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
WinterWindy1416 <- WinterWindy %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
WinterWindy1618 <- WinterWindy %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
WinterWindy1820 <- WinterWindy %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
WinterWindy2022 <- WinterWindy %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
WinterWindy2200 <- WinterWindy %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Create lists of timeframes per season (quiet)

# Spring quiet
SpringQuietTimeframeList <- list(SpringQuiet0002, SpringQuiet0204, SpringQuiet0406, SpringQuiet0608,
                                  SpringQuiet0810, SpringQuiet1012, SpringQuiet1214, SpringQuiet1416,
                                  SpringQuiet1618, SpringQuiet1820, SpringQuiet2022, SpringQuiet2200)

# Summer quiet
SummerQuietTimeframeList <- list(SummerQuiet0002, SummerQuiet0204, SummerQuiet0406, SummerQuiet0608,
                                  SummerQuiet0810, SummerQuiet1012, SummerQuiet1214, SummerQuiet1416,
                                  SummerQuiet1618, SummerQuiet1820, SummerQuiet2022, SummerQuiet2200)

# Autumn quiet
AutumnQuietTimeframeList <- list(AutumnQuiet0002, AutumnQuiet0204, AutumnQuiet0406, AutumnQuiet0608,
                                  AutumnQuiet0810, AutumnQuiet1012, AutumnQuiet1214, AutumnQuiet1416,
                                  AutumnQuiet1618, AutumnQuiet1820, AutumnQuiet2022, AutumnQuiet2200)

# Winter quiet
WinterQuietTimeframeList <- list(WinterQuiet0002, WinterQuiet0204, WinterQuiet0406, WinterQuiet0608,
                                  WinterQuiet0810, WinterQuiet1012, WinterQuiet1214, WinterQuiet1416,
                                  WinterQuiet1618, WinterQuiet1820, WinterQuiet2022, WinterQuiet2200)


## Create lists of timeframes per season (windy)

# Spring windy
SpringWindyTimeframeList <- list(SpringWindy0002, SpringWindy0204, SpringWindy0406, SpringWindy0608,
                                 SpringWindy0810, SpringWindy1012, SpringWindy1214, SpringWindy1416,
                                 SpringWindy1618, SpringWindy1820, SpringWindy2022, SpringWindy2200)

# Summer windy
SummerWindyTimeframeList <- list(SummerWindy0002, SummerWindy0204, SummerWindy0406, SummerWindy0608,
                                 SummerWindy0810, SummerWindy1012, SummerWindy1214, SummerWindy1416,
                                 SummerWindy1618, SummerWindy1820, SummerWindy2022, SummerWindy2200)

# Autumn windy
AutumnWindyTimeframeList <- list(AutumnWindy0002, AutumnWindy0204, AutumnWindy0406, AutumnWindy0608,
                                 AutumnWindy0810, AutumnWindy1012, AutumnWindy1214, AutumnWindy1416,
                                 AutumnWindy1618, AutumnWindy1820, AutumnWindy2022, AutumnWindy2200)

# Winter windy
WinterWindyTimeframeList <- list(WinterWindy0002, WinterWindy0204, WinterWindy0406, WinterWindy0608,
                                 WinterWindy0810, WinterWindy1012, WinterWindy1214, WinterWindy1416,
                                 WinterWindy1618, WinterWindy1820, WinterWindy2022, WinterWindy2200)

# Call JIperTimeframe for each season (quiet)
SpringQuietTimeframeJITable <- JIperTimeframe(SpringQuietTimeframeList)
SummerQuietTimeframeJITable <- JIperTimeframe(SummerQuietTimeframeList)
AutumnQuietTimeframeJITable <- JIperTimeframe(AutumnQuietTimeframeList)
WinterQuietTimeframeJITable <- JIperTimeframe(WinterQuietTimeframeList)

# Call JIperTimeframe for each season (windy)
SpringWindyTimeframeJITable <- JIperTimeframe(SpringWindyTimeframeList)
SummerWindyTimeframeJITable <- JIperTimeframe(SummerWindyTimeframeList)
AutumnWindyTimeframeJITable <- JIperTimeframe(AutumnWindyTimeframeList)
WinterWindyTimeframeJITable <- JIperTimeframe(WinterWindyTimeframeList)


## Melt the tables for visualization purposes

# Spring
SpringQuietTimeframeJITable$timeframe <- rownames(SpringQuietTimeframeJITable)
SpringQuietTimeframeJIMelt <- melt(SpringQuietTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Spring") %>% 
  mutate(average_windspeed = "least windy 25%")
colnames(SpringQuietTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Wind class")

SpringWindyTimeframeJITable$timeframe <- rownames(SpringWindyTimeframeJITable)
SpringWindyTimeframeJIMelt <- melt(SpringWindyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Spring") %>% 
  mutate(average_windspeed = "most windy 25%")
colnames(SpringWindyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Wind class")

# Summer
SummerQuietTimeframeJITable$timeframe <- rownames(SummerQuietTimeframeJITable)
SummerQuietTimeframeJIMelt <- melt(SummerQuietTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Summer") %>% 
  mutate(average_windspeed = "least windy 25%")
colnames(SummerQuietTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Wind class")

SummerWindyTimeframeJITable$timeframe <- rownames(SummerWindyTimeframeJITable)
SummerWindyTimeframeJIMelt <- melt(SummerWindyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Summer") %>% 
  mutate(average_windspeed = "most windy 25%")
colnames(SummerWindyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Wind class")

# Autumn
AutumnQuietTimeframeJITable$timeframe <- rownames(AutumnQuietTimeframeJITable)
AutumnQuietTimeframeJIMelt <- melt(AutumnQuietTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Autumn") %>% 
  mutate(average_windspeed = "least windy 25%")
colnames(AutumnQuietTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Wind class")

AutumnWindyTimeframeJITable$timeframe <- rownames(AutumnWindyTimeframeJITable)
AutumnWindyTimeframeJIMelt <- melt(AutumnWindyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Autumn")  %>% 
  mutate(average_windspeed = "most windy 25%")
colnames(AutumnWindyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Wind class")

# Winter
WinterQuietTimeframeJITable$timeframe <- rownames(WinterQuietTimeframeJITable)
WinterQuietTimeframeJIMelt <- melt(WinterQuietTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Winter") %>% 
  mutate(average_windspeed = "least windy 25%")
colnames(WinterQuietTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Wind class")

WinterWindyTimeframeJITable$timeframe <- rownames(WinterWindyTimeframeJITable)
WinterWindyTimeframeJIMelt <- melt(WinterWindyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Winter")  %>% 
  mutate(average_windspeed = "most windy 25%")
colnames(WinterWindyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Wind class")

# Create merged JI per time of day table
TimeframeJIWind <- rbind(SpringQuietTimeframeJIMelt, SpringWindyTimeframeJIMelt,
                        SummerQuietTimeframeJIMelt, SummerWindyTimeframeJIMelt,
                        AutumnQuietTimeframeJIMelt, AutumnWindyTimeframeJIMelt,
                        WinterQuietTimeframeJIMelt, WinterWindyTimeframeJIMelt)

# Grassland Spring
GrasslandWindSpringTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "grassland" &
                                                                         TimeframeJIWind$season == "Spring"),],
                                           aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassland, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
GrasslandWindSpringTimeframeJIVis

# Grassland Summer
GrasslandWindSummerTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "grassland" &
                                                                         TimeframeJIWind$season == "Summer"),],
                                           aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassland, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
GrasslandWindSummerTimeframeJIVis

# Grassland Autumn
GrasslandWindAutumnTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "grassland" &
                                                                         TimeframeJIWind$season == "Autumn"),],
                                           aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassland, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
GrasslandWindAutumnTimeframeJIVis

# Grassland Winter
GrasslandWindWinterTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "grassland" &
                                                                         TimeframeJIWind$season == "Winter"),],
                                           aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassland, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
GrasslandWindWinterTimeframeJIVis


# Deciduous forest Spring
DecForestWindSpringTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "deciduous forest" &
                                                                         TimeframeJIWind$season == "Spring"),],
                                           aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Deciduous forest, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
DecForestWindSpringTimeframeJIVis

# Deciduous forest Summer
DecForestWindSummerTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "deciduous forest" &
                                                                         TimeframeJIWind$season == "Summer"),],
                                           aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Deciduous forest, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
DecForestWindSummerTimeframeJIVis

# Deciduous forest Autumn
DecForestWindAutumnTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "deciduous forest" &
                                                                         TimeframeJIWind$season == "Autumn"),],
                                           aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Deciduous forest, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
DecForestWindAutumnTimeframeJIVis

# Deciduous forest Winter
DecForestWindWinterTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "deciduous forest" &
                                                                         TimeframeJIWind$season == "Winter"),],
                                           aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Deciduous forest, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
DecForestWindWinterTimeframeJIVis

# Coniferous forest Spring
ConForestWindSpringTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "coniferous forest" &
                                                                         TimeframeJIWind$season == "Spring"),],
                                           aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Coniferous forest, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
ConForestWindSpringTimeframeJIVis

# Coniferous forest Summer
ConForestWindSummerTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "coniferous forest" &
                                                                         TimeframeJIWind$season == "Summer"),],
                                           aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Coniferous forest, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
ConForestWindSummerTimeframeJIVis

# Coniferous forest Autumn
ConForestWindAutumnTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "coniferous forest" &
                                                                         TimeframeJIWind$season == "Autumn"),],
                                           aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Coniferous forest, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
ConForestWindAutumnTimeframeJIVis

# Coniferous forest Winter
ConForestWindWinterTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "coniferous forest" &
                                                                         TimeframeJIWind$season == "Winter"),],
                                           aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Coniferous forest, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
ConForestWindWinterTimeframeJIVis

# Fresh water Spring
FreshWaterWindSpringTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "fresh water" &
                                                                          TimeframeJIWind$season == "Spring"),],
                                            aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Fresh water, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
FreshWaterWindSpringTimeframeJIVis

# Fresh water Summer
FreshWaterWindSummerTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "fresh water" &
                                                                          TimeframeJIWind$season == "Summer"),],
                                            aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Fresh water, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
FreshWaterWindSummerTimeframeJIVis

# Fresh water Autumn
FreshWaterWindAutumnTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "fresh water" &
                                                                          TimeframeJIWind$season == "Autumn"),],
                                            aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Fresh water, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
FreshWaterWindAutumnTimeframeJIVis

# Fresh water Winter
FreshWaterWindWinterTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "fresh water" &
                                                                          TimeframeJIWind$season == "Winter"),],
                                            aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Fresh water, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
FreshWaterWindWinterTimeframeJIVis

# Bare soil Spring
BareSoilWindSpringTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "bare soil" &
                                                                        TimeframeJIWind$season == "Spring"),],
                                          aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Bare soil, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
BareSoilWindSpringTimeframeJIVis

# Bare soil Summer
BareSoilWindSummerTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "bare soil" &
                                                                        TimeframeJIWind$season == "Summer"),],
                                          aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Bare soil, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
BareSoilWindSummerTimeframeJIVis

# Bare soil Autumn
BareSoilWindAutumnTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "bare soil" &
                                                                        TimeframeJIWind$season == "Autumn"),],
                                          aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Bare soil, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
BareSoilWindAutumnTimeframeJIVis

# Bare soil Winter
BareSoilWindWinterTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "bare soil" &
                                                                        TimeframeJIWind$season == "Winter"),],
                                          aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Bare soil, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
BareSoilWindWinterTimeframeJIVis

# Heathland Spring
HeathWindSpringTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "heathland" &
                                                                     TimeframeJIWind$season == "Spring"),],
                                       aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Heathland, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
HeathWindSpringTimeframeJIVis

# Heathland Summer
HeathWindSummerTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "heathland" &
                                                                     TimeframeJIWind$season == "Summer"),],
                                       aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Heathland, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
HeathWindSummerTimeframeJIVis

# Heathland Autumn
HeathWindAutumnTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "heathland" &
                                                                     TimeframeJIWind$season == "Autumn"),],
                                       aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Heathland, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
HeathWindAutumnTimeframeJIVis

# Heathland Winter
HeathWindWinterTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "heathland" &
                                                                     TimeframeJIWind$season == "Winter"),],
                                       aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Heathland, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
HeathWindWinterTimeframeJIVis

# Shrubland Spring
ShrubWindSpringTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "shrubland" &
                                                                     TimeframeJIWind$season == "Spring"),],
                                       aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Shrubland, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
ShrubWindSpringTimeframeJIVis

# Shrubland Summer
ShrubWindSummerTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "shrubland" &
                                                                     TimeframeJIWind$season == "Summer"),],
                                       aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Shrubland, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
ShrubWindSummerTimeframeJIVis

# Shrubland Autumn
ShrubWindAutumnTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "shrubland" &
                                                                     TimeframeJIWind$season == "Autumn"),],
                                       aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Shrubland, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
ShrubWindAutumnTimeframeJIVis

# Shrubland Winter
ShrubWindWinterTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "shrubland" &
                                                                     TimeframeJIWind$season == "Winter"),],
                                       aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Shrubland, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
ShrubWindWinterTimeframeJIVis

# Swamp Spring
SwampWindSpringTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "swamp" &
                                                                     TimeframeJIWind$season == "Spring"),],
                                       aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Swamp, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
SwampWindSpringTimeframeJIVis

# Swamp Summer
SwampWindSummerTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "swamp" &
                                                                     TimeframeJIWind$season == "Summer"),],
                                       aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Swamp, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
SwampWindSummerTimeframeJIVis

# Swamp Autumn
SwampWindAutumnTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "swamp" &
                                                                     TimeframeJIWind$season == "Autumn"),],
                                       aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Swamp, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
SwampWindAutumnTimeframeJIVis

# Swamp Winter
SwampWindWinterTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "swamp" &
                                                                     TimeframeJIWind$season == "Winter"),],
                                       aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Swamp, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
SwampWindWinterTimeframeJIVis

# Grassy heathland Spring
GrassHeathWindSpringTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "grassy heathland" &
                                                                          TimeframeJIWind$season == "Spring"),],
                                            aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassy heathland, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
GrassHeathWindSpringTimeframeJIVis

# Grassy heathland Summer
GrassHeathWindSummerTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "grassy heathland" &
                                                                          TimeframeJIWind$season == "Summer"),],
                                            aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassy heathland, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
GrassHeathWindSummerTimeframeJIVis

# Grassy heathland Autumn
GrassHeathWindAutumnTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "grassy heathland" &
                                                                          TimeframeJIWind$season == "Autumn"),],
                                            aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassy heathland, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
GrassHeathWindAutumnTimeframeJIVis

# Grassy heathland Winter
GrassHeathWindWinterTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "grassy heathland" &
                                                                          TimeframeJIWind$season == "Winter"),],
                                            aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassy heathland, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
GrassHeathWindWinterTimeframeJIVis


# Road Spring
RoadWindSpringTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "road" &
                                                                    TimeframeJIWind$season == "Spring"),],
                                      aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Road, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
RoadWindSpringTimeframeJIVis

# Road Summer
RoadWindSummerTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "road" &
                                                                    TimeframeJIWind$season == "Summer"),],
                                      aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Road, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
RoadWindSummerTimeframeJIVis

# Road Autumn
RoadWindAutumnTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "road" &
                                                                    TimeframeJIWind$season == "Autumn"),],
                                      aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Road, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
RoadWindAutumnTimeframeJIVis

# Road Winter
RoadWindWinterTimeframeJIVis <- ggplot(data = TimeframeJIWind[which(TimeframeJIWind$class == "road" &
                                                                    TimeframeJIWind$season == "Winter"),],
                                      aes(x = timeframe, y = JI, group = `Wind class`, colour = `Wind class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Road, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#3300FF", "#FF0033"))
RoadWindWinterTimeframeJIVis
