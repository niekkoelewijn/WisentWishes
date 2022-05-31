# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to determine the influence of sunshine on the Jacob's index
### of a landuse class per season.

## Comparing JI for sunny and cloudy days per season

# Select spring
SpringCloudy <- AllTrackPoints %>% 
  filter(season == "spring",
         sunshine_duration_day <= as.numeric(quantile(Spring$sunshine_duration_day, 0.25)))

SpringSunny <- AllTrackPoints %>% 
  filter(season == "spring",
         sunshine_duration_day >= as.numeric(quantile(Spring$sunshine_duration_day, 0.75)))

# Select summer
SummerCloudy <- AllTrackPoints %>% 
  filter(season == "summer",
         sunshine_duration_day <= as.numeric(quantile(Summer$sunshine_duration_day, 0.25)))

SummerSunny <- AllTrackPoints %>% 
  filter(season == "summer",
         sunshine_duration_day >= as.numeric(quantile(Summer$sunshine_duration_day, 0.75)))

# Select autumn
AutumnCloudy <- AllTrackPoints %>% 
  filter(season == "autumm",
         sunshine_duration_day <= as.numeric(quantile(Autumn$sunshine_duration_day, 0.25)))

AutumnSunny <- AllTrackPoints %>% 
  filter(season == "autumm",
         sunshine_duration_day >= as.numeric(quantile(Autumn$sunshine_duration_day, 0.75)))

# Select winter
WinterCloudy <- AllTrackPoints %>% 
  filter(season == "winter",
         sunshine_duration_day <= as.numeric(quantile(Winter$sunshine_duration_day, 0.25)))

WinterSunny <- AllTrackPoints %>% 
  filter(season == "winter",
         sunshine_duration_day >= as.numeric(quantile(Winter$sunshine_duration_day, 0.75)))


## Split spring cloudy datasets in time classes

# Select 00:00:00 - 02:00:00
SpringCloudy0002 <- SpringCloudy %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SpringCloudy0204 <- SpringCloudy %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SpringCloudy0406 <- SpringCloudy %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SpringCloudy0608 <- SpringCloudy %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SpringCloudy0810 <- SpringCloudy %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SpringCloudy1012 <- SpringCloudy %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SpringCloudy1214 <- SpringCloudy %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SpringCloudy1416 <- SpringCloudy %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SpringCloudy1618 <- SpringCloudy %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SpringCloudy1820 <- SpringCloudy %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SpringCloudy2022 <- SpringCloudy %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SpringCloudy2200 <- SpringCloudy %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split summer cloudy datasets in time classes

# Select 00:00:00 - 02:00:00
SummerCloudy0002 <- SummerCloudy %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SummerCloudy0204 <- SummerCloudy %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SummerCloudy0406 <- SummerCloudy %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SummerCloudy0608 <- SummerCloudy %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SummerCloudy0810 <- SummerCloudy %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SummerCloudy1012 <- SummerCloudy %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SummerCloudy1214 <- SummerCloudy %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SummerCloudy1416 <- SummerCloudy %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SummerCloudy1618 <- SummerCloudy %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SummerCloudy1820 <- SummerCloudy %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SummerCloudy2022 <- SummerCloudy %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SummerCloudy2200 <- SummerCloudy %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split autumn cloudy datasets in time classes

# Select 00:00:00 - 02:00:00
AutumnCloudy0002 <- AutumnCloudy %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
AutumnCloudy0204 <- AutumnCloudy %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
AutumnCloudy0406 <- AutumnCloudy %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
AutumnCloudy0608 <- AutumnCloudy %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
AutumnCloudy0810 <- AutumnCloudy %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
AutumnCloudy1012 <- AutumnCloudy %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
AutumnCloudy1214 <- AutumnCloudy %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
AutumnCloudy1416 <- AutumnCloudy %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
AutumnCloudy1618 <- AutumnCloudy %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
AutumnCloudy1820 <- AutumnCloudy %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
AutumnCloudy2022 <- AutumnCloudy %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
AutumnCloudy2200 <- AutumnCloudy %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split winter cloudy datasets in time classes

# Select 00:00:00 - 02:00:00
WinterCloudy0002 <- WinterCloudy %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
WinterCloudy0204 <- WinterCloudy %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
WinterCloudy0406 <- WinterCloudy %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
WinterCloudy0608 <- WinterCloudy %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
WinterCloudy0810 <- WinterCloudy %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
WinterCloudy1012 <- WinterCloudy %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
WinterCloudy1214 <- WinterCloudy %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
WinterCloudy1416 <- WinterCloudy %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
WinterCloudy1618 <- WinterCloudy %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
WinterCloudy1820 <- WinterCloudy %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
WinterCloudy2022 <- WinterCloudy %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
WinterCloudy2200 <- WinterCloudy %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split spring sunny datasets in time classes

# Select 00:00:00 - 02:00:00
SpringSunny0002 <- SpringSunny %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SpringSunny0204 <- SpringSunny %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SpringSunny0406 <- SpringSunny %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SpringSunny0608 <- SpringSunny %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SpringSunny0810 <- SpringSunny %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SpringSunny1012 <- SpringSunny %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SpringSunny1214 <- SpringSunny %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SpringSunny1416 <- SpringSunny %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SpringSunny1618 <- SpringSunny %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SpringSunny1820 <- SpringSunny %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SpringSunny2022 <- SpringSunny %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SpringSunny2200 <- SpringSunny %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split summer sunny datasets in time classes

# Select 00:00:00 - 02:00:00
SummerSunny0002 <- SummerSunny %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SummerSunny0204 <- SummerSunny %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SummerSunny0406 <- SummerSunny %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SummerSunny0608 <- SummerSunny %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SummerSunny0810 <- SummerSunny %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SummerSunny1012 <- SummerSunny %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SummerSunny1214 <- SummerSunny %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SummerSunny1416 <- SummerSunny %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SummerSunny1618 <- SummerSunny %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SummerSunny1820 <- SummerSunny %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SummerSunny2022 <- SummerSunny %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SummerSunny2200 <- SummerSunny %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split autumn sunny datasets in time classes

# Select 00:00:00 - 02:00:00
AutumnSunny0002 <- AutumnSunny %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
AutumnSunny0204 <- AutumnSunny %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
AutumnSunny0406 <- AutumnSunny %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
AutumnSunny0608 <- AutumnSunny %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
AutumnSunny0810 <- AutumnSunny %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
AutumnSunny1012 <- AutumnSunny %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
AutumnSunny1214 <- AutumnSunny %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
AutumnSunny1416 <- AutumnSunny %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
AutumnSunny1618 <- AutumnSunny %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
AutumnSunny1820 <- AutumnSunny %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
AutumnSunny2022 <- AutumnSunny %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
AutumnSunny2200 <- AutumnSunny %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split winter sunny datasets in time classes

# Select 00:00:00 - 02:00:00
WinterSunny0002 <- WinterSunny %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
WinterSunny0204 <- WinterSunny %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
WinterSunny0406 <- WinterSunny %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
WinterSunny0608 <- WinterSunny %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
WinterSunny0810 <- WinterSunny %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
WinterSunny1012 <- WinterSunny %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
WinterSunny1214 <- WinterSunny %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
WinterSunny1416 <- WinterSunny %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
WinterSunny1618 <- WinterSunny %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
WinterSunny1820 <- WinterSunny %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
WinterSunny2022 <- WinterSunny %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
WinterSunny2200 <- WinterSunny %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Create lists of timeframes per season (cloudy)

# Spring cloudy
SpringCloudyTimeframeList <- list(SpringCloudy0002, SpringCloudy0204, SpringCloudy0406, SpringCloudy0608,
                                SpringCloudy0810, SpringCloudy1012, SpringCloudy1214, SpringCloudy1416,
                                SpringCloudy1618, SpringCloudy1820, SpringCloudy2022, SpringCloudy2200)

# Summer cloudy
SummerCloudyTimeframeList <- list(SummerCloudy0002, SummerCloudy0204, SummerCloudy0406, SummerCloudy0608,
                                SummerCloudy0810, SummerCloudy1012, SummerCloudy1214, SummerCloudy1416,
                                SummerCloudy1618, SummerCloudy1820, SummerCloudy2022, SummerCloudy2200)

# Autumn cloudy
AutumnCloudyTimeframeList <- list(AutumnCloudy0002, AutumnCloudy0204, AutumnCloudy0406, AutumnCloudy0608,
                                AutumnCloudy0810, AutumnCloudy1012, AutumnCloudy1214, AutumnCloudy1416,
                                AutumnCloudy1618, AutumnCloudy1820, AutumnCloudy2022, AutumnCloudy2200)

# Winter cloudy
WinterCloudyTimeframeList <- list(WinterCloudy0002, WinterCloudy0204, WinterCloudy0406, WinterCloudy0608,
                                WinterCloudy0810, WinterCloudy1012, WinterCloudy1214, WinterCloudy1416,
                                WinterCloudy1618, WinterCloudy1820, WinterCloudy2022, WinterCloudy2200)


## Create lists of timeframes per season (Sunny)

# Spring sunny
SpringSunnyTimeframeList <- list(SpringSunny0002, SpringSunny0204, SpringSunny0406, SpringSunny0608,
                               SpringSunny0810, SpringSunny1012, SpringSunny1214, SpringSunny1416,
                               SpringSunny1618, SpringSunny1820, SpringSunny2022, SpringSunny2200)

# Summer sunny
SummerSunnyTimeframeList <- list(SummerSunny0002, SummerSunny0204, SummerSunny0406, SummerSunny0608,
                               SummerSunny0810, SummerSunny1012, SummerSunny1214, SummerSunny1416,
                               SummerSunny1618, SummerSunny1820, SummerSunny2022, SummerSunny2200)

# Autumn sunny
AutumnSunnyTimeframeList <- list(AutumnSunny0002, AutumnSunny0204, AutumnSunny0406, AutumnSunny0608,
                               AutumnSunny0810, AutumnSunny1012, AutumnSunny1214, AutumnSunny1416,
                               AutumnSunny1618, AutumnSunny1820, AutumnSunny2022, AutumnSunny2200)

# Winter sunny
WinterSunnyTimeframeList <- list(WinterSunny0002, WinterSunny0204, WinterSunny0406, WinterSunny0608,
                               WinterSunny0810, WinterSunny1012, WinterSunny1214, WinterSunny1416,
                               WinterSunny1618, WinterSunny1820, WinterSunny2022, WinterSunny2200)

# Call JIperTimeframe for each season (cloudy)
SpringCloudyTimeframeJITable <- JIperTimeframe(SpringCloudyTimeframeList)
SummerCloudyTimeframeJITable <- JIperTimeframe(SummerCloudyTimeframeList)
AutumnCloudyTimeframeJITable <- JIperTimeframe(AutumnCloudyTimeframeList)
WinterCloudyTimeframeJITable <- JIperTimeframe(WinterCloudyTimeframeList)

# Call JIperTimeframe for each season (sunny)
SpringSunnyTimeframeJITable <- JIperTimeframe(SpringSunnyTimeframeList)
SummerSunnyTimeframeJITable <- JIperTimeframe(SummerSunnyTimeframeList)
AutumnSunnyTimeframeJITable <- JIperTimeframe(AutumnSunnyTimeframeList)
WinterSunnyTimeframeJITable <- JIperTimeframe(WinterSunnyTimeframeList)


## Melt the tables for visualization purposes

# Spring
SpringCloudyTimeframeJITable$timeframe <- rownames(SpringCloudyTimeframeJITable)
SpringCloudyTimeframeJIMelt <- melt(SpringCloudyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Spring") %>% 
  mutate(sunshine_duration = "least sunny 25%")
colnames(SpringCloudyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Sunshine class")

SpringSunnyTimeframeJITable$timeframe <- rownames(SpringSunnyTimeframeJITable)
SpringSunnyTimeframeJIMelt <- melt(SpringSunnyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Spring") %>% 
  mutate(sunshine_duration = "most sunny 25%")
colnames(SpringSunnyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Sunshine class")

# Summer
SummerCloudyTimeframeJITable$timeframe <- rownames(SummerCloudyTimeframeJITable)
SummerCloudyTimeframeJIMelt <- melt(SummerCloudyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Summer") %>% 
  mutate(sunshine_duration = "least sunny 25%")
colnames(SummerCloudyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Sunshine class")

SummerSunnyTimeframeJITable$timeframe <- rownames(SummerSunnyTimeframeJITable)
SummerSunnyTimeframeJIMelt <- melt(SummerSunnyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Summer") %>% 
  mutate(sunshine_duration = "most sunny 25%")
colnames(SummerSunnyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Sunshine class")

# Autumn
AutumnCloudyTimeframeJITable$timeframe <- rownames(AutumnCloudyTimeframeJITable)
AutumnCloudyTimeframeJIMelt <- melt(AutumnCloudyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Autumn") %>% 
  mutate(sunshine_duration = "least sunny 25%")
colnames(AutumnCloudyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Sunshine class")

AutumnSunnyTimeframeJITable$timeframe <- rownames(AutumnSunnyTimeframeJITable)
AutumnSunnyTimeframeJIMelt <- melt(AutumnSunnyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Autumn")  %>% 
  mutate(sunshine_duration = "most sunny 25%")
colnames(AutumnSunnyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Sunshine class")

# Winter
WinterCloudyTimeframeJITable$timeframe <- rownames(WinterCloudyTimeframeJITable)
WinterCloudyTimeframeJIMelt <- melt(WinterCloudyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Winter") %>% 
  mutate(sunshine_duration = "least sunny 25%")
colnames(WinterCloudyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Sunshine class")

WinterSunnyTimeframeJITable$timeframe <- rownames(WinterSunnyTimeframeJITable)
WinterSunnyTimeframeJIMelt <- melt(WinterSunnyTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Winter")  %>% 
  mutate(sunshine_duration = "most sunny 25%")
colnames(WinterSunnyTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Sunshine class")

# Create merged JI per time of day table
TimeframeJISun <- rbind(SpringCloudyTimeframeJIMelt, SpringSunnyTimeframeJIMelt,
                         SummerCloudyTimeframeJIMelt, SummerSunnyTimeframeJIMelt,
                         AutumnCloudyTimeframeJIMelt, AutumnSunnyTimeframeJIMelt,
                         WinterCloudyTimeframeJIMelt, WinterSunnyTimeframeJIMelt)

# Grassland Spring
GrasslandSunSpringTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "grassland" &
                                                                           TimeframeJISun$season == "Spring"),],
                                            aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
GrasslandSunSpringTimeframeJIVis

# Grassland Summer
GrasslandSunSummerTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "grassland" &
                                                                           TimeframeJISun$season == "Summer"),],
                                            aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
GrasslandSunSummerTimeframeJIVis

# Grassland Autumn
GrasslandSunAutumnTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "grassland" &
                                                                           TimeframeJISun$season == "Autumn"),],
                                            aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
GrasslandSunAutumnTimeframeJIVis

# Grassland Winter
GrasslandSunWinterTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "grassland" &
                                                                           TimeframeJISun$season == "Winter"),],
                                            aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
GrasslandSunWinterTimeframeJIVis


# Deciduous forest Spring
DecForestSunSpringTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "deciduous forest" &
                                                                           TimeframeJISun$season == "Spring"),],
                                            aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
DecForestSunSpringTimeframeJIVis

# Deciduous forest Summer
DecForestSunSummerTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "deciduous forest" &
                                                                           TimeframeJISun$season == "Summer"),],
                                            aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
DecForestSunSummerTimeframeJIVis

# Deciduous forest Autumn
DecForestSunAutumnTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "deciduous forest" &
                                                                           TimeframeJISun$season == "Autumn"),],
                                            aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
DecForestSunAutumnTimeframeJIVis

# Deciduous forest Winter
DecForestSunWinterTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "deciduous forest" &
                                                                           TimeframeJISun$season == "Winter"),],
                                            aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
DecForestSunWinterTimeframeJIVis

# Coniferous forest Spring
ConForestSunSpringTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "coniferous forest" &
                                                                           TimeframeJISun$season == "Spring"),],
                                            aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
ConForestSunSpringTimeframeJIVis

# Coniferous forest Summer
ConForestSunSummerTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "coniferous forest" &
                                                                           TimeframeJISun$season == "Summer"),],
                                            aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
ConForestSunSummerTimeframeJIVis

# Coniferous forest Autumn
ConForestSunAutumnTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "coniferous forest" &
                                                                           TimeframeJISun$season == "Autumn"),],
                                            aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
ConForestSunAutumnTimeframeJIVis

# Coniferous forest Winter
ConForestSunWinterTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "coniferous forest" &
                                                                           TimeframeJISun$season == "Winter"),],
                                            aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
ConForestSunWinterTimeframeJIVis

# Fresh water Spring
FreshWaterSunSpringTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "fresh water" &
                                                                            TimeframeJISun$season == "Spring"),],
                                             aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
FreshWaterSunSpringTimeframeJIVis

# Fresh water Summer
FreshWaterSunSummerTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "fresh water" &
                                                                            TimeframeJISun$season == "Summer"),],
                                             aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
FreshWaterSunSummerTimeframeJIVis

# Fresh water Autumn
FreshWaterSunAutumnTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "fresh water" &
                                                                            TimeframeJISun$season == "Autumn"),],
                                             aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
FreshWaterSunAutumnTimeframeJIVis

# Fresh water Winter
FreshWaterSunWinterTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "fresh water" &
                                                                            TimeframeJISun$season == "Winter"),],
                                             aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
FreshWaterSunWinterTimeframeJIVis

# Bare soil Spring
BareSoilSunSpringTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "bare soil" &
                                                                          TimeframeJISun$season == "Spring"),],
                                           aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
BareSoilSunSpringTimeframeJIVis

# Bare soil Summer
BareSoilSunSummerTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "bare soil" &
                                                                          TimeframeJISun$season == "Summer"),],
                                           aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
BareSoilSunSummerTimeframeJIVis

# Bare soil Autumn
BareSoilSunAutumnTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "bare soil" &
                                                                          TimeframeJISun$season == "Autumn"),],
                                           aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
BareSoilSunAutumnTimeframeJIVis

# Bare soil Winter
BareSoilSunWinterTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "bare soil" &
                                                                          TimeframeJISun$season == "Winter"),],
                                           aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
BareSoilSunWinterTimeframeJIVis

# Heathland Spring
HeathSunSpringTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "heathland" &
                                                                       TimeframeJISun$season == "Spring"),],
                                        aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
HeathSunSpringTimeframeJIVis

# Heathland Summer
HeathSunSummerTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "heathland" &
                                                                       TimeframeJISun$season == "Summer"),],
                                        aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
HeathSunSummerTimeframeJIVis

# Heathland Autumn
HeathSunAutumnTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "heathland" &
                                                                       TimeframeJISun$season == "Autumn"),],
                                        aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
HeathSunAutumnTimeframeJIVis

# Heathland Winter
HeathSunWinterTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "heathland" &
                                                                       TimeframeJISun$season == "Winter"),],
                                        aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
HeathSunWinterTimeframeJIVis

# Shrubland Spring
ShrubSunSpringTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "shrubland" &
                                                                       TimeframeJISun$season == "Spring"),],
                                        aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
ShrubSunSpringTimeframeJIVis

# Shrubland Summer
ShrubSunSummerTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "shrubland" &
                                                                       TimeframeJISun$season == "Summer"),],
                                        aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
ShrubSunSummerTimeframeJIVis

# Shrubland Autumn
ShrubSunAutumnTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "shrubland" &
                                                                       TimeframeJISun$season == "Autumn"),],
                                        aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
ShrubSunAutumnTimeframeJIVis

# Shrubland Winter
ShrubSunWinterTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "shrubland" &
                                                                       TimeframeJISun$season == "Winter"),],
                                        aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
ShrubSunWinterTimeframeJIVis

# Swamp Spring
SwampSunSpringTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "swamp" &
                                                                       TimeframeJISun$season == "Spring"),],
                                        aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
SwampSunSpringTimeframeJIVis

# Swamp Summer
SwampSunSummerTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "swamp" &
                                                                       TimeframeJISun$season == "Summer"),],
                                        aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
SwampSunSummerTimeframeJIVis

# Swamp Autumn
SwampSunAutumnTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "swamp" &
                                                                       TimeframeJISun$season == "Autumn"),],
                                        aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
SwampSunAutumnTimeframeJIVis

# Swamp Winter
SwampSunWinterTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "swamp" &
                                                                       TimeframeJISun$season == "Winter"),],
                                        aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
SwampSunWinterTimeframeJIVis

# Grassy heathland Spring
GrassHeathSunSpringTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "grassy heathland" &
                                                                            TimeframeJISun$season == "Spring"),],
                                             aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
GrassHeathSunSpringTimeframeJIVis

# Grassy heathland Summer
GrassHeathSunSummerTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "grassy heathland" &
                                                                            TimeframeJISun$season == "Summer"),],
                                             aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
GrassHeathSunSummerTimeframeJIVis

# Grassy heathland Autumn
GrassHeathSunAutumnTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "grassy heathland" &
                                                                            TimeframeJISun$season == "Autumn"),],
                                             aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
GrassHeathSunAutumnTimeframeJIVis

# Grassy heathland Winter
GrassHeathSunWinterTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "grassy heathland" &
                                                                            TimeframeJISun$season == "Winter"),],
                                             aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
GrassHeathSunWinterTimeframeJIVis


# Road Spring
RoadSunSpringTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "road" &
                                                                      TimeframeJISun$season == "Spring"),],
                                       aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
RoadSunSpringTimeframeJIVis

# Road Summer
RoadSunSummerTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "road" &
                                                                      TimeframeJISun$season == "Summer"),],
                                       aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
RoadSunSummerTimeframeJIVis

# Road Autumn
RoadSunAutumnTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "road" &
                                                                      TimeframeJISun$season == "Autumn"),],
                                       aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
RoadSunAutumnTimeframeJIVis

# Road Winter
RoadSunWinterTimeframeJIVis <- ggplot(data = TimeframeJISun[which(TimeframeJISun$class == "road" &
                                                                      TimeframeJISun$season == "Winter"),],
                                       aes(x = timeframe, y = JI, group = `Sunshine class`, colour = `Sunshine class`)) +
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
RoadSunWinterTimeframeJIVis
