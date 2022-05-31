# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to determine the influence of temperature on the Jacob's index
### of a landuse class per season.


## Comparing JI for hot and cold days per season

# Select spring
SpringCold <- AllTrackPoints %>% 
  filter(season == "spring",
        average_temperature_day <= as.numeric(quantile(Spring$average_temperature_day, 0.25)))

SpringHot <- AllTrackPoints %>% 
  filter(season == "spring",
         average_temperature_day >= as.numeric(quantile(Spring$average_temperature_day, 0.75)))

# Select summer
SummerCold <- AllTrackPoints %>% 
  filter(season == "summer",
         average_temperature_day <= as.numeric(quantile(Summer$average_temperature_day, 0.25)))

SummerHot <- AllTrackPoints %>% 
  filter(season == "summer",
         average_temperature_day >= as.numeric(quantile(Summer$average_temperature_day, 0.75)))

# Select autumn
AutumnCold <- AllTrackPoints %>% 
  filter(season == "autumm",
         average_temperature_day <= as.numeric(quantile(Autumn$average_temperature_day, 0.25)))

AutumnHot <- AllTrackPoints %>% 
  filter(season == "autumm",
         average_temperature_day >= as.numeric(quantile(Autumn$average_temperature_day, 0.75)))

# Select winter
WinterCold <- AllTrackPoints %>% 
  filter(season == "winter",
         average_temperature_day <= as.numeric(quantile(Winter$average_temperature_day, 0.25)))

WinterHot <- AllTrackPoints %>% 
  filter(season == "winter",
         average_temperature_day >= as.numeric(quantile(Winter$average_temperature_day, 0.75)))


## Split spring cold datasets in time classes

# Select 00:00:00 - 02:00:00
SpringCold0002 <- SpringCold %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SpringCold0204 <- SpringCold %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SpringCold0406 <- SpringCold %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SpringCold0608 <- SpringCold %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SpringCold0810 <- SpringCold %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SpringCold1012 <- SpringCold %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SpringCold1214 <- SpringCold %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SpringCold1416 <- SpringCold %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SpringCold1618 <- SpringCold %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SpringCold1820 <- SpringCold %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SpringCold2022 <- SpringCold %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SpringCold2200 <- SpringCold %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split summer cold datasets in time classes

# Select 00:00:00 - 02:00:00
SummerCold0002 <- SummerCold %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SummerCold0204 <- SummerCold %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SummerCold0406 <- SummerCold %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SummerCold0608 <- SummerCold %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SummerCold0810 <- SummerCold %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SummerCold1012 <- SummerCold %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SummerCold1214 <- SummerCold %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SummerCold1416 <- SummerCold %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SummerCold1618 <- SummerCold %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SummerCold1820 <- SummerCold %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SummerCold2022 <- SummerCold %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SummerCold2200 <- SummerCold %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split autumn cold datasets in time classes

# Select 00:00:00 - 02:00:00
AutumnCold0002 <- AutumnCold %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
AutumnCold0204 <- AutumnCold %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
AutumnCold0406 <- AutumnCold %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
AutumnCold0608 <- AutumnCold %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
AutumnCold0810 <- AutumnCold %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
AutumnCold1012 <- AutumnCold %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
AutumnCold1214 <- AutumnCold %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
AutumnCold1416 <- AutumnCold %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
AutumnCold1618 <- AutumnCold %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
AutumnCold1820 <- AutumnCold %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
AutumnCold2022 <- AutumnCold %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
AutumnCold2200 <- AutumnCold %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split winter cold datasets in time classes

# Select 00:00:00 - 02:00:00
WinterCold0002 <- WinterCold %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
WinterCold0204 <- WinterCold %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
WinterCold0406 <- WinterCold %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
WinterCold0608 <- WinterCold %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
WinterCold0810 <- WinterCold %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
WinterCold1012 <- WinterCold %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
WinterCold1214 <- WinterCold %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
WinterCold1416 <- WinterCold %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
WinterCold1618 <- WinterCold %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
WinterCold1820 <- WinterCold %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
WinterCold2022 <- WinterCold %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
WinterCold2200 <- WinterCold %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split spring hot datasets in time classes

# Select 00:00:00 - 02:00:00
SpringHot0002 <- SpringHot %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SpringHot0204 <- SpringHot %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SpringHot0406 <- SpringHot %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SpringHot0608 <- SpringHot %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SpringHot0810 <- SpringHot %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SpringHot1012 <- SpringHot %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SpringHot1214 <- SpringHot %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SpringHot1416 <- SpringHot %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SpringHot1618 <- SpringHot %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SpringHot1820 <- SpringHot %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SpringHot2022 <- SpringHot %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SpringHot2200 <- SpringHot %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split summer hot datasets in time classes

# Select 00:00:00 - 02:00:00
SummerHot0002 <- SummerHot %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SummerHot0204 <- SummerHot %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SummerHot0406 <- SummerHot %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SummerHot0608 <- SummerHot %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SummerHot0810 <- SummerHot %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SummerHot1012 <- SummerHot %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SummerHot1214 <- SummerHot %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SummerHot1416 <- SummerHot %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SummerHot1618 <- SummerHot %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SummerHot1820 <- SummerHot %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SummerHot2022 <- SummerHot %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SummerHot2200 <- SummerHot %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split autumn hot datasets in time classes

# Select 00:00:00 - 02:00:00
AutumnHot0002 <- AutumnHot %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
AutumnHot0204 <- AutumnHot %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
AutumnHot0406 <- AutumnHot %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
AutumnHot0608 <- AutumnHot %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
AutumnHot0810 <- AutumnHot %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
AutumnHot1012 <- AutumnHot %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
AutumnHot1214 <- AutumnHot %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
AutumnHot1416 <- AutumnHot %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
AutumnHot1618 <- AutumnHot %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
AutumnHot1820 <- AutumnHot %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
AutumnHot2022 <- AutumnHot %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
AutumnHot2200 <- AutumnHot %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split winter hot datasets in time classes

# Select 00:00:00 - 02:00:00
WinterHot0002 <- WinterHot %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
WinterHot0204 <- WinterHot %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
WinterHot0406 <- WinterHot %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
WinterHot0608 <- WinterHot %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
WinterHot0810 <- WinterHot %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
WinterHot1012 <- WinterHot %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
WinterHot1214 <- WinterHot %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
WinterHot1416 <- WinterHot %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
WinterHot1618 <- WinterHot %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
WinterHot1820 <- WinterHot %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
WinterHot2022 <- WinterHot %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
WinterHot2200 <- WinterHot %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Create lists of timeframes per season (cold)

# Spring cold
SpringColdTimeframeList <- list(SpringCold0002, SpringCold0204, SpringCold0406, SpringCold0608,
                                SpringCold0810, SpringCold1012, SpringCold1214, SpringCold1416,
                                SpringCold1618, SpringCold1820, SpringCold2022, SpringCold2200)

# Summer cold
SummerColdTimeframeList <- list(SummerCold0002, SummerCold0204, SummerCold0406, SummerCold0608,
                                SummerCold0810, SummerCold1012, SummerCold1214, SummerCold1416,
                                SummerCold1618, SummerCold1820, SummerCold2022, SummerCold2200)

# Autumn cold
AutumnColdTimeframeList <- list(AutumnCold0002, AutumnCold0204, AutumnCold0406, AutumnCold0608,
                                AutumnCold0810, AutumnCold1012, AutumnCold1214, AutumnCold1416,
                                AutumnCold1618, AutumnCold1820, AutumnCold2022, AutumnCold2200)

# Winter cold
WinterColdTimeframeList <- list(WinterCold0002, WinterCold0204, WinterCold0406, WinterCold0608,
                                WinterCold0810, WinterCold1012, WinterCold1214, WinterCold1416,
                                WinterCold1618, WinterCold1820, WinterCold2022, WinterCold2200)


## Create lists of timeframes per season (hot)

# Spring hot
SpringHotTimeframeList <- list(SpringHot0002, SpringHot0204, SpringHot0406, SpringHot0608,
                               SpringHot0810, SpringHot1012, SpringHot1214, SpringHot1416,
                               SpringHot1618, SpringHot1820, SpringHot2022, SpringHot2200)

# Summer hot
SummerHotTimeframeList <- list(SummerHot0002, SummerHot0204, SummerHot0406, SummerHot0608,
                               SummerHot0810, SummerHot1012, SummerHot1214, SummerHot1416,
                               SummerHot1618, SummerHot1820, SummerHot2022, SummerHot2200)

# Autumn hot
AutumnHotTimeframeList <- list(AutumnHot0002, AutumnHot0204, AutumnHot0406, AutumnHot0608,
                               AutumnHot0810, AutumnHot1012, AutumnHot1214, AutumnHot1416,
                               AutumnHot1618, AutumnHot1820, AutumnHot2022, AutumnHot2200)

# Winter hot
WinterHotTimeframeList <- list(WinterHot0002, WinterHot0204, WinterHot0406, WinterHot0608,
                               WinterHot0810, WinterHot1012, WinterHot1214, WinterHot1416,
                               WinterHot1618, WinterHot1820, WinterHot2022, WinterHot2200)

# Call JIperTimeframe for each season (cold)
SpringColdTimeframeJITable <- JIperTimeframe(SpringColdTimeframeList)
SummerColdTimeframeJITable <- JIperTimeframe(SummerColdTimeframeList)
AutumnColdTimeframeJITable <- JIperTimeframe(AutumnColdTimeframeList)
WinterColdTimeframeJITable <- JIperTimeframe(WinterColdTimeframeList)

# Call JIperTimeframe for each season (hot)
SpringHotTimeframeJITable <- JIperTimeframe(SpringHotTimeframeList)
SummerHotTimeframeJITable <- JIperTimeframe(SummerHotTimeframeList)
AutumnHotTimeframeJITable <- JIperTimeframe(AutumnHotTimeframeList)
WinterHotTimeframeJITable <- JIperTimeframe(WinterHotTimeframeList)

## Melt the tables for visualization purposes

# Spring
SpringColdTimeframeJITable$timeframe <- rownames(SpringColdTimeframeJITable)
SpringColdTimeframeJIMelt <- melt(SpringColdTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Spring") %>% 
  mutate(ave_day_temp = "coldest 25%")
colnames(SpringColdTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Temperature class")

SpringHotTimeframeJITable$timeframe <- rownames(SpringHotTimeframeJITable)
SpringHotTimeframeJIMelt <- melt(SpringHotTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Spring") %>% 
  mutate(ave_day_temp = "hottest 25%")
colnames(SpringHotTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Temperature class")

# Summer
SummerColdTimeframeJITable$timeframe <- rownames(SummerColdTimeframeJITable)
SummerColdTimeframeJIMelt <- melt(SummerColdTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Summer") %>% 
  mutate(ave_day_temp = "coldest 25%")
colnames(SummerColdTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Temperature class")

SummerHotTimeframeJITable$timeframe <- rownames(SummerHotTimeframeJITable)
SummerHotTimeframeJIMelt <- melt(SummerHotTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Summer") %>% 
  mutate(ave_day_temp = "hottest 25%")
colnames(SummerHotTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Temperature class")

# Autumn
AutumnColdTimeframeJITable$timeframe <- rownames(AutumnColdTimeframeJITable)
AutumnColdTimeframeJIMelt <- melt(AutumnColdTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Autumn") %>% 
  mutate(ave_day_temp = "coldest 25%")
colnames(AutumnColdTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Temperature class")

AutumnHotTimeframeJITable$timeframe <- rownames(AutumnHotTimeframeJITable)
AutumnHotTimeframeJIMelt <- melt(AutumnHotTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Autumn")  %>% 
  mutate(ave_day_temp = "hottest 25%")
colnames(AutumnHotTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Temperature class")

# Winter
WinterColdTimeframeJITable$timeframe <- rownames(WinterColdTimeframeJITable)
WinterColdTimeframeJIMelt <- melt(WinterColdTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Winter") %>% 
  mutate(ave_day_temp = "coldest 25%")
colnames(WinterColdTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Temperature class")

WinterHotTimeframeJITable$timeframe <- rownames(WinterHotTimeframeJITable)
WinterHotTimeframeJIMelt <- melt(WinterHotTimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Winter")  %>% 
  mutate(ave_day_temp = "hottest 25%")
colnames(WinterHotTimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "Temperature class")

# Create merged JI per time of day table
TimeframeJITemp <- rbind(SpringColdTimeframeJIMelt, SpringHotTimeframeJIMelt,
                         SummerColdTimeframeJIMelt, SummerHotTimeframeJIMelt,
                         AutumnColdTimeframeJIMelt, AutumnHotTimeframeJIMelt,
                         WinterColdTimeframeJIMelt, WinterHotTimeframeJIMelt)

# Grassland Spring
GrasslandTempSpringTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "grassland" &
                                                                     TimeframeJITemp$season == "Spring"),],
                                  aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
GrasslandTempSpringTimeframeJIVis

# Grassland Summer
GrasslandTempSummerTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "grassland" &
                                                                           TimeframeJITemp$season == "Summer"),],
                                            aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
GrasslandTempSummerTimeframeJIVis

# Grassland Autumn
GrasslandTempAutumnTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "grassland" &
                                                                           TimeframeJITemp$season == "Autumn"),],
                                            aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
GrasslandTempAutumnTimeframeJIVis

# Grassland Winter
GrasslandTempWinterTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "grassland" &
                                                                           TimeframeJITemp$season == "Winter"),],
                                            aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
GrasslandTempWinterTimeframeJIVis


# Deciduous forest Spring
DecForestTempSpringTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "deciduous forest" &
                                                                           TimeframeJITemp$season == "Spring"),],
                                            aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
DecForestTempSpringTimeframeJIVis

# Deciduous forest Summer
DecForestTempSummerTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "deciduous forest" &
                                                                           TimeframeJITemp$season == "Summer"),],
                                            aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
DecForestTempSummerTimeframeJIVis

# Deciduous forest Autumn
DecForestTempAutumnTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "deciduous forest" &
                                                                           TimeframeJITemp$season == "Autumn"),],
                                            aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
DecForestTempAutumnTimeframeJIVis

# Deciduous forest Winter
DecForestTempWinterTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "deciduous forest" &
                                                                           TimeframeJITemp$season == "Winter"),],
                                            aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
DecForestTempWinterTimeframeJIVis

# Coniferous forest Spring
ConForestTempSpringTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "coniferous forest" &
                                                                           TimeframeJITemp$season == "Spring"),],
                                            aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
ConForestTempSpringTimeframeJIVis

# Coniferous forest Summer
ConForestTempSummerTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "coniferous forest" &
                                                                           TimeframeJITemp$season == "Summer"),],
                                            aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
ConForestTempSummerTimeframeJIVis

# Coniferous forest Autumn
ConForestTempAutumnTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "coniferous forest" &
                                                                           TimeframeJITemp$season == "Autumn"),],
                                            aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
ConForestTempAutumnTimeframeJIVis

# Coniferous forest Winter
ConForestTempWinterTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "coniferous forest" &
                                                                           TimeframeJITemp$season == "Winter"),],
                                            aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
ConForestTempWinterTimeframeJIVis

# Fresh water Spring
FreshWaterTempSpringTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "fresh water" &
                                                                            TimeframeJITemp$season == "Spring"),],
                                             aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
FreshWaterTempSpringTimeframeJIVis

# Fresh water Summer
FreshWaterTempSummerTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "fresh water" &
                                                                            TimeframeJITemp$season == "Summer"),],
                                             aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
FreshWaterTempSummerTimeframeJIVis

# Fresh water Autumn
FreshWaterTempAutumnTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "fresh water" &
                                                                            TimeframeJITemp$season == "Autumn"),],
                                             aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
FreshWaterTempAutumnTimeframeJIVis

# Fresh water Winter
FreshWaterTempWinterTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "fresh water" &
                                                                            TimeframeJITemp$season == "Winter"),],
                                             aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
FreshWaterTempWinterTimeframeJIVis

# Bare soil Spring
BareSoilTempSpringTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "bare soil" &
                                                                          TimeframeJITemp$season == "Spring"),],
                                           aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
BareSoilTempSpringTimeframeJIVis

# Bare soil Summer
BareSoilTempSummerTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "bare soil" &
                                                                          TimeframeJITemp$season == "Summer"),],
                                           aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
BareSoilTempSummerTimeframeJIVis

# Bare soil Autumn
BareSoilTempAutumnTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "bare soil" &
                                                                          TimeframeJITemp$season == "Autumn"),],
                                           aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
BareSoilTempAutumnTimeframeJIVis

# Bare soil Winter
BareSoilTempWinterTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "bare soil" &
                                                                          TimeframeJITemp$season == "Winter"),],
                                           aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
BareSoilTempWinterTimeframeJIVis

# Heathland Spring
HeathTempSpringTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "heathland" &
                                                                       TimeframeJITemp$season == "Spring"),],
                                        aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
HeathTempSpringTimeframeJIVis

# Heathland Summer
HeathTempSummerTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "heathland" &
                                                                       TimeframeJITemp$season == "Summer"),],
                                        aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
HeathTempSummerTimeframeJIVis

# Heathland Autumn
HeathTempAutumnTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "heathland" &
                                                                       TimeframeJITemp$season == "Autumn"),],
                                        aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
HeathTempAutumnTimeframeJIVis

# Heathland Winter
HeathTempWinterTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "heathland" &
                                                                       TimeframeJITemp$season == "Winter"),],
                                        aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
HeathTempWinterTimeframeJIVis

# Shrubland Spring
ShrubTempSpringTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "shrubland" &
                                                                       TimeframeJITemp$season == "Spring"),],
                                        aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
ShrubTempSpringTimeframeJIVis

# Shrubland Summer
ShrubTempSummerTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "shrubland" &
                                                                       TimeframeJITemp$season == "Summer"),],
                                        aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
ShrubTempSummerTimeframeJIVis

# Shrubland Autumn
ShrubTempAutumnTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "shrubland" &
                                                                       TimeframeJITemp$season == "Autumn"),],
                                        aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
ShrubTempAutumnTimeframeJIVis

# Shrubland Winter
ShrubTempWinterTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "shrubland" &
                                                                       TimeframeJITemp$season == "Winter"),],
                                        aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
ShrubTempWinterTimeframeJIVis

# Swamp Spring
SwampTempSpringTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "swamp" &
                                                                       TimeframeJITemp$season == "Spring"),],
                                        aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
SwampTempSpringTimeframeJIVis

# Swamp Summer
SwampTempSummerTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "swamp" &
                                                                       TimeframeJITemp$season == "Summer"),],
                                        aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
SwampTempSummerTimeframeJIVis

# Swamp Autumn
SwampTempAutumnTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "swamp" &
                                                                       TimeframeJITemp$season == "Autumn"),],
                                        aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
SwampTempAutumnTimeframeJIVis

# Swamp Winter
SwampTempWinterTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "swamp" &
                                                                       TimeframeJITemp$season == "Winter"),],
                                        aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
SwampTempWinterTimeframeJIVis

# Grassy heathland Spring
GrassHeathTempSpringTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "grassy heathland" &
                                                                            TimeframeJITemp$season == "Spring"),],
                                             aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
GrassHeathTempSpringTimeframeJIVis

# Grassy heathland Summer
GrassHeathTempSummerTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "grassy heathland" &
                                                                            TimeframeJITemp$season == "Summer"),],
                                             aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
GrassHeathTempSummerTimeframeJIVis

# Grassy heathland Autumn
GrassHeathTempAutumnTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "grassy heathland" &
                                                                            TimeframeJITemp$season == "Autumn"),],
                                             aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
GrassHeathTempAutumnTimeframeJIVis

# Grassy heathland Winter
GrassHeathTempWinterTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "grassy heathland" &
                                                                            TimeframeJITemp$season == "Winter"),],
                                             aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
GrassHeathTempWinterTimeframeJIVis


# Road Spring
RoadTempSpringTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "road" &
                                                                      TimeframeJITemp$season == "Spring"),],
                                       aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
RoadTempSpringTimeframeJIVis

# Road Summer
RoadTempSummerTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "road" &
                                                                      TimeframeJITemp$season == "Summer"),],
                                       aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
RoadTempSummerTimeframeJIVis

# Road Autumn
RoadTempAutumnTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "road" &
                                                                      TimeframeJITemp$season == "Autumn"),],
                                       aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
RoadTempAutumnTimeframeJIVis

# Road Winter
RoadTempWinterTimeframeJIVis <- ggplot(data = TimeframeJITemp[which(TimeframeJITemp$class == "road" &
                                                                      TimeframeJITemp$season == "Winter"),],
                                       aes(x = timeframe, y = JI, group = `Temperature class`, colour = `Temperature class`)) +
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
RoadTempWinterTimeframeJIVis

