# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to determine the influence of CCI on the Jacob's index
### of a landuse class per season.

## Comparing JI for perceived hot and perceived cold days per season

# Select spring
SpringLowCCI <- AllTrackPoints %>% 
  filter(season == "spring",
         CCI <= as.numeric(quantile(Spring$CCI, 0.20)))

SpringHighCCI <- AllTrackPoints %>% 
  filter(season == "spring",
         CCI >= as.numeric(quantile(Spring$CCI, 0.80)))

# Select summer
SummerLowCCI <- AllTrackPoints %>% 
  filter(season == "summer",
         CCI <= as.numeric(quantile(Summer$CCI, 0.20)))

SummerHighCCI <- AllTrackPoints %>% 
  filter(season == "summer",
         CCI >= as.numeric(quantile(Summer$CCI, 0.80)))

# Select autumn
AutumnLowCCI <- AllTrackPoints %>% 
  filter(season == "autumm",
         CCI <= as.numeric(quantile(Autumn$CCI, 0.20)))

AutumnHighCCI <- AllTrackPoints %>% 
  filter(season == "autumm",
         CCI >= as.numeric(quantile(Autumn$CCI, 0.80)))

# Select winter
WinterLowCCI <- AllTrackPoints %>% 
  filter(season == "winter",
         CCI <= as.numeric(quantile(Winter$CCI, 0.20)))

WinterHighCCI <- AllTrackPoints %>% 
  filter(season == "winter",
         CCI >= as.numeric(quantile(Winter$CCI, 0.80)))


## Split spring low CCI datasets in time classes

# Select 00:00:00 - 02:00:00
SpringLowCCI0002 <- SpringLowCCI %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SpringLowCCI0204 <- SpringLowCCI %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SpringLowCCI0406 <- SpringLowCCI %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SpringLowCCI0608 <- SpringLowCCI %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SpringLowCCI0810 <- SpringLowCCI %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SpringLowCCI1012 <- SpringLowCCI %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SpringLowCCI1214 <- SpringLowCCI %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SpringLowCCI1416 <- SpringLowCCI %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SpringLowCCI1618 <- SpringLowCCI %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SpringLowCCI1820 <- SpringLowCCI %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SpringLowCCI2022 <- SpringLowCCI %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SpringLowCCI2200 <- SpringLowCCI %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split summer low CCI datasets in time classes

# Select 00:00:00 - 02:00:00
SummerLowCCI0002 <- SummerLowCCI %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SummerLowCCI0204 <- SummerLowCCI %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SummerLowCCI0406 <- SummerLowCCI %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SummerLowCCI0608 <- SummerLowCCI %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SummerLowCCI0810 <- SummerLowCCI %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SummerLowCCI1012 <- SummerLowCCI %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SummerLowCCI1214 <- SummerLowCCI %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SummerLowCCI1416 <- SummerLowCCI %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SummerLowCCI1618 <- SummerLowCCI %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SummerLowCCI1820 <- SummerLowCCI %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SummerLowCCI2022 <- SummerLowCCI %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SummerLowCCI2200 <- SummerLowCCI %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split autumn low CCI datasets in time classes

# Select 00:00:00 - 02:00:00
AutumnLowCCI0002 <- AutumnLowCCI %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
AutumnLowCCI0204 <- AutumnLowCCI %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
AutumnLowCCI0406 <- AutumnLowCCI %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
AutumnLowCCI0608 <- AutumnLowCCI %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
AutumnLowCCI0810 <- AutumnLowCCI %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
AutumnLowCCI1012 <- AutumnLowCCI %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
AutumnLowCCI1214 <- AutumnLowCCI %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
AutumnLowCCI1416 <- AutumnLowCCI %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
AutumnLowCCI1618 <- AutumnLowCCI %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
AutumnLowCCI1820 <- AutumnLowCCI %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
AutumnLowCCI2022 <- AutumnLowCCI %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
AutumnLowCCI2200 <- AutumnLowCCI %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split winter low CCI datasets in time classes

# Select 00:00:00 - 02:00:00
WinterLowCCI0002 <- WinterLowCCI %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
WinterLowCCI0204 <- WinterLowCCI %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
WinterLowCCI0406 <- WinterLowCCI %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
WinterLowCCI0608 <- WinterLowCCI %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
WinterLowCCI0810 <- WinterLowCCI %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
WinterLowCCI1012 <- WinterLowCCI %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
WinterLowCCI1214 <- WinterLowCCI %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
WinterLowCCI1416 <- WinterLowCCI %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
WinterLowCCI1618 <- WinterLowCCI %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
WinterLowCCI1820 <- WinterLowCCI %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
WinterLowCCI2022 <- WinterLowCCI %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
WinterLowCCI2200 <- WinterLowCCI %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split spring high CCI datasets in time classes

# Select 00:00:00 - 02:00:00
SpringHighCCI0002 <- SpringHighCCI %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SpringHighCCI0204 <- SpringHighCCI %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SpringHighCCI0406 <- SpringHighCCI %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SpringHighCCI0608 <- SpringHighCCI %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SpringHighCCI0810 <- SpringHighCCI %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SpringHighCCI1012 <- SpringHighCCI %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SpringHighCCI1214 <- SpringHighCCI %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SpringHighCCI1416 <- SpringHighCCI %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SpringHighCCI1618 <- SpringHighCCI %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SpringHighCCI1820 <- SpringHighCCI %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SpringHighCCI2022 <- SpringHighCCI %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SpringHighCCI2200 <- SpringHighCCI %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split summer high CCI datasets in time classes

# Select 00:00:00 - 02:00:00
SummerHighCCI0002 <- SummerHighCCI %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
SummerHighCCI0204 <- SummerHighCCI %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
SummerHighCCI0406 <- SummerHighCCI %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
SummerHighCCI0608 <- SummerHighCCI %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
SummerHighCCI0810 <- SummerHighCCI %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
SummerHighCCI1012 <- SummerHighCCI %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
SummerHighCCI1214 <- SummerHighCCI %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
SummerHighCCI1416 <- SummerHighCCI %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
SummerHighCCI1618 <- SummerHighCCI %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
SummerHighCCI1820 <- SummerHighCCI %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
SummerHighCCI2022 <- SummerHighCCI %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
SummerHighCCI2200 <- SummerHighCCI %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split autumn high CCI datasets in time classes

# Select 00:00:00 - 02:00:00
AutumnHighCCI0002 <- AutumnHighCCI %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
AutumnHighCCI0204 <- AutumnHighCCI %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
AutumnHighCCI0406 <- AutumnHighCCI %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
AutumnHighCCI0608 <- AutumnHighCCI %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
AutumnHighCCI0810 <- AutumnHighCCI %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
AutumnHighCCI1012 <- AutumnHighCCI %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
AutumnHighCCI1214 <- AutumnHighCCI %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
AutumnHighCCI1416 <- AutumnHighCCI %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
AutumnHighCCI1618 <- AutumnHighCCI %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
AutumnHighCCI1820 <- AutumnHighCCI %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
AutumnHighCCI2022 <- AutumnHighCCI %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
AutumnHighCCI2200 <- AutumnHighCCI %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Split winter high CCI datasets in time classes

# Select 00:00:00 - 02:00:00
WinterHighCCI0002 <- WinterHighCCI %>% 
  filter(hms >= hms("00:00:00"), hms < hms("02:00:00"))

# Select 02:00:00 - 04:00:00
WinterHighCCI0204 <- WinterHighCCI %>% 
  filter(hms >= hms("02:00:00"), hms < hms("04:00:00"))

# Select 04:00:00 - 06:00:00
WinterHighCCI0406 <- WinterHighCCI %>% 
  filter(hms >= hms("04:00:00"), hms < hms("06:00:00"))

# Select 06:00:00 - 08:00:00
WinterHighCCI0608 <- WinterHighCCI %>% 
  filter(hms >= hms("06:00:00"), hms < hms("08:00:00"))

# Select 08:00:00 - 10:00:00
WinterHighCCI0810 <- WinterHighCCI %>% 
  filter(hms >= hms("08:00:00"), hms < hms("10:00:00"))

# Select 10:00:00 - 12:00:00
WinterHighCCI1012 <- WinterHighCCI %>% 
  filter(hms >= hms("10:00:00"), hms < hms("12:00:00"))

# Select 12:00:00 - 14:00:00
WinterHighCCI1214 <- WinterHighCCI %>% 
  filter(hms >= hms("12:00:00"), hms < hms("14:00:00"))

# Select 14:00:00 - 16:00:00
WinterHighCCI1416 <- WinterHighCCI %>% 
  filter(hms >= hms("14:00:00"), hms < hms("16:00:00"))

# Select 16:00:00 - 18:00:00
WinterHighCCI1618 <- WinterHighCCI %>% 
  filter(hms >= hms("16:00:00"), hms < hms("18:00:00"))

# Select 18:00:00 - 20:00:00
WinterHighCCI1820 <- WinterHighCCI %>% 
  filter(hms >= hms("18:00:00"), hms < hms("20:00:00"))

# Select 20:00:00 - 22:00:00
WinterHighCCI2022 <- WinterHighCCI %>% 
  filter(hms >= hms("20:00:00"), hms < hms("22:00:00"))

# Select 22:00:00 - 00:00:00
WinterHighCCI2200 <- WinterHighCCI %>% 
  filter(hms >= hms("22:00:00"), hms <= hms("23:59:59"))


## Create lists of timeframes per season (low CCI)

# Spring low CCI
SpringLowCCITimeframeList <- list(SpringLowCCI0002, SpringLowCCI0204, SpringLowCCI0406, SpringLowCCI0608,
                                  SpringLowCCI0810, SpringLowCCI1012, SpringLowCCI1214, SpringLowCCI1416,
                                  SpringLowCCI1618, SpringLowCCI1820, SpringLowCCI2022, SpringLowCCI2200)

# Summer low CCI
SummerLowCCITimeframeList <- list(SummerLowCCI0002, SummerLowCCI0204, SummerLowCCI0406, SummerLowCCI0608,
                                  SummerLowCCI0810, SummerLowCCI1012, SummerLowCCI1214, SummerLowCCI1416,
                                  SummerLowCCI1618, SummerLowCCI1820, SummerLowCCI2022, SummerLowCCI2200)

# Autumn low CCI
AutumnLowCCITimeframeList <- list(AutumnLowCCI0002, AutumnLowCCI0204, AutumnLowCCI0406, AutumnLowCCI0608,
                                  AutumnLowCCI0810, AutumnLowCCI1012, AutumnLowCCI1214, AutumnLowCCI1416,
                                  AutumnLowCCI1618, AutumnLowCCI1820, AutumnLowCCI2022, AutumnLowCCI2200)

# Winter low CCI
WinterLowCCITimeframeList <- list(WinterLowCCI0002, WinterLowCCI0204, WinterLowCCI0406, WinterLowCCI0608,
                                  WinterLowCCI0810, WinterLowCCI1012, WinterLowCCI1214, WinterLowCCI1416,
                                  WinterLowCCI1618, WinterLowCCI1820, WinterLowCCI2022, WinterLowCCI2200)


## Create lists of timeframes per season (high CCI)

# Spring high CCI
SpringHighCCITimeframeList <- list(SpringHighCCI0002, SpringHighCCI0204, SpringHighCCI0406, SpringHighCCI0608,
                                 SpringHighCCI0810, SpringHighCCI1012, SpringHighCCI1214, SpringHighCCI1416,
                                 SpringHighCCI1618, SpringHighCCI1820, SpringHighCCI2022, SpringHighCCI2200)

# Summer high CCI
SummerHighCCITimeframeList <- list(SummerHighCCI0002, SummerHighCCI0204, SummerHighCCI0406, SummerHighCCI0608,
                                 SummerHighCCI0810, SummerHighCCI1012, SummerHighCCI1214, SummerHighCCI1416,
                                 SummerHighCCI1618, SummerHighCCI1820, SummerHighCCI2022, SummerHighCCI2200)

# Autumn high CCI
AutumnHighCCITimeframeList <- list(AutumnHighCCI0002, AutumnHighCCI0204, AutumnHighCCI0406, AutumnHighCCI0608,
                                 AutumnHighCCI0810, AutumnHighCCI1012, AutumnHighCCI1214, AutumnHighCCI1416,
                                 AutumnHighCCI1618, AutumnHighCCI1820, AutumnHighCCI2022, AutumnHighCCI2200)

# Winter high CCI
WinterHighCCITimeframeList <- list(WinterHighCCI0002, WinterHighCCI0204, WinterHighCCI0406, WinterHighCCI0608,
                                 WinterHighCCI0810, WinterHighCCI1012, WinterHighCCI1214, WinterHighCCI1416,
                                 WinterHighCCI1618, WinterHighCCI1820, WinterHighCCI2022, WinterHighCCI2200)

# Call JIperTimeframe for each season (low CCI)
SpringLowCCITimeframeJITable <- JIperTimeframe(SpringLowCCITimeframeList)
SummerLowCCITimeframeJITable <- JIperTimeframe(SummerLowCCITimeframeList)
AutumnLowCCITimeframeJITable <- JIperTimeframe(AutumnLowCCITimeframeList)
WinterLowCCITimeframeJITable <- JIperTimeframe(WinterLowCCITimeframeList)

# Call JIperTimeframe for each season (high CCI)
SpringHighCCITimeframeJITable <- JIperTimeframe(SpringHighCCITimeframeList)
SummerHighCCITimeframeJITable <- JIperTimeframe(SummerHighCCITimeframeList)
AutumnHighCCITimeframeJITable <- JIperTimeframe(AutumnHighCCITimeframeList)
WinterHighCCITimeframeJITable <- JIperTimeframe(WinterHighCCITimeframeList)


## Melt the tables for visualization purposes

# Spring
SpringLowCCITimeframeJITable$timeframe <- rownames(SpringLowCCITimeframeJITable)
SpringLowCCITimeframeJIMelt <- melt(SpringLowCCITimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Spring") %>% 
  mutate(CCI = "lowest CCI 20%")
colnames(SpringLowCCITimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "CCI class")

SpringHighCCITimeframeJITable$timeframe <- rownames(SpringHighCCITimeframeJITable)
SpringHighCCITimeframeJIMelt <- melt(SpringHighCCITimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Spring") %>% 
  mutate(CCI = "highest CCI 20%")
colnames(SpringHighCCITimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "CCI class")

# Summer
SummerLowCCITimeframeJITable$timeframe <- rownames(SummerLowCCITimeframeJITable)
SummerLowCCITimeframeJIMelt <- melt(SummerLowCCITimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Summer") %>% 
  mutate(CCI = "lowest CCI 20%")
colnames(SummerLowCCITimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "CCI class")

SummerHighCCITimeframeJITable$timeframe <- rownames(SummerHighCCITimeframeJITable)
SummerHighCCITimeframeJIMelt <- melt(SummerHighCCITimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Summer") %>% 
  mutate(CCI = "highest CCI 20%")
colnames(SummerHighCCITimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "CCI class")

# Autumn
AutumnLowCCITimeframeJITable$timeframe <- rownames(AutumnLowCCITimeframeJITable)
AutumnLowCCITimeframeJIMelt <- melt(AutumnLowCCITimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Autumn") %>% 
  mutate(CCI = "lowest CCI 20%")
colnames(AutumnLowCCITimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "CCI class")

AutumnHighCCITimeframeJITable$timeframe <- rownames(AutumnHighCCITimeframeJITable)
AutumnHighCCITimeframeJIMelt <- melt(AutumnHighCCITimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Autumn")  %>% 
  mutate(CCI = "highest CCI 20%")
colnames(AutumnHighCCITimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "CCI class")

# Winter
WinterLowCCITimeframeJITable$timeframe <- rownames(WinterLowCCITimeframeJITable)
WinterLowCCITimeframeJIMelt <- melt(WinterLowCCITimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Winter") %>% 
  mutate(CCI = "lowest CCI 20%")
colnames(WinterLowCCITimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "CCI class")

WinterHighCCITimeframeJITable$timeframe <- rownames(WinterHighCCITimeframeJITable)
WinterHighCCITimeframeJIMelt <- melt(WinterHighCCITimeframeJITable, id = "timeframe") %>% 
  mutate(season = "Winter")  %>% 
  mutate(CCI = "highest CCI 20%")
colnames(WinterHighCCITimeframeJIMelt) <- c("timeframe", "class", "JI", "season", "CCI class")

# Create merged JI per time of day table
TimeframeJICCI <- rbind(SpringLowCCITimeframeJIMelt, SpringHighCCITimeframeJIMelt,
                        SummerLowCCITimeframeJIMelt, SummerHighCCITimeframeJIMelt,
                        AutumnLowCCITimeframeJIMelt, AutumnHighCCITimeframeJIMelt,
                        WinterLowCCITimeframeJIMelt, WinterHighCCITimeframeJIMelt)


# Grassland Spring
GrasslandCCISpringTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "grassland" &
                                                                         TimeframeJICCI$season == "Spring"),],
                                           aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassland, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
GrasslandCCISpringTimeframeJIVis

# Grassland Summer
GrasslandCCISummerTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "grassland" &
                                                                         TimeframeJICCI$season == "Summer"),],
                                           aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassland, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
GrasslandCCISummerTimeframeJIVis

# Grassland Autumn
GrasslandCCIAutumnTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "grassland" &
                                                                         TimeframeJICCI$season == "Autumn"),],
                                           aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassland, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
GrasslandCCIAutumnTimeframeJIVis

# Grassland Winter
GrasslandCCIWinterTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "grassland" &
                                                                         TimeframeJICCI$season == "Winter"),],
                                           aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassland, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
GrasslandCCIWinterTimeframeJIVis


# Deciduous forest Spring
DecForestCCISpringTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "deciduous forest" &
                                                                         TimeframeJICCI$season == "Spring"),],
                                           aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Deciduous forest, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
DecForestCCISpringTimeframeJIVis

# Deciduous forest Summer
DecForestCCISummerTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "deciduous forest" &
                                                                         TimeframeJICCI$season == "Summer"),],
                                           aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Deciduous forest, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
DecForestCCISummerTimeframeJIVis

# Deciduous forest Autumn
DecForestCCIAutumnTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "deciduous forest" &
                                                                         TimeframeJICCI$season == "Autumn"),],
                                           aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Deciduous forest, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
DecForestCCIAutumnTimeframeJIVis

# Deciduous forest Winter
DecForestCCIWinterTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "deciduous forest" &
                                                                         TimeframeJICCI$season == "Winter"),],
                                           aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Deciduous forest, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
DecForestCCIWinterTimeframeJIVis

# Coniferous forest Spring
ConForestCCISpringTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "coniferous forest" &
                                                                         TimeframeJICCI$season == "Spring"),],
                                           aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Coniferous forest, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
ConForestCCISpringTimeframeJIVis

# Coniferous forest Summer
ConForestCCISummerTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "coniferous forest" &
                                                                         TimeframeJICCI$season == "Summer"),],
                                           aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Coniferous forest, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
ConForestCCISummerTimeframeJIVis

# Coniferous forest Autumn
ConForestCCIAutumnTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "coniferous forest" &
                                                                         TimeframeJICCI$season == "Autumn"),],
                                           aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Coniferous forest, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
ConForestCCIAutumnTimeframeJIVis

# Coniferous forest Winter
ConForestCCIWinterTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "coniferous forest" &
                                                                         TimeframeJICCI$season == "Winter"),],
                                           aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Coniferous forest, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
ConForestCCIWinterTimeframeJIVis

# Fresh water Spring
FreshWaterCCISpringTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "fresh water" &
                                                                          TimeframeJICCI$season == "Spring"),],
                                            aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Fresh water, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
FreshWaterCCISpringTimeframeJIVis

# Fresh water Summer
FreshWaterCCISummerTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "fresh water" &
                                                                          TimeframeJICCI$season == "Summer"),],
                                            aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Fresh water, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
FreshWaterCCISummerTimeframeJIVis

# Fresh water Autumn
FreshWaterCCIAutumnTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "fresh water" &
                                                                          TimeframeJICCI$season == "Autumn"),],
                                            aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Fresh water, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
FreshWaterCCIAutumnTimeframeJIVis

# Fresh water Winter
FreshWaterCCIWinterTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "fresh water" &
                                                                          TimeframeJICCI$season == "Winter"),],
                                            aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Fresh water, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
FreshWaterCCIWinterTimeframeJIVis

# Bare soil Spring
BareSoilCCISpringTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "bare soil" &
                                                                        TimeframeJICCI$season == "Spring"),],
                                          aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Bare soil, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
BareSoilCCISpringTimeframeJIVis

# Bare soil Summer
BareSoilCCISummerTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "bare soil" &
                                                                        TimeframeJICCI$season == "Summer"),],
                                          aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Bare soil, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
BareSoilCCISummerTimeframeJIVis

# Bare soil Autumn
BareSoilCCIAutumnTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "bare soil" &
                                                                        TimeframeJICCI$season == "Autumn"),],
                                          aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Bare soil, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
BareSoilCCIAutumnTimeframeJIVis

# Bare soil Winter
BareSoilCCIWinterTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "bare soil" &
                                                                        TimeframeJICCI$season == "Winter"),],
                                          aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Bare soil, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
BareSoilCCIWinterTimeframeJIVis

# Heathland Spring
HeathCCISpringTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "heathland" &
                                                                     TimeframeJICCI$season == "Spring"),],
                                       aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Heathland, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
HeathCCISpringTimeframeJIVis

# Heathland Summer
HeathCCISummerTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "heathland" &
                                                                     TimeframeJICCI$season == "Summer"),],
                                       aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Heathland, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
HeathCCISummerTimeframeJIVis

# Heathland Autumn
HeathCCIAutumnTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "heathland" &
                                                                     TimeframeJICCI$season == "Autumn"),],
                                       aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Heathland, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
HeathCCIAutumnTimeframeJIVis

# Heathland Winter
HeathCCIWinterTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "heathland" &
                                                                     TimeframeJICCI$season == "Winter"),],
                                       aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Heathland, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
HeathCCIWinterTimeframeJIVis

# Shrubland Spring
ShrubCCISpringTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "shrubland" &
                                                                     TimeframeJICCI$season == "Spring"),],
                                       aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Shrubland, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
ShrubCCISpringTimeframeJIVis

# Shrubland Summer
ShrubCCISummerTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "shrubland" &
                                                                     TimeframeJICCI$season == "Summer"),],
                                       aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Shrubland, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
ShrubCCISummerTimeframeJIVis

# Shrubland Autumn
ShrubCCIAutumnTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "shrubland" &
                                                                     TimeframeJICCI$season == "Autumn"),],
                                       aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Shrubland, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
ShrubCCIAutumnTimeframeJIVis

# Shrubland Winter
ShrubCCIWinterTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "shrubland" &
                                                                     TimeframeJICCI$season == "Winter"),],
                                       aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Shrubland, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
ShrubCCIWinterTimeframeJIVis

# Swamp Spring
SwampCCISpringTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "swamp" &
                                                                     TimeframeJICCI$season == "Spring"),],
                                       aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Swamp, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
SwampCCISpringTimeframeJIVis

# Swamp Summer
SwampCCISummerTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "swamp" &
                                                                     TimeframeJICCI$season == "Summer"),],
                                       aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Swamp, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
SwampCCISummerTimeframeJIVis

# Swamp Autumn
SwampCCIAutumnTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "swamp" &
                                                                     TimeframeJICCI$season == "Autumn"),],
                                       aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Swamp, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
SwampCCIAutumnTimeframeJIVis

# Swamp Winter
SwampCCIWinterTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "swamp" &
                                                                     TimeframeJICCI$season == "Winter"),],
                                       aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Swamp, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
SwampCCIWinterTimeframeJIVis

# Grassy heathland Spring
GrassHeathCCISpringTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "grassy heathland" &
                                                                          TimeframeJICCI$season == "Spring"),],
                                            aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassy heathland, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
GrassHeathCCISpringTimeframeJIVis

# Grassy heathland Summer
GrassHeathCCISummerTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "grassy heathland" &
                                                                          TimeframeJICCI$season == "Summer"),],
                                            aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassy heathland, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
GrassHeathCCISummerTimeframeJIVis

# Grassy heathland Autumn
GrassHeathCCIAutumnTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "grassy heathland" &
                                                                          TimeframeJICCI$season == "Autumn"),],
                                            aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassy heathland, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
GrassHeathCCIAutumnTimeframeJIVis

# Grassy heathland Winter
GrassHeathCCIWinterTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "grassy heathland" &
                                                                          TimeframeJICCI$season == "Winter"),],
                                            aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Grassy heathland, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
GrassHeathCCIWinterTimeframeJIVis


# Road Spring
RoadCCISpringTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "road" &
                                                                    TimeframeJICCI$season == "Spring"),],
                                      aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Road, spring)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
RoadCCISpringTimeframeJIVis

# Road Summer
RoadCCISummerTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "road" &
                                                                    TimeframeJICCI$season == "Summer"),],
                                      aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Road, summer)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
RoadCCISummerTimeframeJIVis

# Road Autumn
RoadCCIAutumnTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "road" &
                                                                    TimeframeJICCI$season == "Autumn"),],
                                      aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Road, autumn)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
RoadCCIAutumnTimeframeJIVis

# Road Winter
RoadCCIWinterTimeframeJIVis <- ggplot(data = TimeframeJICCI[which(TimeframeJICCI$class == "road" &
                                                                    TimeframeJICCI$season == "Winter"),],
                                      aes(x = timeframe, y = JI, group = `CCI class`, colour = `CCI class`)) +
  geom_line() +
  geom_point() +
  ylim(c(-1,1)) +
  ylab("Jacobs index") +
  xlab("Timeframe of day (hours)") +
  ggtitle("Diurnal variation in Jacobs index (Road, winter)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 1, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = c("#FF0033", "#3300FF" ))
RoadCCIWinterTimeframeJIVis