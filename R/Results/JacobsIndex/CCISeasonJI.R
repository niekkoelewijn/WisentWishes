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
         CCI <= as.numeric(quantile(Spring$CCI, 0.25)))

SpringHighCCI <- AllTrackPoints %>% 
  filter(season == "spring",
         CCI >= as.numeric(quantile(Spring$CCI, 0.75)))

# Select summer
SummerLowCCI <- AllTrackPoints %>% 
  filter(season == "summer",
         CCI <= as.numeric(quantile(Summer$CCI, 0.25)))

SummerHighCCI <- AllTrackPoints %>% 
  filter(season == "summer",
         CCI >= as.numeric(quantile(Summer$CCI, 0.75)))

# Select autumn
AutumnLowCCI <- AllTrackPoints %>% 
  filter(season == "autumm",
         CCI <= as.numeric(quantile(Autumn$CCI, 0.25)))

AutumnHighCCI <- AllTrackPoints %>% 
  filter(season == "autumm",
         CCI >= as.numeric(quantile(Autumn$CCI, 0.75)))

# Select winter
WinterLowCCI <- AllTrackPoints %>% 
  filter(season == "winter",
         CCI <= as.numeric(quantile(Winter$CCI, 0.25)))

WinterHighCCI <- AllTrackPoints %>% 
  filter(season == "winter",
         CCI >= as.numeric(quantile(Winter$CCI, 0.75)))

## Split spring LowCCI datasets in time classes

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


## Split summer LowCCI datasets in time classes

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


## Split autumn LowCCI datasets in time classes

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


## Split winter LowCCI datasets in time classes

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


## Split spring HighCCI datasets in time classes

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


## Split summer HighCCI datasets in time classes

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


## Split autumn HighCCI datasets in time classes

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


## Split winter HighCCI datasets in time classes

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


## Create lists of timeframes per season (LowCCI)

# Spring LowCCI
SpringLowCCITimeframeList <- list(SpringLowCCI0002, SpringLowCCI0204, SpringLowCCI0406, SpringLowCCI0608,
                                  SpringLowCCI0810, SpringLowCCI1012, SpringLowCCI1214, SpringLowCCI1416,
                                  SpringLowCCI1618, SpringLowCCI1820, SpringLowCCI2022, SpringLowCCI2200)

# Summer LowCCI
SummerLowCCITimeframeList <- list(SummerLowCCI0002, SummerLowCCI0204, SummerLowCCI0406, SummerLowCCI0608,
                                  SummerLowCCI0810, SummerLowCCI1012, SummerLowCCI1214, SummerLowCCI1416,
                                  SummerLowCCI1618, SummerLowCCI1820, SummerLowCCI2022, SummerLowCCI2200)

# Autumn LowCCI
AutumnLowCCITimeframeList <- list(AutumnLowCCI0002, AutumnLowCCI0204, AutumnLowCCI0406, AutumnLowCCI0608,
                                  AutumnLowCCI0810, AutumnLowCCI1012, AutumnLowCCI1214, AutumnLowCCI1416,
                                  AutumnLowCCI1618, AutumnLowCCI1820, AutumnLowCCI2022, AutumnLowCCI2200)

# Winter LowCCI
WinterLowCCITimeframeList <- list(WinterLowCCI0002, WinterLowCCI0204, WinterLowCCI0406, WinterLowCCI0608,
                                  WinterLowCCI0810, WinterLowCCI1012, WinterLowCCI1214, WinterLowCCI1416,
                                  WinterLowCCI1618, WinterLowCCI1820, WinterLowCCI2022, WinterLowCCI2200)


## Create lists of timeframes per season (HighCCI)

# Spring HighCCI
SpringHighCCITimeframeList <- list(SpringHighCCI0002, SpringHighCCI0204, SpringHighCCI0406, SpringHighCCI0608,
                                 SpringHighCCI0810, SpringHighCCI1012, SpringHighCCI1214, SpringHighCCI1416,
                                 SpringHighCCI1618, SpringHighCCI1820, SpringHighCCI2022, SpringHighCCI2200)

# Summer HighCCI
SummerHighCCITimeframeList <- list(SummerHighCCI0002, SummerHighCCI0204, SummerHighCCI0406, SummerHighCCI0608,
                                 SummerHighCCI0810, SummerHighCCI1012, SummerHighCCI1214, SummerHighCCI1416,
                                 SummerHighCCI1618, SummerHighCCI1820, SummerHighCCI2022, SummerHighCCI2200)

# Autumn HighCCI
AutumnHighCCITimeframeList <- list(AutumnHighCCI0002, AutumnHighCCI0204, AutumnHighCCI0406, AutumnHighCCI0608,
                                 AutumnHighCCI0810, AutumnHighCCI1012, AutumnHighCCI1214, AutumnHighCCI1416,
                                 AutumnHighCCI1618, AutumnHighCCI1820, AutumnHighCCI2022, AutumnHighCCI2200)

# Winter HighCCI
WinterHighCCITimeframeList <- list(WinterHighCCI0002, WinterHighCCI0204, WinterHighCCI0406, WinterHighCCI0608,
                                 WinterHighCCI0810, WinterHighCCI1012, WinterHighCCI1214, WinterHighCCI1416,
                                 WinterHighCCI1618, WinterHighCCI1820, WinterHighCCI2022, WinterHighCCI2200)

# Call JIperTimeframe for each season (LowCCI)
SpringLowCCITimeframeJITable <- JIperTimeframe(SpringLowCCITimeframeList)
SummerLowCCITimeframeJITable <- JIperTimeframe(SummerLowCCITimeframeList)
AutumnLowCCITimeframeJITable <- JIperTimeframe(AutumnLowCCITimeframeList)
WinterLowCCITimeframeJITable <- JIperTimeframe(WinterLowCCITimeframeList)

# Call JIperTimeframe for each season (HighCCI)
SpringHighCCITimeframeJITable <- JIperTimeframe(SpringHighCCITimeframeList)
SummerHighCCITimeframeJITable <- JIperTimeframe(SummerHighCCITimeframeList)
AutumnHighCCITimeframeJITable <- JIperTimeframe(AutumnHighCCITimeframeList)
WinterHighCCITimeframeJITable <- JIperTimeframe(WinterHighCCITimeframeList)