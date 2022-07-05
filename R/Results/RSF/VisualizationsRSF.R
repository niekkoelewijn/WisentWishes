# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script visualize the results of the RSF in bar charts and maps

## Maps with for all study areas, per season, hour of the day, and average CCI

# Define funcion to rescale HSM raster
RescaleAndVisualizeHSM <- function(HSMRaster){
  
  # Get positive values for selection, negative for avoidance
  LogTransformedRaster <- log((HSMRaster)/(1/11))
  
  # Get values lower than -1 to replace them by -1
  ToSmall <- which(LogTransformedRaster[] < -1)
  
  # Get values bigger than 1 to replace them by w1
  ToBig <- which(LogTransformedRaster[] > 1)
  
  # Replace values
  LogTransformedRaster[ToSmall] <- -1
  LogTransformedRaster[ToBig] <- 1
  
  # Plot HSM raster with on a scale from -1 (strong avoidance) to 1 (strong selection)
  plot(LogTransformedRaster, zlim = c(-1,1))
}


LogKraansvlakSpringNight <- log((KraansvlakSpringNight)/(1/11))
ToSmall <- which(LogKraansvlakSpringNight[] < -1)
LogKraansvlakSpringNight[ToSmall] <- -1
plot(LogKraansvlakSpringNight, zlim = c(-1,1))

# Kraansvlak
KraansvlakSpringDay <- getHSM("Kraansvlak", getMeanCCI("Kraansvlak", "spring", "12:00:00"), "12:00:00", "spring")
KraansvlakSpringNight <- getHSM("Kraansvlak", getMeanCCI("Kraansvlak", "spring", "00:00:00"), "00:00:00", "spring")
KraansvlakSummerDay <- getHSM("Kraansvlak", getMeanCCI("Kraansvlak", "summer", "12:00:00"), "12:00:00", "summer")
KraansvlakSummerNight <- getHSM("Kraansvlak", getMeanCCI("Kraansvlak", "summer", "00:00:00"), "00:00:00", "summer")
KraansvlakAutumnDay <- getHSM("Kraansvlak", getMeanCCI("Kraansvlak", "autumn", "12:00:00"), "12:00:00", "autumn")
KraansvlakAutumnNight <- getHSM("Kraansvlak", getMeanCCI("Kraansvlak", "autumn", "00:00:00"), "00:00:00", "autumn")
KraansvlakWinterDay <- getHSM("Kraansvlak", getMeanCCI("Kraansvlak", "winter", "12:00:00"), "12:00:00", "winter")
KraansvlakWinterNight <- getHSM("Kraansvlak", getMeanCCI("Kraansvlak", "winter", "00:00:00"), "00:00:00", "winter")

RescaleAndVisualizeHSM(KraansvlakSpringDay)
RescaleAndVisualizeHSM(KraansvlakSpringNight)
RescaleAndVisualizeHSM(KraansvlakSummerDay)
RescaleAndVisualizeHSM(KraansvlakSummerNight)
RescaleAndVisualizeHSM(KraansvlakAutumnDay)
RescaleAndVisualizeHSM(KraansvlakAutumnNight)
RescaleAndVisualizeHSM(KraansvlakWinterDay)
RescaleAndVisualizeHSM(KraansvlakWinterNight)

# Maashorst2016
Maashorst2016SpringDay <- getHSM("Maashorst2016", getMeanCCI("Maashorst2016", "spring", "12:00:00"), "12:00:00", "spring")
Maashorst2016SpringNight <- getHSM("Maashorst2016", getMeanCCI("Maashorst2016", "spring", "00:00:00"), "00:00:00", "spring")
Maashorst2016SummerDay <- getHSM("Maashorst2016", getMeanCCI("Maashorst2016", "summer", "12:00:00"), "12:00:00", "summer")
Maashorst2016SummerNight <- getHSM("Maashorst2016", getMeanCCI("Maashorst2016", "summer", "00:00:00"), "00:00:00", "summer")
Maashorst2016AutumnDay <- getHSM("Maashorst2016", getMeanCCI("Maashorst2016", "autumn", "12:00:00"), "12:00:00", "autumn")
Maashorst2016AutumnNight <- getHSM("Maashorst2016", getMeanCCI("Maashorst2016", "autumn", "00:00:00"), "00:00:00", "autumn")
Maashorst2016WinterDay <- getHSM("Maashorst2016", getMeanCCI("Maashorst2016", "winter", "12:00:00"), "12:00:00", "winter")
Maashorst2016WinterNight <- getHSM("Maashorst2016", getMeanCCI("Maashorst2016", "winter", "00:00:00"), "00:00:00", "winter")

RescaleAndVisualizeHSM(Maashorst2016SpringDay)
RescaleAndVisualizeHSM(Maashorst2016SpringNight)
RescaleAndVisualizeHSM(Maashorst2016SummerDay)
RescaleAndVisualizeHSM(Maashorst2016SummerNight)
RescaleAndVisualizeHSM(Maashorst2016AutumnDay)
RescaleAndVisualizeHSM(Maashorst2016AutumnNight)
RescaleAndVisualizeHSM(Maashorst2016WinterDay)
RescaleAndVisualizeHSM(Maashorst2016WinterNight)

# Maashorst20172021
Maashorst20172021SpringDay <- getHSM("Maashorst20172021", getMeanCCI("Maashorst20172021", "spring", "12:00:00"), "12:00:00", "spring")
Maashorst20172021SpringNight <- getHSM("Maashorst20172021", getMeanCCI("Maashorst20172021", "spring", "00:00:00"), "00:00:00", "spring")
Maashorst20172021SummerDay <- getHSM("Maashorst20172021", getMeanCCI("Maashorst20172021", "summer", "12:00:00"), "12:00:00", "summer")
Maashorst20172021SummerNight <- getHSM("Maashorst20172021", getMeanCCI("Maashorst20172021", "summer", "00:00:00"), "00:00:00", "summer")
Maashorst20172021AutumnDay <- getHSM("Maashorst20172021", getMeanCCI("Maashorst20172021", "autumn", "12:00:00"), "12:00:00", "autumn")
Maashorst20172021AutumnNight <- getHSM("Maashorst20172021", getMeanCCI("Maashorst20172021", "autumn", "00:00:00"), "00:00:00", "autumn")
Maashorst20172021WinterDay <- getHSM("Maashorst20172021", getMeanCCI("Maashorst20172021", "winter", "12:00:00"), "12:00:00", "winter")
Maashorst20172021WinterNight <- getHSM("Maashorst20172021", getMeanCCI("Maashorst20172021", "winter", "00:00:00"), "00:00:00", "winter")

RescaleAndVisualizeHSM(Maashorst20172021SpringDay)
RescaleAndVisualizeHSM(Maashorst20172021SpringNight)
RescaleAndVisualizeHSM(Maashorst20172021SummerDay)
RescaleAndVisualizeHSM(Maashorst20172021SummerNight)
RescaleAndVisualizeHSM(Maashorst20172021AutumnDay)
RescaleAndVisualizeHSM(Maashorst20172021AutumnNight)
RescaleAndVisualizeHSM(Maashorst20172021WinterDay)
RescaleAndVisualizeHSM(Maashorst20172021WinterNight)

# Maashorst2022
Maashorst2022SpringDay <- getHSM("Maashorst2022", getMeanCCI("Maashorst2022", "spring", "12:00:00"), "12:00:00", "spring")
Maashorst2022SpringNight <- getHSM("Maashorst2022", getMeanCCI("Maashorst2022", "spring", "00:00:00"), "00:00:00", "spring")
Maashorst2022SummerDay <- getHSM("Maashorst2022", getMeanCCI("Maashorst2022", "summer", "12:00:00"), "12:00:00", "summer")
Maashorst2022SummerNight <- getHSM("Maashorst2022", getMeanCCI("Maashorst2022", "summer", "00:00:00"), "00:00:00", "summer")
Maashorst2022AutumnDay <- getHSM("Maashorst2022", getMeanCCI("Maashorst2022", "autumn", "12:00:00"), "12:00:00", "autumn")
Maashorst2022AutumnNight <- getHSM("Maashorst2022", getMeanCCI("Maashorst2022", "autumn", "00:00:00"), "00:00:00", "autumn")
Maashorst2022WinterDay <- getHSM("Maashorst2022", getMeanCCI("Maashorst2022", "winter", "12:00:00"), "12:00:00", "winter")
Maashorst2022WinterNight <- getHSM("Maashorst2022", getMeanCCI("Maashorst2022", "winter", "00:00:00"), "00:00:00", "winter")

RescaleAndVisualizeHSM(Maashorst2022SpringDay)
RescaleAndVisualizeHSM(Maashorst2022SpringNight)
RescaleAndVisualizeHSM(Maashorst2022SummerDay)
RescaleAndVisualizeHSM(Maashorst2022SummerNight)
RescaleAndVisualizeHSM(Maashorst2022AutumnDay)
RescaleAndVisualizeHSM(Maashorst2022AutumnNight)
RescaleAndVisualizeHSM(Maashorst2022WinterDay)
RescaleAndVisualizeHSM(Maashorst2022WinterNight)

# SlikkenvdHeen
SlikkenvdHeenSpringDay <- getHSM("SlikkenvdHeen", getMeanCCI("SlikkenvdHeen", "spring", "12:00:00"), "12:00:00", "spring")
SlikkenvdHeenSpringNight <- getHSM("SlikkenvdHeen", getMeanCCI("SlikkenvdHeen", "spring", "00:00:00"), "00:00:00", "spring")
SlikkenvdHeenSummerDay <- getHSM("SlikkenvdHeen", getMeanCCI("SlikkenvdHeen", "summer", "12:00:00"), "12:00:00", "summer")
SlikkenvdHeenSummerNight <- getHSM("SlikkenvdHeen", getMeanCCI("SlikkenvdHeen", "summer", "00:00:00"), "00:00:00", "summer")
SlikkenvdHeenAutumnDay <- getHSM("SlikkenvdHeen", getMeanCCI("SlikkenvdHeen", "autumn", "12:00:00"), "12:00:00", "autumn")
SlikkenvdHeenAutumnNight <- getHSM("SlikkenvdHeen", getMeanCCI("SlikkenvdHeen", "autumn", "00:00:00"), "00:00:00", "autumn")
SlikkenvdHeenWinterDay <- getHSM("SlikkenvdHeen", getMeanCCI("SlikkenvdHeen", "winter", "12:00:00"), "12:00:00", "winter")
SlikkenvdHeenWinterNight <- getHSM("SlikkenvdHeen", getMeanCCI("SlikkenvdHeen", "winter", "00:00:00"), "00:00:00", "winter")

RescaleAndVisualizeHSM(SlikkenvdHeenSpringDay)
RescaleAndVisualizeHSM(SlikkenvdHeenSpringNight)
RescaleAndVisualizeHSM(SlikkenvdHeenSummerDay)
RescaleAndVisualizeHSM(SlikkenvdHeenSummerNight)
RescaleAndVisualizeHSM(SlikkenvdHeenAutumnDay)
RescaleAndVisualizeHSM(SlikkenvdHeenAutumnNight)
RescaleAndVisualizeHSM(SlikkenvdHeenWinterDay)
RescaleAndVisualizeHSM(SlikkenvdHeenWinterNight)

# SlikkenvdHeenHab
SlikkenvdHeenHabSpringDay <- getHSM("SlikkenvdHeenHab", getMeanCCI("SlikkenvdHeenHab", "spring", "12:00:00"), "12:00:00", "spring")
SlikkenvdHeenHabSpringNight <- getHSM("SlikkenvdHeenHab", getMeanCCI("SlikkenvdHeenHab", "spring", "00:00:00"), "00:00:00", "spring")
SlikkenvdHeenHabSummerDay <- getHSM("SlikkenvdHeenHab", getMeanCCI("SlikkenvdHeenHab", "summer", "12:00:00"), "12:00:00", "summer")
SlikkenvdHeenHabSummerNight <- getHSM("SlikkenvdHeenHab", getMeanCCI("SlikkenvdHeenHab", "summer", "00:00:00"), "00:00:00", "summer")
SlikkenvdHeenHabAutumnDay <- getHSM("SlikkenvdHeenHab", getMeanCCI("SlikkenvdHeenHab", "autumn", "12:00:00"), "12:00:00", "autumn")
SlikkenvdHeenHabAutumnNight <- getHSM("SlikkenvdHeenHab", getMeanCCI("SlikkenvdHeenHab", "autumn", "00:00:00"), "00:00:00", "autumn")
SlikkenvdHeenHabWinterDay <- getHSM("SlikkenvdHeenHab", getMeanCCI("SlikkenvdHeenHab", "winter", "12:00:00"), "12:00:00", "winter")
SlikkenvdHeenHabWinterNight <- getHSM("SlikkenvdHeenHab", getMeanCCI("SlikkenvdHeenHab", "winter", "00:00:00"), "00:00:00", "winter")

RescaleAndVisualizeHSM(SlikkenvdHeenHabSpringDay)
RescaleAndVisualizeHSM(SlikkenvdHeenHabSpringNight)
RescaleAndVisualizeHSM(SlikkenvdHeenHabSummerDay)
RescaleAndVisualizeHSM(SlikkenvdHeenHabSummerNight)
RescaleAndVisualizeHSM(SlikkenvdHeenHabAutumnDay)
RescaleAndVisualizeHSM(SlikkenvdHeenHabAutumnNight)
RescaleAndVisualizeHSM(SlikkenvdHeenHabWinterDay)
RescaleAndVisualizeHSM(SlikkenvdHeenHabWinterNight)

# Veluwe
VeluweSpringDay <- getHSM("Veluwe", getMeanCCI("Veluwe", "spring", "12:00:00"), "12:00:00", "spring")
VeluweSpringNight <- getHSM("Veluwe", getMeanCCI("Veluwe", "spring", "00:00:00"), "00:00:00", "spring")
VeluweSummerDay <- getHSM("Veluwe", getMeanCCI("Veluwe", "summer", "12:00:00"), "12:00:00", "summer")
VeluweSummerNight <- getHSM("Veluwe", getMeanCCI("Veluwe", "summer", "00:00:00"), "00:00:00", "summer")
VeluweAutumnDay <- getHSM("Veluwe", getMeanCCI("Veluwe", "autumn", "12:00:00"), "12:00:00", "autumn")
VeluweAutumnNight <- getHSM("Veluwe", getMeanCCI("Veluwe", "autumn", "00:00:00"), "00:00:00", "autumn")
VeluweWinterDay <- getHSM("Veluwe", getMeanCCI("Veluwe", "winter", "12:00:00"), "12:00:00", "winter")
VeluweWinterNight <- getHSM("Veluwe", getMeanCCI("Veluwe", "winter", "00:00:00"), "00:00:00", "winter")

RescaleAndVisualizeHSM(VeluweSpringDay)
RescaleAndVisualizeHSM(VeluweSpringNight)
RescaleAndVisualizeHSM(VeluweSummerDay)
RescaleAndVisualizeHSM(VeluweSummerNight)
RescaleAndVisualizeHSM(VeluweAutumnDay)
RescaleAndVisualizeHSM(VeluweAutumnNight)
RescaleAndVisualizeHSM(VeluweWinterDay)
RescaleAndVisualizeHSM(VeluweWinterNight)

# VeluweHab
VeluweHabSpringDay <- getHSM("VeluweHab", getMeanCCI("VeluweHab", "spring", "12:00:00"), "12:00:00", "spring")
VeluweHabSpringNight <- getHSM("VeluweHab", getMeanCCI("VeluweHab", "spring", "00:00:00"), "00:00:00", "spring")
VeluweHabSummerDay <- getHSM("VeluweHab", getMeanCCI("VeluweHab", "summer", "12:00:00"), "12:00:00", "summer")
VeluweHabSummerNight <- getHSM("VeluweHab", getMeanCCI("VeluweHab", "summer", "00:00:00"), "00:00:00", "summer")
VeluweHabAutumnDay <- getHSM("VeluweHab", getMeanCCI("VeluweHab", "autumn", "12:00:00"), "12:00:00", "autumn")
VeluweHabAutumnNight <- getHSM("VeluweHab", getMeanCCI("VeluweHab", "autumn", "00:00:00"), "00:00:00", "autumn")
VeluweHabWinterDay <- getHSM("VeluweHab", getMeanCCI("VeluweHab", "winter", "12:00:00"), "12:00:00", "winter")
VeluweHabWinterNight <- getHSM("VeluweHab", getMeanCCI("VeluweHab", "winter", "00:00:00"), "00:00:00", "winter")

RescaleAndVisualizeHSM(VeluweHabSpringDay)
RescaleAndVisualizeHSM(VeluweHabSpringNight)
RescaleAndVisualizeHSM(VeluweHabSummerDay)
RescaleAndVisualizeHSM(VeluweHabSummerNight)
RescaleAndVisualizeHSM(VeluweHabAutumnDay)
RescaleAndVisualizeHSM(VeluweHabAutumnNight)
RescaleAndVisualizeHSM(VeluweHabWinterDay)
RescaleAndVisualizeHSM(VeluweHabWinterNight)

# KraansvlakWinterDay <- getHSM("Kraansvlak", getMeanCCI("Kraansvlak", "winter", "12:00:00"), "12:00:00", "winter")
# plot(KraansvlakWinterDay)
# KraansvlakWinterNight <- getHSM("Kraansvlak", getMeanCCI("Kraansvlak", "winter", "00:00:00"), "00:00:00", "winter")
# plot(KraansvlakWinterNight)
# 
# writeRaster(KraansvlakSummerDay,'~/WisentWishes/ResultsRSF/Kraansvlak/KraansvlakSummerDay',options=c('TFW=YES'), overwrite = T)
# writeRaster(KraansvlakSummerNight,'~/WisentWishes/ResultsRSF/Kraansvlak/KraansvlakSummerNight',options=c('TFW=YES'), overwrite = T)
# writeRaster(KraansvlakWinterDay,'~/WisentWishes/ResultsRSF/Kraansvlak/KraansvlakWinterDay',options=c('TFW=YES'), overwrite = T)
# writeRaster(KraansvlakWinterNight,'~/WisentWishes/ResultsRSF/Kraansvlak/KraansvlakWinterNight',options=c('TFW=YES'), overwrite = T)
