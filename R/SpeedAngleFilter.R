# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to filter out unrealistic speeds and turning angles to filter out unrealistic movement / spikes ###

## First assign variables to all GPS datasets

# Veluwe
path = "~/WisentWishes/MScThesisData/GPS location data/Veluwe/Preprocessed/"
SharaVeluwe20162020QF <- read.csv(paste0(path, "SharaVeluwe20162020_qualityfiltered.csv"))

# Maashorst
path = "~/WisentWishes/MScThesisData/GPS location data/Maashorst/Preprocessed/"
DeliaMaashorst2016QF <- read.csv(paste0(path, "DeliaMaashorst2016_qualityfiltered.csv"))
KraylaMaashorst2016QF <- read.csv(paste0(path, "KraylaMaashorst2016_qualityfiltered.csv"))
KroosjaMaashorst20162018QF <- read.csv(paste0(path, "KroosjaMaashorst20162018_qualityfiltered.csv"))
MaaikeMaashorst20192022QF <- read.csv(paste0(path, "MaaikeMaashorst20192022_qualityfiltered.csv"))
BullEverestMaashorst2022QF <- read.csv(paste0(path, "BullEverestMaashorst2022_qualityfiltered.csv"))

# Slikken vd Heen
path = "~/WisentWishes/MScThesisData/GPS location data/SlikkenvdHeen/Preprocessed/"
CaliopeSvdH20202021QF <- read.csv(paste0(path, "CaliopeSvdH20202021_qualityfiltered.csv"))
NadiaSvdH20202022QF <- read.csv(paste0(path, "NadiaSvdH20202022_qualityfiltered.csv"))

# Kraansvlak
path = "~/WisentWishes/MScThesisData/GPS location data/Kraansvlak/Preprocessed/"
Kraansvlak20202022QF <- read.csv(paste0(path, "Kraansvlak20202022_qualityfiltered.csv"))


## Merge date and time attribute to one time column when necessary

# Veluwe dataset
SharaVeluwe20162020QF <- na.omit(SharaVeluwe20162020QF)
SharaVeluwe20162020QF$time_human_readable <- as.POSIXct(paste(SharaVeluwe20162020QF$Date, SharaVeluwe20162020QF$Time), format="%m/%d/%Y %H:%M")
SharaVeluwe20162020QF$time_coded <- as.numeric(SharaVeluwe20162020QF$time_human_readable)

## Get time decoded for all GPS other datasets

# Maashorst
DeliaMaashorst2016QF$time_human_readable <- as.POSIXct(DeliaMaashorst2016QF$time, format = "%Y-%m-%d %H:%M:%S")
DeliaMaashorst2016QF$time_coded <- as.numeric(DeliaMaashorst2016QF$time_human_readable)

KraylaMaashorst2016QF$time_human_readable <- as.POSIXct(KraylaMaashorst2016QF$time, format = "%Y-%m-%d %H:%M:%S")
KraylaMaashorst2016QF$time_coded <- as.numeric(KraylaMaashorst2016QF$time_human_readable)

KroosjaMaashorst20162018QF$time_human_readable <- as.POSIXct(KroosjaMaashorst20162018QF$time, format = "%Y-%m-%d %H:%M:%S")
KroosjaMaashorst20162018QF$time_coded <- as.numeric(KroosjaMaashorst20162018QF$time_human_readable)

MaaikeMaashorst20192022QF$time_human_readable <- as.POSIXct(MaaikeMaashorst20192022QF$time, format = "%Y-%m-%d %H:%M:%S")
MaaikeMaashorst20192022QF$time_coded <- as.numeric(MaaikeMaashorst20192022QF$time_human_readable)

BullEverestMaashorst2022QF$time_human_readable <- as.POSIXct(BullEverestMaashorst2022QF$time, format = "%Y-%m-%d %H:%M:%S")
BullEverestMaashorst2022QF$time_coded <- as.numeric(BullEverestMaashorst2022QF$time_human_readable)

# Slikken vd Heen
CaliopeSvdH20202021QF$time_human_readable <- as.POSIXct(CaliopeSvdH20202021QF$time, format = "%Y-%m-%d %H:%M:%S")
CaliopeSvdH20202021QF$time_coded <- as.numeric(CaliopeSvdH20202021QF$time_human_readable)

NadiaSvdH20202022QF$time_human_readable <- as.POSIXct(NadiaSvdH20202022QF$time, format = "%Y-%m-%d %H:%M:%S")
NadiaSvdH20202022QF$time_coded <- as.numeric(NadiaSvdH20202022QF$time_human_readable)

# Kraansvlak GPS points already have a numeric time stemp


### To calculate speed attribute, the xy coordinate in RD new needs to be calculated and added as attribute

## Veluwe
SharaVeluwe20162020QF <- na.omit(SharaVeluwe20162020QF)
SharaVeluwe20162020QFSF <- st_as_sf(SharaVeluwe20162020QF, coords = c("lng", "lat"), crs = st_crs(4326))
SharaVeluwe20162020QFSF <- st_transform(SharaVeluwe20162020QFSF, crs = st_crs(28992))

for(i in 1:nrow(SharaVeluwe20162020QFSF)){
  SharaVeluwe20162020QFSF$RDnX[i] <- st_geometry(SharaVeluwe20162020QFSF)[[i]][1]
  SharaVeluwe20162020QFSF$RDnY[i] <- st_geometry(SharaVeluwe20162020QFSF)[[i]][2]
}

## Maashorst

# Delia
DeliaMaashorst2016QF <- na.omit(DeliaMaashorst2016QF)
DeliaMaashorst2016QFSF <- st_as_sf(DeliaMaashorst2016QF, coords = c("lng", "lat"), crs = st_crs(4326))
DeliaMaashorst2016QFSF <- st_transform(DeliaMaashorst2016QFSF, crs = st_crs(28992))

for(i in 1:nrow(DeliaMaashorst2016QFSF)){
  DeliaMaashorst2016QFSF$RDnX[i] <- st_geometry(DeliaMaashorst2016QFSF)[[i]][1]
  DeliaMaashorst2016QFSF$RDnY[i] <- st_geometry(DeliaMaashorst2016QFSF)[[i]][2]
}

# Krayla
KraylaMaashorst2016QF <- na.omit(KraylaMaashorst2016QF)
KraylaMaashorst2016QFSF <- st_as_sf(KraylaMaashorst2016QF, coords = c("lng", "lat"), crs = st_crs(4326))
KraylaMaashorst2016QFSF <- st_transform(KraylaMaashorst2016QFSF, crs = st_crs(28992))

for(i in 1:nrow(KraylaMaashorst2016QFSF)){
  KraylaMaashorst2016QFSF$RDnX[i] <- st_geometry(KraylaMaashorst2016QFSF)[[i]][1]
  KraylaMaashorst2016QFSF$RDnY[i] <- st_geometry(KraylaMaashorst2016QFSF)[[i]][2]
}

# Kroosja
KroosjaMaashorst20162018QF <- na.omit(KroosjaMaashorst20162018QF)
KroosjaMaashorst20162018QFSF <- st_as_sf(KroosjaMaashorst20162018QF, coords = c("lng", "lat"), crs = st_crs(4326))
KroosjaMaashorst20162018QFSF <- st_transform(KroosjaMaashorst20162018QFSF, crs = st_crs(28992))

for(i in 1:nrow(KroosjaMaashorst20162018QFSF)){
  KroosjaMaashorst20162018QFSF$RDnX[i] <- st_geometry(KroosjaMaashorst20162018QFSF)[[i]][1]
  KroosjaMaashorst20162018QFSF$RDnY[i] <- st_geometry(KroosjaMaashorst20162018QFSF)[[i]][2]
}

# Maaike
MaaikeMaashorst20192022QF <- na.omit(MaaikeMaashorst20192022QF)
MaaikeMaashorst20192022QFSF <- st_as_sf(MaaikeMaashorst20192022QF, coords = c("lng", "lat"), crs = st_crs(4326))
MaaikeMaashorst20192022QFSF <- st_transform(MaaikeMaashorst20192022QFSF, crs = st_crs(28992))

for(i in 1:nrow(MaaikeMaashorst20192022QFSF)){
  MaaikeMaashorst20192022QFSF$RDnX[i] <- st_geometry(MaaikeMaashorst20192022QFSF)[[i]][1]
  MaaikeMaashorst20192022QFSF$RDnY[i] <- st_geometry(MaaikeMaashorst20192022QFSF)[[i]][2]
}

# Bull Everest
BullEverestMaashorst2022QF <- na.omit(BullEverestMaashorst2022QF)
BullEverestMaashorst2022QFSF <- st_as_sf(BullEverestMaashorst2022QF, coords = c("lng", "lat"), crs = st_crs(4326))
BullEverestMaashorst2022QFSF <- st_transform(BullEverestMaashorst2022QFSF, crs = st_crs(28992))

for(i in 1:nrow(BullEverestMaashorst2022QFSF)){
  BullEverestMaashorst2022QFSF$RDnX[i] <- st_geometry(BullEverestMaashorst2022QFSF)[[i]][1]
  BullEverestMaashorst2022QFSF$RDnY[i] <- st_geometry(BullEverestMaashorst2022QFSF)[[i]][2]
}


## Slikken vd Heen

# Caliope
CaliopeSvdH20202021QF <- na.omit(CaliopeSvdH20202021QF)
CaliopeSvdH20202021QFSF <- st_as_sf(CaliopeSvdH20202021QF, coords = c("lng", "lat"), crs = st_crs(4326))
CaliopeSvdH20202021QFSF <- st_transform(CaliopeSvdH20202021QFSF, crs = st_crs(28992))

for(i in 1:nrow(CaliopeSvdH20202021QFSF)){
  CaliopeSvdH20202021QFSF$RDnX[i] <- st_geometry(CaliopeSvdH20202021QFSF)[[i]][1]
  CaliopeSvdH20202021QFSF$RDnY[i] <- st_geometry(CaliopeSvdH20202021QFSF)[[i]][2]
}

# Nadia
NadiaSvdH20202022QF <- na.omit(NadiaSvdH20202022QF)
NadiaSvdH20202022QFSF <- st_as_sf(NadiaSvdH20202022QF, coords = c("lng", "lat"), crs = st_crs(4326))
NadiaSvdH20202022QFSF <- st_transform(NadiaSvdH20202022QFSF, crs = st_crs(28992))

for(i in 1:nrow(NadiaSvdH20202022QFSF)){
  NadiaSvdH20202022QFSF$RDnX[i] <- st_geometry(NadiaSvdH20202022QFSF)[[i]][1]
  NadiaSvdH20202022QFSF$RDnY[i] <- st_geometry(NadiaSvdH20202022QFSF)[[i]][2]
}

## Kraansvlak
Kraansvlak20202022QF <- na.omit(Kraansvlak20202022QF)
Kraansvlak20202022QFSF <- st_as_sf(Kraansvlak20202022QF, coords = c("lng", "lat"), crs = st_crs(4326))
Kraansvlak20202022QFSF <- st_transform(Kraansvlak20202022QFSF, crs = st_crs(28992))

for(i in 1:nrow(Kraansvlak20202022QFSF)){
  Kraansvlak20202022QFSF$RDnX[i] <- st_geometry(Kraansvlak20202022QFSF)[[i]][1]
  Kraansvlak20202022QFSF$RDnY[i] <- st_geometry(Kraansvlak20202022QFSF)[[i]][2]
}


### Now assign speed in, speed out, angle and time interval attributes to all GPS datasets


## Veluwe
SharaVeluwe20162020QFSF$speed_in <- atl_get_speed(SharaVeluwe20162020QFSF, x = "RDnX", y = "RDnY",
                                                time = "time_coded", type = "in")
SharaVeluwe20162020QFSF$speed_out <- atl_get_speed(SharaVeluwe20162020QFSF, x = "RDnX", y = "RDnY",
                                                time = "time_coded", type = "out")
SharaVeluwe20162020QFSF$turning_angle <- atl_turning_angle(SharaVeluwe20162020QFSF, x = "RDnX", y = "RDnY",
                                                time = "time_coded")

SharaVeluwe20162020QFSF$time_interval <- NA
for(i in 1:length(SharaVeluwe20162020QFSF$time_interval)){
  if(i == 1){
    SharaVeluwe20162020QFSF$time_interval[i] <- NA
  }else{
    SharaVeluwe20162020QFSF$time_interval[i] <- (SharaVeluwe20162020QFSF$time_coded[i]) - (SharaVeluwe20162020QFSF$time_coded[i-1])
  }
}


## Maashorst

# Delia
DeliaMaashorst2016QFSF$speed_in <- atl_get_speed(DeliaMaashorst2016QFSF, x = "RDnX", y = "RDnY",
                                                time = "time_coded", type = "in")
DeliaMaashorst2016QFSF$speed_out <- atl_get_speed(DeliaMaashorst2016QFSF, x = "RDnX", y = "RDnY",
                                                 time = "time_coded", type = "out")
DeliaMaashorst2016QFSF$turning_angle <- atl_turning_angle(DeliaMaashorst2016QFSF, x = "RDnX", y = "RDnY",
                                                         time = "time_coded")

DeliaMaashorst2016QFSF$time_interval <- NA
for(i in 1:length(DeliaMaashorst2016QFSF$time_interval)){
  if(i == 1){
    DeliaMaashorst2016QFSF$time_interval[i] <- NA
  }else{
    DeliaMaashorst2016QFSF$time_interval[i] <- (DeliaMaashorst2016QFSF$time_coded[i]) - (DeliaMaashorst2016QFSF$time_coded[i-1])
  }
}

# Krayla
KraylaMaashorst2016QFSF$speed_in <- atl_get_speed(KraylaMaashorst2016QFSF, x = "RDnX", y = "RDnY",
                                                 time = "time_coded", type = "in")
KraylaMaashorst2016QFSF$speed_out <- atl_get_speed(KraylaMaashorst2016QFSF, x = "RDnX", y = "RDnY",
                                                  time = "time_coded", type = "out")
KraylaMaashorst2016QFSF$turning_angle <- atl_turning_angle(KraylaMaashorst2016QFSF, x = "RDnX", y = "RDnY",
                                                          time = "time_coded")

KraylaMaashorst2016QFSF$time_interval <- NA
for(i in 1:length(KraylaMaashorst2016QFSF$time_interval)){
  if(i == 1){
    KraylaMaashorst2016QFSF$time_interval[i] <- NA
  }else{
    KraylaMaashorst2016QFSF$time_interval[i] <- (KraylaMaashorst2016QFSF$time_coded[i]) - (KraylaMaashorst2016QFSF$time_coded[i-1])
  }
}

# Kroosja
KroosjaMaashorst20162018QFSF$speed_in <- atl_get_speed(KroosjaMaashorst20162018QFSF, x = "RDnX", y = "RDnY",
                                                  time = "time_coded", type = "in")
KroosjaMaashorst20162018QFSF$speed_out <- atl_get_speed(KroosjaMaashorst20162018QFSF, x = "RDnX", y = "RDnY",
                                                   time = "time_coded", type = "out")
KroosjaMaashorst20162018QFSF$turning_angle <- atl_turning_angle(KroosjaMaashorst20162018QFSF, x = "RDnX", y = "RDnY",
                                                           time = "time_coded")

KroosjaMaashorst20162018QFSF$time_interval <- NA
for(i in 1:length(KroosjaMaashorst20162018QFSF$time_interval)){
  if(i == 1){
    KroosjaMaashorst20162018QFSF$time_interval[i] <- NA
  }else{
    KroosjaMaashorst20162018QFSF$time_interval[i] <- (KroosjaMaashorst20162018QFSF$time_coded[i]) - (KroosjaMaashorst20162018QFSF$time_coded[i-1])
  }
}

# Maaike
MaaikeMaashorst20192022QFSF$speed_in <- atl_get_speed(MaaikeMaashorst20192022QFSF, x = "RDnX", y = "RDnY",
                                                       time = "time_coded", type = "in")
MaaikeMaashorst20192022QFSF$speed_out <- atl_get_speed(MaaikeMaashorst20192022QFSF, x = "RDnX", y = "RDnY",
                                                        time = "time_coded", type = "out")
MaaikeMaashorst20192022QFSF$turning_angle <- atl_turning_angle(MaaikeMaashorst20192022QFSF, x = "RDnX", y = "RDnY",
                                                                time = "time_coded")

MaaikeMaashorst20192022QFSF$time_interval <- NA
for(i in 1:length(MaaikeMaashorst20192022QFSF$time_interval)){
  if(i == 1){
    MaaikeMaashorst20192022QFSF$time_interval[i] <- NA
  }else{
    MaaikeMaashorst20192022QFSF$time_interval[i] <- (MaaikeMaashorst20192022QFSF$time_coded[i]) - (MaaikeMaashorst20192022QFSF$time_coded[i-1])
  }
}

# Bull Everest
BullEverestMaashorst2022QFSF$speed_in <- atl_get_speed(BullEverestMaashorst2022QFSF, x = "RDnX", y = "RDnY",
                                                      time = "time_coded", type = "in")
BullEverestMaashorst2022QFSF$speed_out <- atl_get_speed(BullEverestMaashorst2022QFSF, x = "RDnX", y = "RDnY",
                                                       time = "time_coded", type = "out")
BullEverestMaashorst2022QFSF$turning_angle <- atl_turning_angle(BullEverestMaashorst2022QFSF, x = "RDnX", y = "RDnY",
                                                               time = "time_coded")

BullEverestMaashorst2022QFSF$time_interval <- NA
for(i in 1:length(BullEverestMaashorst2022QFSF$time_interval)){
  if(i == 1){
    BullEverestMaashorst2022QFSF$time_interval[i] <- NA
  }else{
    BullEverestMaashorst2022QFSF$time_interval[i] <- (BullEverestMaashorst2022QFSF$time_coded[i]) - (BullEverestMaashorst2022QFSF$time_coded[i-1])
  }
}

## Slikken vd Heen

# Caliope
CaliopeSvdH20202021QFSF$speed_in <- atl_get_speed(CaliopeSvdH20202021QFSF, x = "RDnX", y = "RDnY",
                                                       time = "time_coded", type = "in")
CaliopeSvdH20202021QFSF$speed_out <- atl_get_speed(CaliopeSvdH20202021QFSF, x = "RDnX", y = "RDnY",
                                                        time = "time_coded", type = "out")
CaliopeSvdH20202021QFSF$turning_angle <- atl_turning_angle(CaliopeSvdH20202021QFSF, x = "RDnX", y = "RDnY",
                                                                time = "time_coded")

CaliopeSvdH20202021QFSF$time_interval <- NA
for(i in 1:length(CaliopeSvdH20202021QFSF$time_interval)){
  if(i == 1){
    CaliopeSvdH20202021QFSF$time_interval[i] <- NA
  }else{
    CaliopeSvdH20202021QFSF$time_interval[i] <- (CaliopeSvdH20202021QFSF$time_coded[i]) - (CaliopeSvdH20202021QFSF$time_coded[i-1])
  }
}

# Nadia
NadiaSvdH20202022QFSF$speed_in <- atl_get_speed(NadiaSvdH20202022QFSF, x = "RDnX", y = "RDnY",
                                                  time = "time_coded", type = "in")
NadiaSvdH20202022QFSF$speed_out <- atl_get_speed(NadiaSvdH20202022QFSF, x = "RDnX", y = "RDnY",
                                                   time = "time_coded", type = "out")
NadiaSvdH20202022QFSF$turning_angle <- atl_turning_angle(NadiaSvdH20202022QFSF, x = "RDnX", y = "RDnY",
                                                           time = "time_coded")

NadiaSvdH20202022QFSF$time_interval <- NA
for(i in 1:length(NadiaSvdH20202022QFSF$time_interval)){
  if(i == 1){
    NadiaSvdH20202022QFSF$time_interval[i] <- NA
  }else{
    NadiaSvdH20202022QFSF$time_interval[i] <- (NadiaSvdH20202022QFSF$time_coded[i]) - (NadiaSvdH20202022QFSF$time_coded[i-1])
  }
}

## Kraansvlak
names(Kraansvlak20202022QFSF)[names(Kraansvlak20202022QFSF) == 'time_of_fix'] <- "time_coded"
Kraansvlak20202022QFSF$speed_in <- atl_get_speed(Kraansvlak20202022QFSF, x = "RDnX", y = "RDnY",
                                                time = "time_coded", type = "in")
Kraansvlak20202022QFSF$speed_out <- atl_get_speed(Kraansvlak20202022QFSF, x = "RDnX", y = "RDnY",
                                                 time = "time_coded", type = "out")
Kraansvlak20202022QFSF$turning_angle <- atl_turning_angle(Kraansvlak20202022QFSF, x = "RDnX", y = "RDnY",
                                                         time = "time_coded")

Kraansvlak20202022QFSF$time_interval <- NA
for(i in 1:length(NadiaSvdH20202022QFSF$time_interval)){
  if(i == 1){
    Kraansvlak20202022QFSF$time_interval[i] <- NA
  }else{
    Kraansvlak20202022QFSF$time_interval[i] <- (Kraansvlak20202022QFSF$time_coded[i]) - (Kraansvlak20202022QFSF$time_coded[i-1])
  }
}





















