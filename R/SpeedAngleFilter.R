# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to filter out unrealistic speeds and turning angles to filter out unrealistic movement / spikes ###

## First assign attributes to all GPS datasets

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


## Now assign speed in, speed out and angle attributes to all GPS datasets

# Veluwe
SharaVeluwe20162020QF <- na.omit(SharaVeluwe20162020QF)
SharaVeluwe20162020QF$speed_in <- atl_get_speed(SharaVeluwe20162020QF, x = "lng", y = "lat",
                                                time = "time_coded", type = "in")
SharaVeluwe20162020QF$speed_out <- atl_get_speed(SharaVeluwe20162020QF, x = "lng", y = "lat",
                                                time = "time_coded", type = "out")
SharaVeluwe20162020QF$turning_angle <- atl_turning_angle(SharaVeluwe20162020QF, x = "lng", y = "lat",
                                                         time = "time_coded")

# Maashorst
DeliaMaashorst2016QF$speed_in <- atl_get_speed(DeliaMaashorst2016QF, x = "lng", y = "lat",
                                                time = "time_coded", type = "in")
DeliaMaashorst2016QF$speed_out <- atl_get_speed(DeliaMaashorst2016QF, x = "lng", y = "lat",
                                                 time = "time_coded", type = "out")
DeliaMaashorst2016QF$turning_angle <- atl_turning_angle(DeliaMaashorst2016QF, x = "lng", y = "lat",
                                                         time = "time_coded")

