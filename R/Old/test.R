# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: Henjo de Knegt
# Year: 2022

# Load data of bull on the Maashorst
points <- read.csv(file = "positions_deployment_37.csv")

# Create Unix epoch timestemp
points$UnixEpoch <- NA

for (i in 1:length(points$UnixEpoch)){
  points$UnixEpoch[i] <- as.numeric(as.POSIXct(points$time[i], "%Y-%m-%d %H:%M:%OS", tz = ""))
}

# Remove first 4 points and 111th point
points <- points[-c(1:4),]
points <- points[-c(107,111),]


# Get x, y data
library(rgdal)
points.dec = SpatialPoints(cbind(points$lng, points$lat, points$UnixEpoch), proj4string = CRS("+proj=longlat"))

points.UTM <- spTransform(points.dec, CRS("+init=epsg:32631"))
points.UTM <- data.frame(points.UTM)
names(points.UTM) <- c("x", "y", "time")

# Calculating the speed
points.UTM$speed <- atlastools::atl_get_speed(data = points.UTM, x = "x", y = "y", time = "time", type = "in")
points.UTM$speed <- points.UTM$speed * 3.6

# Calculating turning anlge
points.UTM$angle <- atlastools::atl_turning_angle(data = points.UTM, x = "x", y = "y", time = "time")

# Filtering data
filtered_points <- atlastools::atl_filter_covariates()

# Download administrative map of the Netherlands
library(raster)
library(sf)
NethAdm <- raster::getData(name = "GADM", country = "NLD", level = 2)
NethAdmSf <- sf::st_as_sf(NethAdm)

# Get Barneveld, Apeldoorn and Ede
Barneveld <- NethAdmSf[NethAdmSf$NAME_2 == "Barneveld",]
Apeldoorn <- NethAdmSf[NethAdmSf$NAME_2 == "Apeldoorn",]
Ede <- NethAdmSf[NethAdmSf$NAME_2 == "Ede",]

# Make one area out of it
RadioKootwijkArea <- st_union(Barneveld, Apeldoorn)
RadioKootwijkArea <- st_union(RadioKootwijkArea, Ede)
crs
plot(RadioKootwijkArea, border = "green", col = "transparent", ldw = 3)

# Get Veluwe point data and reproject it
setwd("~/Downloads/MScThesisData/GPS location data/Veluwe/")
VeluwePoints <- read.csv("VeluweData2016-2020.csv")
VeluwePointsSf <- sf::st_as_sf(VeluwePoints, coords = c("Longitude", "Latitude"), crs = 4326)
targetCRS <- sf::st_crs(RadioKootwijkArea)
VeluwePointsSf <- sf::st_transform(VeluwePointsSf, targetCRS)

# Clip data to studyarea
##VeluwePointsSf_clip <- VeluwePointsSf[RadioKootwijkArea, ]
wgs1984.proj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
VeluwePointsSp <- sp::SpatialPoints(coords = VeluwePoints[, c(5,4)], 
                                    proj4string = wgs1984.proj)
RadioKootwijkAreaSp <- sf::as_Spatial(RadioKootwijkArea$geometry)
RadioKootwijkAreaSp <- spTransform(RadioKootwijkAreaSp, wgs1984.proj)
VeluwePoints_clip <- VeluwePointsSp[RadioKootwijkAreaSp, ]
  
# Check attributes of VeluwePointsSp
VeluwePointsSf_clip <- sf::st_as_sf(VeluwePoints_clip)

# Digitalize LUT
value <- c(1:6, 8:12, 16:20, 22:28, 30:43, 45:47, 61, 62, 321:323, 331:333)
LandUseClass <- c("agrarisch gras", "maïs", "aardappelen", "bieten", "granen",
                  "overige landbouwgewassen", "glastuinbouw", "boomgaarden",
                  "bloembollen", "loofbos", "naaldbos", "zoet water", "zout water",
                  "bebouwing in primair bebouwd gebied", "bebouwing in secundair bebouwd gebied",
                  "bos in primair bebouwd gebied", "bos in secundair bebouwd gebied",
                  "gras in primair bebouwd gebied", "kale grond in bebouwd gebied",
                  "wegen en spoorwegen", "bebouwing in buitengebied", 
                  "overig grondgebruik in buitengebied", "gras in secundair bebouwd gebied",
                  "kwelders", "open zand in kustgebied", "duinen met lage vegetatie",
                  "duinen met hoge vegetatie", "duinheide", "open stuifzand en/of rivierzand",
                  "heide", "matig vergraste heide", "sterk vergraste heide", "hoogveen",
                  "bos in hoogveengebied", "overige moerasvegetatie", "rietvegetie",
                  "bos inn moerasgebied", "natuurgraslanden", "gras in kustgebied",
                  "overig gras", "boomkwekerijen", "fruitkwekerijen", 
                  "struikvegetatie in hoogveengebied (laag)", "struikvegetatie in moerasgebied (laag)",
                  "overige struikvegetatie (laag)", "struikvegetatie in hoogveengebied (hoog)", 
                  "struikvegetatie in moerasgebied (hoog)", "overige struikvegetatie (hoog)")
LUTLNG <- data.frame(value, LandUseClass)

# Get Kraansvlak location data
KraansvlakPoints <- read.csv("MScThesisData/GPS location data/Kraansvlak/Kraansvlak.csv")

# Get temperature attribute from most recent port 12 message
KraansvlakPointsCopy <- KraansvlakPoints
for(i in 1:length(KraansvlakPointsCopy[,"temperature"])){
  if(KraansvlakPointsCopy[i,"temperature"] == "\\N"){
    KraansvlakPointsCopy[i,"temperature"] = KraansvlakPointsCopy[i-1,"temperature"]
}
}

# Get port 1 only
KraansvlakPointsOnly1 <- KraansvlakPointsCopy[KraansvlakPointsCopy$port == 1, ]

# Omit rows with hdop values greater then 10
KraansvlakPointsAccurate <- KraansvlakPointsOnly1[as.numeric(KraansvlakPointsOnly1$hdop) < 10,]

# Remove unnessesary attributes
KraansvlakPointsCleaned <- KraansvlakPointsOnly1[, c(1, 3, 31, 30, 21, 22, 12, 25)]

# Create date attributes
KraansvlakPointsCleaned$season <- NA
KraansvlakPointsCleaned$date <- as.Date(KraansvlakPointsCleaned$time_of_fix_decoded, "%d/%m/%Y")
KraansvlakPointsCleaned$time <- as.POSIXct(KraansvlakPointsCleaned$time_of_fix_decoded, format="%d/%m/%Y %H:%M",tz=Sys.timezone())
KraansvlakPointsCleaned$weekday <-  weekdays(KraansvlakPointsCleaned$date, abbreviate = F)

