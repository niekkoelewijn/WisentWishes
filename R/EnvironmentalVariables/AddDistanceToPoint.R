# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to calculate and add the distance to specific land use types as an
### attribute to the points in the GPS track

# Create function to add distance attributes to GPS tracks
AddWaterDistance <- function(GPSTrackList, LanduseList, NameVec){
  
  # Make copy of GPSTrackList
  WaterDistanceTrackList <- GPSTrackList
  
  # Calculate distance to water for each point in each element in list
  for(i in seq_along(CleanedTrackList)){
    CleanedTrackList[i] %in% 16
    
    
  }
}



MaashorstWater <- MaashorstLandUse %in% 16
MaashorstWater[MaashorstWater < 1] <- NA
plot(MaashorstWater)
MaashorstWaterPolygons <- rasterToPolygons(MaashorstWater)
plot(MaashorstWaterPolygons, add=T)
plot(st_geometry(TransformPolygon(MaashorstStudyArea2016)), add = T)

MaashorstWaterPolygons <- st_as_sf(MaashorstWaterPolygons)
MaashorstStudyArea2016Sf <- st_as_sf(MaashorstStudyArea2016)
MaashorstStudyArea2016RDnewSf <- st_transform(MaashorstStudyArea2016Sf, st_crs(MaashorstWaterPolygons))

MaashorstStudyAreaWaterPolygons <- MaashorstWaterPolygons[MaashorstStudyArea2016RDnewSf, ]

plot(st_geometry(MaashorstStudyAreaWaterPolygons), xlim = st_bbox(MaashorstStudyArea2016RDnewSf)[c(1, 3)],
     ylim = st_bbox(MaashorstStudyArea2016RDnewSf)[c(2, 4)])
plot(st_geometry(MaashorstStudyArea2016RDnewSf), add = T)

MaashorstTrack1 <- read_csv("~/WisentWishes/MScThesisData/GPS location data/Step8Preprocess/MaashorstTrack1.csv")
MaashorstTrack1Sf <- st_as_sf(MaashorstTrack1, coords = c("X", "Y"), crs = 28992)
plot(st_geometry(MaashorstTrack1Sf), add = T)

MaashorstTrack1Sp <- as_Spatial(MaashorstTrack1Sf)
MaashorstStudyAreaWaterPolygonsSp <- as_Spatial(MaashorstStudyAreaWaterPolygons)
CRS(MaashorstTrack1Sp)
spTransform(MaashorstStudyAreaWaterPolygonsSp, )

DistanceToWater <- apply(gDistance(as_Spatial(MaashorstStudyAreaWaterPolygons), as_Spatial(MaashorstTrack1Sf), byid=TRUE), 1, min)
MaashorstTrack1 <- MaashorstTrack1 %>% 
  mutate(distance_to_water = DistanceToWater)
write_csv(MaashorstTrack1, "~/WisentWishes/MScThesisData/GPS location data/Old/MaashorstWithWaterAttribute.csv")


## Create forest raster
MaashorstForest <- MaashorstLandUse %in% c(11,12)
MaashorstForest[MaashorstForest < 1] <- NA
plot(MaashorstForest)

## Create roads raster
MaashorstRoads <- MaashorstLandUse %in% 25
MaashorstRoads[MaashorstRoads < 1] <- NA
plot(MaashorstRoads)