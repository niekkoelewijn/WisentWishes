# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to adapt LGN2020, the landuse map that I will use to determine
### in which landuse classes the points of the GPS tracks fall. LGN2020 will be
### cropped to the study area extents, reprojected to RDnew, and its values will
### be adapted.

### In the Maashorst study area, there has been a expansion of the bison reserve
### in Februari of 2022. LGN2020 is a landuse map that shows the landuse of the
### Netherlands in a 5x5 meter resolution, for the year 2020. The parts near the
### Maashorst study area that are part of the expansion zones of 2022, have 
### non-current LGN2020 values. The LGN2020 values suggest that in these areas,
### agricultural activities such as the growing of maize, potato or beets takes 
### place, while currently these areas are used as natural grasslands. I will 
### adapt these land use values, to increase the level of realism for the 
### analyses of the Maashorst tracks of 2022. Codes of maize, potato and beets
### are respectively 2, 3 and 4


## Load the LGN2020 dataset

# Paths to GPS files step 3
setwd("~/WisentWishes")
LandUsePath <- "~/WisentWishes/MScThesisData/EnvironmentalVariables/LGN2020/"
LandUseTIF <- raster("~/WisentWishes/MScThesisData/EnvironmentalVariables/LGN2020/LGN2020_5m.tif")


## Get bounding box of study areas with 100 m buffer zone

# Kraansvlak
KraansvlakBbox <- KraansvlakStudyArea %>% 
  TransformPolygon() %>% 
  st_bbox() %>% 
  unname()

KraansvlakXmin <- KraansvlakBbox[1] - 100
KraansvlakXmax <- KraansvlakBbox[3] + 100
KraansvlakYmin <- KraansvlakBbox[2] - 100
KraansvlakYmax <- KraansvlakBbox[4] + 100

KraansvlakExtent <- raster::extent(KraansvlakXmin, KraansvlakXmax, KraansvlakYmin, KraansvlakYmax)

# Maashorst
MaashorstBbox <- MaashorstStudyArea2016 %>% 
  TransformPolygon() %>% 
  st_bbox() %>% 
  unname()

MaashorstXmin <- MaashorstBbox[1] - 100
MaashorstXmax <- MaashorstBbox[3] + 100
MaashorstYmin <- MaashorstBbox[2] - 100
MaashorstYmax <- MaashorstBbox[4] + 100

MaashorstExtent <- raster::extent(MaashorstXmin, MaashorstXmax, MaashorstYmin, MaashorstYmax)

# Slikken vd Heen
SlikkenvdHeenBbox <- SlikkenvdHeenStudyArea %>% 
  TransformPolygon() %>% 
  st_bbox() %>% 
  unname()

SlikkenvdHeenXmin <- SlikkenvdHeenBbox[1] - 100
SlikkenvdHeenXmax <- SlikkenvdHeenBbox[3] + 100
SlikkenvdHeenYmin <- SlikkenvdHeenBbox[2] - 100
SlikkenvdHeenYmax <- SlikkenvdHeenBbox[4] + 100

SlikkenvdHeenExtent <- raster::extent(SlikkenvdHeenXmin, SlikkenvdHeenXmax, SlikkenvdHeenYmin, SlikkenvdHeenYmax)

# Veluwe
VeluweBbox <- VeluweStudyArea %>% 
  TransformPolygon() %>% 
  st_bbox() %>% 
  unname()

VeluweXmin <- VeluweBbox[1] - 100
VeluweXmax <- VeluweBbox[3] + 100
VeluweYmin <- VeluweBbox[2] - 100
VeluweYmax <- VeluweBbox[4] + 100

VeluweExtent <- raster::extent(VeluweXmin, VeluweXmax, VeluweYmin, VeluweYmax)


## Crop LandUseTIF to study area extents and reproject to RD new

# RD new proj
RDnewProj <- "+init=epsg:28992"

# Kraansvlak
KraansvlakLandUse <- LandUseTIF %>% 
  raster::crop(KraansvlakExtent) %>% 
  projectRaster(crs = crs(RDnewProj))

# Maashorst
MaashorstLandUse <- LandUseTIF %>% 
  raster::crop(MaashorstExtent) %>% 
  projectRaster(crs = crs(RDnewProj))

# Slikken vd Heen
SlikkenvdHeenLandUse <- LandUseTIF %>% 
  raster::crop(SlikkenvdHeenExtent) %>% 
  projectRaster(crs = crs(RDnewProj))

# Veluwe
VeluweLandUse <- LandUseTIF %>% 
  raster::crop(VeluweExtent) %>% 
  projectRaster(crs = crs(RDnewProj))

# Remove the enormous LandUseTIF from the global environment 
remove(LandUseTIF)

# List the land use maps of the study areas
LandUseList = list(KraansvlakLandUse, MaashorstLandUse, SlikkenvdHeenLandUse, VeluweLandUse)
names(LandUseList) <- c("KraansvlakLandUse", "MaashorstLandUse", "SlikkenvdHeenLandUse", "VeluweLandUse")


## Create a look up table that shows the meaning of the LGN2020 raster values
value <- c(1:6, 8:12, 16:20, 22:28, 30:43, 45:47, 61, 62, 321:323, 331:333)
DutchLandUseClass <- c("agrarisch gras", "maïs", "aardappelen", "bieten", "granen",
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
                      "bos in moerasgebied", "natuurgraslanden", "gras in kustgebied",
                      "overig gras", "boomkwekerijen", "fruitkwekerijen", 
                      "struikvegetatie in hoogveengebied (laag)", "struikvegetatie in moerasgebied (laag)",
                      "overige struikvegetatie (laag)", "struikvegetatie in hoogveengebied (hoog)", 
                      "struikvegetatie in moerasgebied (hoog)", "overige struikvegetatie (hoog)")
LUTLGN <- data.frame(value, DutchLandUseClass)
LUTLGN <- as_tibble(LUTLGN)
names(LUTLGN) <- c("landuse_code", "Dutch_landuse_name")

  
## Determine the class of each landuse code

# Create landuse classes
LUTLGN <- LUTLGN %>% 
  
  # Create new landuse class column and add it to LUTLGN
  mutate(landuse_class = NA)

# Grassland
grasscodes <- c(1, 28, 32, 45, 46, 47)
LUTLGN[LUTLGN$landuse_code %in% grasscodes, ]$landuse_class = "grassland"

# Arable land
arablelandcodes <- c(2, 3, 4, 5, 6, 8, 9, 10, 61, 62)
LUTLGN[LUTLGN$landuse_code %in% arablelandcodes, ]$landuse_class = "arable land"

# Deciduous forest 
deciduous_codes <- c(11)
LUTLGN[LUTLGN$landuse_code %in% deciduous_codes, ]$landuse_class = "deciduous forest"

# Coniferous forest
coniferous_codes <- c(12)
LUTLGN[LUTLGN$landuse_code %in% coniferous_codes, ]$landuse_class = "coniferous forest"

# Fresh water
fresh_water_codes <- c(16)
LUTLGN[LUTLGN$landuse_code %in% fresh_water_codes, ]$landuse_class = "fresh water"

# Salt water
salt_water_codes <- c(17)
LUTLGN[LUTLGN$landuse_code %in% salt_water_codes, ]$landuse_class = "salt water"

# Build-up area
build_up_codes <- c(18, 19, 20, 22, 23, 24, 26)
LUTLGN[LUTLGN$landuse_code %in% build_up_codes, ]$landuse_class = "build-up area"

# Roads
road_codes <- c(25)
LUTLGN[LUTLGN$landuse_code %in% road_codes, ]$landuse_class = "road"

# Swamp
swamp_codes <- c(30, 39, 40, 41, 42, 43, 321, 322, 331, 332)
LUTLGN[LUTLGN$landuse_code %in% swamp_codes, ]$landuse_class = "swamp"

# Bare soil
bare_soil_codes <- c(27, 31, 35)
LUTLGN[LUTLGN$landuse_code %in% bare_soil_codes, ]$landuse_class = "bare soil"

# Heather
heather_codes <- c(34, 36)
LUTLGN[LUTLGN$landuse_code %in% heather_codes, ]$landuse_class = "heathland"

# Grassy heather
grassy_heather_codes <- c(37, 38)
LUTLGN[LUTLGN$landuse_code %in% grassy_heather_codes, ]$landuse_class = "grassy heathland"

# Shrubland
shrubland_codes <- c(33, 323, 333)
LUTLGN[LUTLGN$landuse_code %in% shrubland_codes, ]$landuse_class = "shrubland"

# Add landuse class integer code to LUTLGN
LUTLGN <- LUTLGN %>%
  mutate(landuse_class_code = as.numeric(factor(LUTLGN$landuse_class, levels=unique(LUTLGN$landuse_class))))

## Adapt landuse code of tifs to the user defined landuse classes

# Create general function for the adaptations
UpdateClasses <- function(LandUseList, LookUpTable){
  
  # Loop over elements of land use list
  for(i in seq_along(LandUseList)){
    
    # Create numeric representation of the factor categories in the look up table
    NewClasses <- factor(LookUpTable$landuse_class, levels=unique(LookUpTable$landuse_class)) %>% 
                    as.integer()
    
    # Get values of the land use class and translate them to factors
    LandUseList[[i]][] <- as.integer(factor(LandUseList[[i]][], levels=unique(LookUpTable$landuse_code)))
    
    # Reclassify the values of the land use raster
    LandUseList[[i]][] <- NewClasses[LandUseList[[i]][]]
  }
  
  # Return the adapted land use raster
  return(LandUseList)
}

# Call UpdateClasses
LandUseList <- UpdateClasses(LandUseList, LUTLGN)

# Replace arable land use codes with the code of nature grass for the Maashorst
LandUseList$MaashorstLandUse[LandUseList$MaashorstLandUse == 2] <- 1


## Mask of the rasters with the study areas and create frequency tables per landuse class

# Create general function
MaskLandUse <- function(LandUseList, StudyAreaList){
  
  # Create masked landuse list
  MaskedList <- list()
  
  # Loop over elements in study area vector
  for(i in seq_along(StudyAreaList)){
    
    # Get Spatial Polygon feature in RDnew of study area
    StudyAreaList[[i]] <- spTransform(StudyAreaList[[i]], CRSobj = crs(LandUseList$KraansvlakLandUse))
    
    # Mask the landuse maps to the study areas 
    if(i == 1){
      MaskedList[[i]] <- raster::mask(x = LandUseList$KraansvlakLandUse, mask = StudyAreaList[[i]])
    }else if(i %in% 2:4){
      MaskedList[[i]] <- raster::mask(x = LandUseList$MaashorstLandUse, mask = StudyAreaList[[i]])
    }else if(i %in% 5:6){
      MaskedList[[i]] <- raster::mask(x = LandUseList$SlikkenvdHeenLandUse, mask = StudyAreaList[[i]])
    }else{
      MaskedList[[i]] <- raster::mask(x = LandUseList$VeluweLandUse, mask = StudyAreaList[[i]])
    }
  }
  
  # Return the list with masked land use maps
  return(MaskedList)
}

# Create study area vector
StudyAreaList <-  c(KraansvlakStudyArea, MaashorstStudyArea2016, MaashorstStudyArea20172021,
                   MaashorstStudyArea2022, SlikkenvdHeenHabituateArea,
                   SlikkenvdHeenStudyArea, VeluweHabituateArea, VeluweStudyArea)

# Call MaskLandUse function
MaskedList <- MaskLandUse(LandUseList, StudyAreaList)

# Name MaskedLandUse list elements
names(MaskedList) <- c("Kraansvlak", "Maashorst2016", "Maashorst20172021",
                           "Maashorst2022", "SlikkenvdHeenHabituate",
                           "SlikkenvdHeen", "VeluweHabituate", "Veluwe")


## Write elements of MaskedList to files

# Create path
path <- "~/WisentWishes/MScThesisData/EnvironmentalVariables/LGN2020/"

# Loop to write files
for(i in seq_along(MaskedList)){
  writeRaster(MaskedList[[i]], filename = paste0(path, names(MaskedList)[i],".tfw"), 
              options = c("TFW=YES"), overwrite = T)
}
