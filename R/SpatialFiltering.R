# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022


### Script to spatially filter GPS dataset based on study area polygons

## Load studyarea shapefiles in R

# Paths to GPS input files
setwd("~/WisentWishes")
StudyAreas <- "~/WisentWishes/MScThesisData/GISFilesAreas/StudyAreas/"
StudyAreasVec <- list.files(path = StudyAreas, pattern = ".shp")

# Load study areas
KraansvlakStudyArea <- readOGR(paste0(StudyAreas, StudyAreasVec[1]))
MaashorstStudyArea2016 <- readOGR(paste0(StudyAreas, StudyAreasVec[2]))
MaashorstStudyArea20172021 <- readOGR(paste0(StudyAreas, StudyAreasVec[3]))
MaashorstStudyArea2022 <- readOGR(paste0(StudyAreas, StudyAreasVec[4]))
SlikkenvdHeenHabituateArea <- readOGR(paste0(StudyAreas, StudyAreasVec[5]))
SlikkenvdHeenStudyArea <- readOGR(paste0(StudyAreas, StudyAreasVec[6]))
VeluweHabituateArea  <- readOGR(paste0(StudyAreas, StudyAreasVec[7]))
VeluweStudyArea <- readOGR(paste0(StudyAreas, StudyAreasVec[8]))


# Put study areas in a list
StudyAreaList <- list(KraansvlakStudyArea, MaashorstStudyArea2016, 
                      MaashorstStudyArea20172021, MaashorstStudyArea2022,
                      SlikkenvdHeenHabituateArea, SlikkenvdHeenStudyArea, 
                      VeluweHabituateArea, VeluweStudyArea)

# Define function to transform study area datasets
TransformPolygon <- function(polygon){
  
  # Transform polygons in piped structure
  TransformedPolygon <- polygon %>% 
    
    # Change class to sf
    st_as_sf() %>% 
    
    # Transform polygons to RDnew
    st_transform(st_crs(28992))
  
  return(TransformedPolygon)
}

# Apply TransformPolygon to all study areas
StudyAreas <- lapply(StudyAreaList, TransformPolygon) 

# Give elements in StudyAreas list appropriate names
names(StudyAreas) <- StudyAreasVec


## Now spatially filter the cleaned GPS datasets of step 1

# Paths to GPS files step 1
setwd("~/WisentWishes")
GPSStep1Path <- "~/WisentWishes/MScThesisData/GPS location data/Step1Preprocess/"
GPSStep1Vec <- list.files(path = GPSStep1Path)

# Load GPS tables as tibbles
Caliope <- read_csv(paste0(GPSStep1Path, GPSStep1Vec[1]))
Delia <- read_csv(paste0(GPSStep1Path, GPSStep1Vec[2]))
Everest <- read_csv(paste0(GPSStep1Path, GPSStep1Vec[3]))
Kraansvlak <- read_csv(paste0(GPSStep1Path, GPSStep1Vec[4]))
Krayla <- read_csv(paste0(GPSStep1Path, GPSStep1Vec[5]))
Kroosja <- read_csv(paste0(GPSStep1Path, GPSStep1Vec[6]))
Maaike <- read_csv(paste0(GPSStep1Path, GPSStep1Vec[7]))
Nadia <- read_csv(paste0(GPSStep1Path, GPSStep1Vec[8]))
Nevaya <- read_csv(paste0(GPSStep1Path, GPSStep1Vec[9]))
Shara <- read_csv(paste0(GPSStep1Path, GPSStep1Vec[10]))
Veluwe <- read_csv(paste0(GPSStep1Path, GPSStep1Vec[11]))

# Define function to transform GPS tables
SpatialyFilter <- function(GPSfile, studyarea){
  
  # Transform GPS files in piped structure
  TransformedTable <- GPSfile %>% 
    
    # Transform to class sf
    st_as_sf(coords = c("lng", "lat"), crs = st_crs(4326)) %>% 
    
    # Transfrom WGS84 to RD new
    st_transform(crs = st_crs(28992))
    
  # Get x and y column
  TransformedTable$X <- st_coordinates(TransformedTable)[,1]
  TransformedTable$Y <- st_coordinates(TransformedTable)[,2]
  
  # Spatially filter with alt_filterbounds
  TransformedTable <- atl_filter_bounds(TransformedTable , x = "X", y = "Y", 
                                        sf_polygon = studyarea, remove_inside = F)
  
  # Return to sf
  TransformedTable <- st_as_sf(TransformedTable)
  
  return(TransformedTable)
}

# Spatially filter the GPS tables with the study areas
EverestSpaFil <- SpatialyFilter(Everest, StudyAreas[["MaashorstStudyArea2017-2021.shp"]])
CaliopeSpaFil <- SpatialyFilter(Caliope, StudyAreas[["SlikkenvdHeenStudyArea.shp"]])
DeliaSpaFil <- SpatialyFilter(Delia, StudyAreas[["MaashorstStudyArea2016.shp"]])
KraansvlakSpaFil <- SpatialyFilter(Kraansvlak, StudyAreas[["KraansvlakStudyArea.shp"]])
KraylaSpaFil <- SpatialyFilter(Krayla, StudyAreas[["MaashorstStudyArea2016.shp"]])
KroosjaSpaFil <- SpatialyFilter(Kroosja, StudyAreas[["MaashorstStudyArea2016.shp"]])
MaaikeSpaFil <- SpatialyFilter(Maaike, StudyAreas[["MaashorstStudyArea2017-2021.shp"]])
NadiaSpaFil <- SpatialyFilter(Nadia, StudyAreas[["SlikkenvdHeenStudyArea.shp"]])
NevayaSpaFil <- SpatialyFilter(Nevaya, StudyAreas[["MaashorstStudyArea2022.shp"]])
SharaSpaFil <- SpatialyFilter(Shara, StudyAreas[["VeluweStudyArea.shp"]])
VeluweSpaFil <- SpatialyFilter(Veluwe, StudyAreas[["VeluweStudyArea.shp"]])


## Write the elements of the list to a file

# Create path
path <- "MScThesisData/GPS location data/Step2Preprocess/"

# Create directory
if(!dir.exists(path)){
  dir.create(path)
}

# Create list of spatially filtered datasets
Step2ProcessingList <- list( 
  EverestSpaFil, CaliopeSpaFil, DeliaSpaFil, KraansvlakSpaFil, KraylaSpaFil,
  KroosjaSpaFil, MaaikeSpaFil, NadiaSpaFil, NevayaSpaFil, SharaSpaFil, VeluweSpaFil
)

# Create vector of file names
NameVec <- c("EverestStep2", "CaliopeStep2", "DeliaStep2", "KraansvlakStep2", 
             "KraylaStep2", "KroosjaStep2", "MaaikeStep2", "NadiaStep2", "NevayaStep2",
             "SharaStep2", "VeluweStep2")

# Write csv's
for(i in seq_along(Step2ProcessingList)){
  write_csv(Step2ProcessingList[[i]], file = paste0(path, NameVec[i], ".csv"))
} 
