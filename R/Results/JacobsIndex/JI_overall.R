# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script calculate the Jacobs index per land use class, per study area

## Determining the used habitat r

# Create general funcion to determine r per habitat type per study area
getR <- function(StudyAreaList, PointsList){
  
  # Create result table
  StudyAreas <- names(StudyAreaList)
  LanduseClasses <- LUTLanduseClasses$landuse_class
  ResultRdf <- data.frame(matrix(NA, nrow = length(StudyAreas), ncol = length(LanduseClasses)+1))
  names(ResultRdf) <- c("Study area", LanduseClasses)
  ResultR <- as_tibble(ResultRdf)
  ResultR[,1] <- StudyAreas
  
  # Iterate over elements of point list
  for(i in seq_along(PointsList)){
    
    # Get frequency of points per calss
    FreqPerClass <- table(PointsList[[i]]$landuse_code)
    
    # Total number of points 
    TotalNumberPoints <- sum(table(PointsList[[i]]$landuse_code))
    
    # Vector with the landuse class codes
    classes <- LUTLanduseClasses$landuse_code
    
    # Create vector of proportion per landuse class for this for study area i
    ProportionHabitatUsed <- c()
    
    # Fill vector with proportions per landuse class with data
    for(j in seq_along(classes)){
      if(j %in% as.numeric(names(FreqPerClass))){
        ProportionHabitatUsed[j] <- FreqPerClass[which(as.numeric(names(FreqPerClass)) == j)] / TotalNumberPoints
      }else{
        ProportionHabitatUsed[j] <- 0
      }
    }
    
    # Fill in the proportion per habitat class in the result table
    for(k in 2:14){
      ResultR[i,][k] <- ProportionHabitatUsed[k-1]
    }
  }
  
  return(ResultR)
}

# Call getR
ProportionUsedPerClass <- getR(MaskedList, PointsList)

# Create Jacobs Index function
JacobsIndex <- function(r, p){
  D <-  (r - p)/(r + p - 2*r*p)
  return(D)
}

# Create general funcion to determine D per habitat type per study area
getD <- function(StudyAreaList, R, P){
  
  # Create result table
  StudyAreas <- names(StudyAreaList)
  LanduseClasses <- LUTLanduseClasses$landuse_class
  ResultDdf <- data.frame(matrix(NA, nrow = length(StudyAreas), ncol = length(LanduseClasses)+1))
  names(ResultDdf) <- c("Study area", LanduseClasses)
  ResultD <- as_tibble(ResultDdf)
  ResultD[,1] <- StudyAreas
  
  for(i in 1:nrow(ResultD)){
    for(j in 2:14){
      ResultD[i,j] <- JacobsIndex(r = R[i,j], p= P[i,j])
    }
  }
  
  return(ResultD)
}

# Call get D
JacobsIndexPerStudyArea <- getD(MaskedList, ProportionUsedPerClass, ProportionAvailablePerClass)
