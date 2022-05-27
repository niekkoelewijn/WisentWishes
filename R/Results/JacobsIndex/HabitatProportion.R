# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to calculate Jacob's index for habitat preference under different
### conditions

### The formula to calculate the index is D = (r - p)/(r + p – 2rp) in which 
### D is a value between -1 and 1 indicating the habitat preference, r is the 
### proportion of habitat used and p is the proportion of habitat available.


## Determining p for the different habitat types

# Create general funcion to determine p per habitat type per study area
getP <- function(StudyAreaList){
  
  # Create result table
  StudyAreas <- names(StudyAreaList)
  LanduseClasses <- LUTLanduseClasses$landuse_class
  ResultPdf <- data.frame(matrix(NA, nrow = length(StudyAreas), ncol = length(LanduseClasses)+1))
  names(ResultPdf) <- c("Study area", LanduseClasses)
  ResultP <- as_tibble(ResultPdf)
  ResultP[,1] <- StudyAreas
  
  # Iterate over elements of study area list
  for(i in seq_along(StudyAreaList)){
    
    ## Determine total number of cells in the study area
    
    # Frequency table per class
    FreqPerClass <- freq(StudyAreaList[[i]])
    
    # If freq per class <= 10, remove that class from analysis
    for(j in 1:nrow(FreqPerClass)){
      if(!is.na(FreqPerClass[,2][j])){
        if(FreqPerClass[,2][j] <= 10){
          FreqPerClass <- FreqPerClass[-j,]
        }
      }
    }
    
    # Sum of the count per class, without the NA values 
    TotalCells <- sum(FreqPerClass[,2]) - FreqPerClass[,2][nrow(FreqPerClass)]
    
    # Vector with the landuse class codes
    classes <- LUTLanduseClasses$landuse_code
    
    # Create vector of proportion per landuse class for this for study area i
    ProportionPerClass <- c()
    
    # Fill vector with proportions per landuse class with data
    for(j in seq_along(classes)){
      if(j %in% FreqPerClass[,1]){
        ProportionPerClass[j] <- FreqPerClass[,2][which(FreqPerClass[,1] == j)] / TotalCells
      }else{
      ProportionPerClass[j] <- 0
      }
    }
    
    # Fill in the proportion per habitat class in the result table
    for(k in 2:14){
      ResultP[i,][k] <- ProportionPerClass[k-1]
    }
  }
  
  # Return result table with p, the proportion of habitat available 
  return(ResultP)
}

# Call getP
ProportionAvailablePerClass <- getP(MaskedList)


## Determining r for the different habitat types
FreqPerClass <- table(LanduseTracks$VeluweTrack9$landuse_class)
TotalPoints <- nrow(LanduseTracks$VeluweTrack9)
TotalPoints <- sum(FreqPerClass)

