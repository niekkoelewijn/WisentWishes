# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script calculate the Jacobs index per land use class for all points. This
### is a exploratory step to see the general patterns. It is not completely
### accurate, as the habitat available difference per year in the Maashorst, 
### but this difference in availability cannot be accounted for in this study.


## Cluster all tracks into 1 tibble

# Create an empty tibble with 28 columns, the number of attributes that each
# point has
TrackPoints <- as_tibble(data.frame(matrix(NA, nrow = 1, ncol = 28)))

# Get column names from the track list of previous step
colnames(TrackPoints) <- colnames(WeatherTracks$KraansvlakTrack1)

# Iterate over elements of TrackFiles to read them and add them to TrackPoints
for(i in seq_along(TrackVec)){
  if(i %in% c(27:32)){
    next
  }else{
    TrackPoints <- TrackPoints %>% 
      add_row(read_csv(file = paste0(TrackPath, TrackVec[i]))) %>% 
      filter(!is.na(ID))
  }
  
}

# Get column names from the track list of previous step
colnames(TrackPoints) <- colnames(WeatherTracks$KraansvlakTrack1)

# Assign unique code to each row in the dataset
AllTrackPoints <- TrackPoints %>% 
  dplyr::rename(track_row_ID = ID) %>% 
  rowid_to_column("ID")


## Sum of landuse code values of all study areas 

# Get frequency tables of 4 study areas
FreqKraansvlak <- table(MaskedList$Kraansvlak[])
FreqVeluwe <- table(MaskedList$Veluwe[])
FreqSlikkenvdHeen <- table(MaskedList$SlikkenvdHeen[])
FreqMaashorst <- table(MaskedList$Maashorst20172021[])

# Create table with total frequencies
TotalFreq <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(TotalFreq) <- as.character(LUTLanduseClasses$landuse_code)

# Create table with proportion available
PropAvail <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(PropAvail) <- as.character(LUTLanduseClasses$landuse_code)

# Fill in total frequency table with proportion habitat available
for(i in seq_along(TotalFreq)){
  if(is.na(FreqKraansvlak[as.character(i)])){
    FreqKraansvlak[as.character(i)] <- 0
  }
  if(is.na(FreqVeluwe[as.character(i)])){
    FreqVeluwe[as.character(i)] <- 0
  }
  if(is.na(FreqSlikkenvdHeen[as.character(i)])){
    FreqSlikkenvdHeen[as.character(i)] <- 0
  }
  if(is.na(FreqMaashorst[as.character(i)])){
    FreqMaashorst[as.character(i)] <- 0
  }
  TotalFreq[i] <- FreqKraansvlak[as.character(i)] + FreqVeluwe[as.character(i)] + FreqSlikkenvdHeen[as.character(i)] + FreqMaashorst[as.character(i)]
}

# Calculate the proportion used from the total frequency of a class and the total of all classes
for(i in seq_along(PropAvail)){
  Prop <- as.numeric(TotalFreq[i]) / sum(TotalFreq)
  PropAvail[i] <- as.numeric(Prop)
}
colnames(PropAvail) <- as.character(LUTLanduseClasses$landuse_class)

## Calculate proportions of landuse class used
OverallClassUsed <- table(AllTrackPoints$landuse_code)
NumberOfPoints <- length(AllTrackPoints$landuse_code)

# Create table with proportion used per of landuse class
PropUsed <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(PropUsed) <- as.character(LUTLanduseClasses$landuse_class)

# Fill empty table PropUsed with proportion used per landuse class
for(i in seq_along(PropUsed)){
  if(is.na(OverallClassUsed[as.character(i)])){
    OverallClassUsed[as.character(i)] <- 0
  }
  PropUsed[1,i] <- OverallClassUsed[as.character(i)] / NumberOfPoints
}


## Calculate overall Jacobs index

# Create empty table to store result
JIOverall <- as_tibble(data.frame(matrix(data = NA, nrow = 1, ncol = 13)))
colnames(JIOverall) <- LUTLanduseClasses$landuse_class

# Iterate over elements of JIOverall
for(i in seq_along(JIOverall)){
  JIOverall[i] <- JacobsIndex(as.numeric(PropUsed[i]), as.numeric(PropAvail[i]))
}








