# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to determine the relationship between the JI of a landuse class and 
### the proportion of its availability.

# Create table with 9 rows, and 11 columns
JI_PropAvail <- as_tibble(data.frame(matrix(data = NA, nrow = 9, ncol = 20)))

# The columns have information of the proportion availabilty of each landuse type
# and the jacob index that corresponds to that class
colnames(JI_PropAvail) <- c("PA grass", "JI grass", "PA deciduous forest", 
                            "JI deciduous forest", "PA coniferous forest", 
                            "JI coniferous forest", "PA fresh water",
                            "JI fresh water", "PA road", "JI road", 
                            "PA bare soil", "JI bare soil", 
                            "PA swamp", "JI swamp", "PA shrubland",
                            "JI shrubland", "PA heathland", "JI heathland", 
                            "PA grassy heathland", "JI grassy heathland") 

# Add rownames to the relation table
rownames(JI_PropAvail) <- c(JacobsIndexPerStudyArea$`Study area`, "Overall")

# Fill the tibble with values
for(i in seq_along(rownames(JI_PropAvail))){
  
  ## Grassland
  
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,1] <- PropAvail$grassland
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,1] <- ProportionAvailablePerClass$grassland[i]
  }
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,2] <- JIOverall$grassland
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,2] <- JacobsIndexPerStudyArea$grassland[i]
  }
  
  ## Deciduous forest
  
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,3] <- PropAvail$`deciduous forest`
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,3] <- ProportionAvailablePerClass$`deciduous forest`[i]
  }
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,4] <- JIOverall$`deciduous forest`
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,4] <- JacobsIndexPerStudyArea$`deciduous forest`[i]
  }
  
  ## Coniferous forest
  
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,5] <- PropAvail$`coniferous forest`
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,5] <- ProportionAvailablePerClass$`coniferous forest`[i]
  }
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,6] <- JIOverall$`coniferous forest`
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,6] <- JacobsIndexPerStudyArea$`coniferous forest`[i]
  }
  
  ## Fresh water
  
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,7] <- PropAvail$`fresh water`
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,7] <- ProportionAvailablePerClass$`fresh water`[i]
  }
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,8] <- JIOverall$`fresh water`
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,8] <- JacobsIndexPerStudyArea$`fresh water`[i]
  }
  
  ## Road
  
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,9] <- PropAvail$road
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,9] <- ProportionAvailablePerClass$road[i]
  }
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,10] <- JIOverall$road
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,10] <- JacobsIndexPerStudyArea$road[i]
  }
  
  ## Bare soil
  
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,11] <- PropAvail$`bare soil`
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,11] <- ProportionAvailablePerClass$`bare soil`[i]
  }
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,12] <- JIOverall$`bare soil`
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,12] <- JacobsIndexPerStudyArea$`bare soil`[i]
  }
  
  ## Swamp
  
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,13] <- PropAvail$swamp
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,13] <- ProportionAvailablePerClass$swamp[i]
  }
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,14] <- JIOverall$swamp
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,14] <- JacobsIndexPerStudyArea$swamp[i]
  }
  
  ## Shrubland
  
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,15] <- PropAvail$shrubland
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,15] <- ProportionAvailablePerClass$shrubland[i]
  }
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,16] <- JIOverall$shrubland
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,16] <- JacobsIndexPerStudyArea$shrubland[i]
  }
  
  ## Heathland
  
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,17] <- PropAvail$heathland
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,17] <- ProportionAvailablePerClass$heathland[i]
  }
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,18] <- JIOverall$heathland
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,18] <- JacobsIndexPerStudyArea$heathland[i]
  }
  
  ## Grassy `grassy heathland`
  
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,19] <- PropAvail$`grassy heathland`
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,19] <- ProportionAvailablePerClass$`grassy heathland`[i]
  }
  # The 9th row is the overall Jacobs index, produced in step 4 of results
  if(i == 9){
    JI_PropAvail[i,20] <- JIOverall$`grassy heathland`
  }
  # All other values come from the jacob indexes per study area, produced in 
  # step 3 of the results
  else{
    JI_PropAvail[i,20] <- JacobsIndexPerStudyArea$`grassy heathland`[i]
  }
  
}

# Add rownames again
rownames(JI_PropAvail) <- c(JacobsIndexPerStudyArea$`Study area`, "Overall")








