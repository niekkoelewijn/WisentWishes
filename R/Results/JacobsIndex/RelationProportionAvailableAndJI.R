# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script determine the relationship between the JI of a landuse class and the 
### proportion of its availability.

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


## Plot relations

# Grass
plot(x = JI_PropAvail$`PA grass`, y = JI_PropAvail$`JI grass`,
     xlab = "Proportion available", ylab = "Jacobs index", 
     ylim = c(-1, 1),
     main = "Relation Jacobs index and availability of grass",
     cex.main = 1)
text(JI_PropAvail$`PA grass`, JI_PropAvail$`JI grass`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Deciduous forest
plot(x = JI_PropAvail$`PA deciduous forest`, y = JI_PropAvail$`JI deciduous forest`,
     xlab = "Proportion available", ylab = "Jacobs index", 
     ylim = c(-1, 1),
     main = "Relation Jacobs index and availability of deciduous forest",
     cex.main = 1)
text(JI_PropAvail$`PA deciduous forest`, JI_PropAvail$`JI deciduous forest`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Coniferous forest
plot(x = JI_PropAvail$`PA coniferous forest`, y = JI_PropAvail$`JI coniferous forest`,
     xlab = "Proportion available", ylab = "Jacobs index", 
     ylim = c(-1, 1),
     main = "Relation Jacobs index and availability of coniferous forest",
     cex.main = 1)
text(JI_PropAvail$`PA coniferous forest`, JI_PropAvail$`JI coniferous forest`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Fresh water
plot(x = JI_PropAvail$`PA fresh water`, y = JI_PropAvail$`JI fresh water`,
     ylim = c(-1, 1),
     xlab = "Proportion available", ylab = "Jacobs index", 
     main = "Relation Jacobs index and availability of fresh water",
     cex.main = 1)
text(JI_PropAvail$`PA fresh water`, JI_PropAvail$`JI fresh water`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Road
plot(x = JI_PropAvail$`PA road`, y = JI_PropAvail$`JI road`,
     ylim = c(-1, 1),
     xlab = "Proportion available", ylab = "Jacobs index", 
     main = "Relation Jacobs index and availability of road",
     cex.main = 1)
text(JI_PropAvail$`PA road`, JI_PropAvail$`JI road`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Bare soil
plot(x = JI_PropAvail$`PA bare soil`, y = JI_PropAvail$`JI bare soil`,
     ylim = c(-1, 1),
     xlab = "Proportion available", ylab = "Jacobs index", 
     main = "Relation Jacobs index and availability of bare soil",
     cex.main = 1)
text(JI_PropAvail$`PA bare soil`, JI_PropAvail$`JI bare soil`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Swamp
plot(x = JI_PropAvail$`PA swamp`, y = JI_PropAvail$`JI swamp`,
     ylim = c(-1, 1),
     xlab = "Proportion available", ylab = "Jacobs index", 
     main = "Relation Jacobs index and availability of swamp",
     cex.main = 1)
text(JI_PropAvail$`PA swamp`, JI_PropAvail$`JI swamp`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Shrubland
plot(x = JI_PropAvail$`PA shrubland`, y = JI_PropAvail$`JI shrubland`,
     ylim = c(-1, 1),
     xlab = "Proportion available", ylab = "Jacobs index", 
     main = "Relation Jacobs index and availability of shrubland",
     cex.main = 1)
text(JI_PropAvail$`PA shrubland`, JI_PropAvail$`JI shrubland`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Heathland
plot(x = JI_PropAvail$`PA heathland`, y = JI_PropAvail$`JI heathland`,
     ylim = c(-1, 1),
     xlab = "Proportion available", ylab = "Jacobs index", 
     main = "Relation Jacobs index and availability of heathland",
     cex.main = 1)
text(JI_PropAvail$`PA heathland`, JI_PropAvail$`JI heathland`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)

# Grassy heathland
plot(x = JI_PropAvail$`PA grassy heathland`, y = JI_PropAvail$`JI grassy heathland`,
     ylim = c(-1, 1),
     xlab = "Proportion available", ylab = "Jacobs index", 
     main = "Relation Jacobs index and availability of grassy heathland",
     cex.main = 1)
text(JI_PropAvail$`PA grassy heathland`, JI_PropAvail$`JI grassy heathland`,
     labels=rownames(JI_PropAvail), cex=0.4, font=2)








