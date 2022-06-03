# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to set up SSF for all study areas and all times

# Test with 1 track
KraansvlakTrack1 <- AllTrackPoints[which(AllTrackPoints$track_ID == 1),]

KraansvlakTrack1xyt <- make_track(KraansvlakTrack1, .x = "X", .y = "Y", .t = "time",
                                  crs = 28992, all_cols = T)

KraansvlakTrack1yxtssfdat <- KraansvlakTrack1xyt %>% 
  track_resample(rate = hours(1), tolerance = seconds(10)) %>% 
  steps_by_burst(lonlat = F, keep_cols = "start" ) %>%
  random_steps() 

# Make sure you have rasters of distance to forest, road and water per study area
# and the landuse class codes. This needs to be joined to the LUTLanduseClasses
