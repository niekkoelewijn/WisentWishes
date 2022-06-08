


xy.obs <- KraansvlakPoints
xy.random <- st_sample(KraansvlakStudyAreaRDNew, 10*nrow(KraansvlakPoints), type = "random")
plot(xy.random, asp = 1, col = "darkblue", pch = 19, cex = 0.5)
points(x = xy.obs$X, y = xy.obs$Y , pch = 19, col = "orange", cex = 0.5)

StudyAreasCollection <- st_union(KraansvlakStudyAreaRDNew, SlikkenvdHeenStudyAreaRDNew)
StudyAreasCollection <- st_union(StudyAreasCollection, VeluweStudyAreaRDNew)
StudyAreasCollection <- st_union(StudyAreasCollection, MaashorstStudyAreaRDNew2022)
StudyAreasCollection <- st_union(StudyAreasCollection, MaashorstStudyAreaRDNew2016)

# Test with 1 track
KraansvlakTrack1 <- AllTrackPoints[which(AllTrackPoints$track_ID == 1),]

KraansvlakTrack1xyt <- make_track(KraansvlakTrack1, .x = "X", .y = "Y", .t = "time",
                        crs = 28992, all_cols = T)

KraansvlakTrack1yxtssfdat <- KraansvlakTrack1xyt %>% 
  track_resample(rate = hours(1), tolerance = seconds(10)) %>% 
  steps_by_burst(lonlat = F, keep_cols = "start" ) %>%
  random_steps() 

write_csv(KraansvlakTrack1yxtssfdat, file = "~/WisentWishes/MScThesisData/GPS location data/Step3RSF/KraansvlakTrack1.csv")

plot(xy.random, asp = 1, col = "darkblue", pch = 19, cex = 0.5)
points(x = xy.obs$X, y = xy.obs$Y , pch = 19, col = "orange", cex = 8)


