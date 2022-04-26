# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to interpolate GPS tracks to get the same temporal resolution
### everywhere.
IDTrack <- mutate(VeluweTracks[[2]], ID = 1)
InterpolatedTrack <- momentuHMM::crawlWrap(IDTrack, Time.name = "time", coord = c("X", "Y"), 
                                           time.scale = "hours", proj = 28992, timeStep = 3600)
InterpolatedTrackSf <- st_as_sf(InterpolatedTrack[[2]], coords = c("mu.x", "mu.y"))
plot(st_geometry(InterpolatedTrackSf))