# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to set up first, simple, RSF for a specific time of the day (12:00 -
### 16:00) and a specific season (summer)

### Create random points for each study area

## Kraansvlak

# Observations
KraansvlakObs <- make_track(KraansvlakPoints, .x = "X", .y = "Y", .t = "time",
                            crs = 28992, all_cols = T)

# Pseudo-absences
KraansvlakAbs <- KraansvlakObs %>% 
  random_points() %>% 
  extract_covariates(MaskedList$Kraansvlak) %>% 
  drop_na(LGN2020_5m)


