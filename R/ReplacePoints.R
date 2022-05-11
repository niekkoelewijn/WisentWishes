# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: dr. ir. H.J. de Knegt
# Year: 2022

### Script to replace interpolated points in the tracks that felled outside the
### study area.

### In each study area, in some of the tracks, interpolated points where generated
### in places outside of the study area. In total 224 out of 122511 points fell
### outside the study area after interpolation, which is equal to 0.002% of the
### points. Out of 47 tracks, 16 had no interpolated points outside the study
### area, and 31 had 1 or more points outside the study area. Interpolated points
### outside the study area were mostly caused by the dispropotionate shape of the
### study areas, with lots of angles, edges, and for the Veluwe area a closed off
### area within the study area. I will replace the interpolated points, so that
### so that the track of the bisons becomes more realistic. No, when a point is
### generated outside the study area, the point is placed in a unrealistic short
### cut between two measured GPS points. By replacing these erroneous to more
### plausible places, the level of realism of the tracks increases. By determining
### the new location of the point, the distance between the interpolated point
### and the original point was conserved as best as possible.


## Load files from step 6

# Paths to step 6 tracks
setwd("~/WisentWishes")
GPSStep6Path <- "~/WisentWishes/MScThesisData/GPS location data/Step6Preprocess/"
GPSStep6Vec <- list.files(path = GPSStep6Path)

# Get names of tracks, without .csv
NameVec <- c()
for(i in seq_along(GPSStep6Vec)){
  NameVec[i] <- tools::file_path_sans_ext(GPSStep6Vec[i])
}

# Create list of files from step 6 directory
Step6Tracks <- lst()
for(i in seq_along(GPSStep6Vec)){
  Step6Tracks[[i]] <- read_csv(file = paste0(GPSStep6Path, GPSStep6Vec[i]))
}

# Add names to list to understand what tibbles are from which tracks
names(Step6Tracks) <- NameVec


## Create general function to adapt coordinates of a point in the dataset
ReplacePoint <- function(GPStrack, time, new_x, new_y){
  
  # Make shure the track is arranged on time
  UpdatedGPStrack <- GPStrack %>% 
    arrange(time)
  
  # Get row of the input time, the time where I want to adapt the coordinates
  RowNum <- which(
    UpdatedGPStrack$time == as.POSIXct(strptime(time, 
                                                format = "%Y-%m-%d %H:%M:%S", 
                                                tz = "GMT"))
    , arr.ind = TRUE)[1]
  
  # Replace current mu.x and mu.y with new, desired, coordinates
  UpdatedGPStrack[RowNum, ]$mu.x <- new_x
  UpdatedGPStrack[RowNum, ]$mu.y <- new_y
  
  # Return the updated GPS track
  return(UpdatedGPStrack)
}



### Kraansvlak

## Kraansvlak Non-Intervention tracks

# Replace the 2 interpolated trackpoints outside the study area of KraansvlakTrack1
Step6Tracks$KraansvlakTrack1 <- ReplacePoint(Step6Tracks$KraansvlakTrack1, 
                                        time = "2020-10-25 20:56:43",
                                        new_x = 99521.37,
                                        new_y = 489986.76)

Step6Tracks$KraansvlakTrack1 <- ReplacePoint(Step6Tracks$KraansvlakTrack1, 
                                        time = "2020-10-25 21:56:43",
                                        new_x = 99573.93,
                                        new_y = 489971.47)

# Replace the 6 interpolated trackpoints outside the study area of KraansvlakTrack2
Step6Tracks$KraansvlakTrack2 <- ReplacePoint(Step6Tracks$KraansvlakTrack2, 
                                             time = "2020-11-30 03:05:32",
                                             new_x = 99170.57,
                                             new_y = 488425.25)

Step6Tracks$KraansvlakTrack2 <- ReplacePoint(Step6Tracks$KraansvlakTrack2, 
                                             time = "2020-11-30 04:05:32",
                                             new_x = 99215.08,
                                             new_y = 488257.66)

Step6Tracks$KraansvlakTrack2 <- ReplacePoint(Step6Tracks$KraansvlakTrack2, 
                                             time = "2020-12-10 02:05:32",
                                             new_x = 99579.40,
                                             new_y = 489972.03)

Step6Tracks$KraansvlakTrack2 <- ReplacePoint(Step6Tracks$KraansvlakTrack2, 
                                             time = "2020-12-10 03:05:32",
                                             new_x = 99524.11,
                                             new_y = 489983.09)

Step6Tracks$KraansvlakTrack2 <- ReplacePoint(Step6Tracks$KraansvlakTrack2, 
                                             time = "2020-12-26 22:05:32",
                                             new_x = 98443.41,
                                             new_y = 489138.20)

Step6Tracks$KraansvlakTrack2 <- ReplacePoint(Step6Tracks$KraansvlakTrack2, 
                                             time = "2021-01-08 03:05:32",
                                             new_x = 99148.36,
                                             new_y = 488468.23)

# Replace the 3 interpolated trackpoints outside the study area of KraansvlakTrack3
Step6Tracks$KraansvlakTrack3 <- ReplacePoint(Step6Tracks$KraansvlakTrack3, 
                                             time = "2021-04-14 08:44:54",
                                             new_x = 98824.09,
                                             new_y = 490264.39)

Step6Tracks$KraansvlakTrack3 <- ReplacePoint(Step6Tracks$KraansvlakTrack3, 
                                             time = "2021-04-21 08:44:54",
                                             new_x = 100009.33,
                                             new_y = 489400.04)

Step6Tracks$KraansvlakTrack3 <- ReplacePoint(Step6Tracks$KraansvlakTrack3, 
                                             time = "2021-04-21 09:44:54",
                                             new_x = 100026.31,
                                             new_y = 489376.35)

# Replace the 17 interpolated trackpoints outside the study area of KraansvlakTrack4
Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2021-05-20 14:39:38",
                                             new_x = 98401.284,
                                             new_y = 489363.790)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2021-05-27 12:39:38",
                                             new_x = 98857.31,
                                             new_y = 490247.45)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2021-06-03 14:39:38",
                                             new_x = 98456.27,
                                             new_y = 489353.03)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2021-06-10 18:39:38",
                                             new_x = 98792.614,
                                             new_y = 488605.667)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2021-07-20 08:39:38",
                                             new_x = 100113.373,
                                             new_y = 489298.444)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2021-08-08 12:39:38",
                                             new_x = 99210.23,
                                             new_y = 488417.51)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2021-08-17 08:39:38",
                                             new_x = 97981.78,
                                             new_y = 489502.86)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2021-09-01 09:39:38",
                                             new_x = 99326.91,
                                             new_y = 490113.66)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2021-09-01 12:39:38",
                                             new_x = 99525.564,
                                             new_y = 489981.311)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2021-09-02 10:39:38",
                                             new_x = 99886.392,
                                             new_y = 489630.428)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2021-10-08 10:39:38",
                                             new_x = 99869.271,
                                             new_y = 489904.247)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2021-11-21 20:39:38",
                                             new_x = 99982.33,
                                             new_y = 489484.19)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2021-11-21 21:39:38",
                                             new_x = 99954.36,
                                             new_y = 489518.64)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2021-11-24 17:39:38",
                                             new_x = 99367.89,
                                             new_y = 490099.08)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2021-12-19 16:39:38",
                                             new_x = 98735.542,
                                             new_y = 490307.403)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2022-01-02 01:39:38",
                                             new_x = 99494.116,
                                             new_y = 490057.136)

Step6Tracks$KraansvlakTrack4 <- ReplacePoint(Step6Tracks$KraansvlakTrack4, 
                                             time = "2022-01-18 13:39:38",
                                             new_x = 99182.88,
                                             new_y = 488431.87)


### Slikken vd Heen


## Slikken vd Heen Non-Intervention Tracks

# Replace the 3 interpolated trackpoints outside the study area of SlikkenvdHeenTrack1
Step6Tracks$SlikkenvdHeenTrack1 <- ReplacePoint(Step6Tracks$SlikkenvdHeenTrack1, 
                                                time = "2020-09-27 13:00:00",
                                                new_x = 73648.89,
                                                new_y = 405655.85)

Step6Tracks$SlikkenvdHeenTrack1 <- ReplacePoint(Step6Tracks$SlikkenvdHeenTrack1, 
                                                time = "2020-10-16 02:00:00",
                                                new_x = 73822.855,
                                                new_y = 405766.438)

Step6Tracks$SlikkenvdHeenTrack1 <- ReplacePoint(Step6Tracks$SlikkenvdHeenTrack1, 
                                                time = "2020-11-09 20:00:00",
                                                new_x = 73674.41,
                                                new_y = 405642.19)

# Replace the 1 interpolated trackpoint outside the study area of SlikkenvdHeenTrack2
Step6Tracks$SlikkenvdHeenTrack2 <- ReplacePoint(Step6Tracks$SlikkenvdHeenTrack2, 
                                                   time = "2020-12-17 02:00:30",
                                                   new_x = 73745.0,
                                                   new_y = 405669.6)

# Replace the 7 interpolated trackpoints outside the study area of SlikkenvdHeenTrack3
Step6Tracks$SlikkenvdHeenTrack3 <- ReplacePoint(Step6Tracks$SlikkenvdHeenTrack3, 
                                                time = "2021-05-06 19:00:30",
                                                new_x = 73641.61,
                                                new_y = 405656.92)

Step6Tracks$SlikkenvdHeenTrack3 <- ReplacePoint(Step6Tracks$SlikkenvdHeenTrack3, 
                                                time = "2021-05-12 22:00:30",
                                                new_x = 73628.46,
                                                new_y = 405658.05)

Step6Tracks$SlikkenvdHeenTrack3 <- ReplacePoint(Step6Tracks$SlikkenvdHeenTrack3, 
                                                time = "2021-05-25 17:00:30",
                                                new_x = 73654.97,
                                                new_y = 405665.00)

Step6Tracks$SlikkenvdHeenTrack3 <- ReplacePoint(Step6Tracks$SlikkenvdHeenTrack3, 
                                                time = "2021-05-25 19:00:30",
                                                new_x = 73688.41,
                                                new_y = 405684.27)

Step6Tracks$SlikkenvdHeenTrack3 <- ReplacePoint(Step6Tracks$SlikkenvdHeenTrack3, 
                                                time = "2021-05-25 20:00:30",
                                                new_x = 73751.38,
                                                new_y = 405720.29)

Step6Tracks$SlikkenvdHeenTrack3 <- ReplacePoint(Step6Tracks$SlikkenvdHeenTrack3, 
                                                time = "2021-10-10 15:00:30",
                                                new_x = 73726.77,
                                                new_y = 405655.74)

Step6Tracks$SlikkenvdHeenTrack3 <- ReplacePoint(Step6Tracks$SlikkenvdHeenTrack3, 
                                                time = "2021-11-04 23:00:30",
                                                new_x = 73747.3,
                                                new_y = 405688.7)


## Slikken vd Heen Habituate tracks

# Replace the 1 interpolated trackpoint outside the study area of SlikkenvdHeenHabTrack2
Step6Tracks$SlikkenvdHeenHabTrack2 <- ReplacePoint(Step6Tracks$SlikkenvdHeenHabTrack2, 
                                             time = "2020-08-21 16:00:00",
                                             new_x = 74047.44,
                                             new_y = 405777.48)

# Replace the 4 interpolated trackpoints outside the study area of SlikkenvdHeenHabTrack4
Step6Tracks$SlikkenvdHeenHabTrack4 <- ReplacePoint(Step6Tracks$SlikkenvdHeenHabTrack4, 
                                                   time = "2020-08-29 12:00:32",
                                                   new_x = 74228.08,
                                                   new_y = 405419.25)

Step6Tracks$SlikkenvdHeenHabTrack4 <- ReplacePoint(Step6Tracks$SlikkenvdHeenHabTrack4, 
                                                   time = "2020-08-29 13:00:32",
                                                   new_x = 74167.59,
                                                   new_y = 405534.48)

Step6Tracks$SlikkenvdHeenHabTrack4 <- ReplacePoint(Step6Tracks$SlikkenvdHeenHabTrack4, 
                                                   time = "2020-08-29 14:00:32",
                                                   new_x = 74132.44,
                                                   new_y = 405653.75)

Step6Tracks$SlikkenvdHeenHabTrack4 <- ReplacePoint(Step6Tracks$SlikkenvdHeenHabTrack4, 
                                                   time = "2020-08-29 15:00:32",
                                                   new_x = 74051.22,
                                                   new_y = 405772.94)


### Maashorst


## Maashorst Non-Intervention Tracks

# Replace the 5 interpolated trackpoints outside the study area of MaashorstTrack1
Step6Tracks$MaashorstTrack1 <- ReplacePoint(Step6Tracks$MaashorstTrack1, 
                                                   time = "2016-03-19 17:00:00",
                                                   new_x = 171927.1,
                                                   new_y = 411831.3)

Step6Tracks$MaashorstTrack1 <- ReplacePoint(Step6Tracks$MaashorstTrack1, 
                                            time = "2016-03-20 01:00:00",
                                            new_x = 171553.88,
                                            new_y = 412721.69)

Step6Tracks$MaashorstTrack1 <- ReplacePoint(Step6Tracks$MaashorstTrack1, 
                                            time = "2016-03-28 04:00:00",
                                            new_x = 172821.48,
                                            new_y = 411063.41)

Step6Tracks$MaashorstTrack1 <- ReplacePoint(Step6Tracks$MaashorstTrack1, 
                                            time = "2016-03-28 05:00:00",
                                            new_x = 172831.52,
                                            new_y = 411141.93)

Step6Tracks$MaashorstTrack1 <- ReplacePoint(Step6Tracks$MaashorstTrack1, 
                                            time = "2016-03-28 06:00:00",
                                            new_x = 172718.31,
                                            new_y = 411209.49)

# Replace the 10 interpolated trackpoints outside the study area of MaashorstTrack4
Step6Tracks$MaashorstTrack4 <- ReplacePoint(Step6Tracks$MaashorstTrack4, 
                                            time = "2016-12-28 18:30:00",
                                            new_x = 172444.83,
                                            new_y = 411087.36)

Step6Tracks$MaashorstTrack4 <- ReplacePoint(Step6Tracks$MaashorstTrack4, 
                                            time = "2016-12-28 19:30:00",
                                            new_x = 172474.28,
                                            new_y = 411123.18)

Step6Tracks$MaashorstTrack4 <- ReplacePoint(Step6Tracks$MaashorstTrack4, 
                                            time = "2016-12-28 20:30:00",
                                            new_x = 172509.50,
                                            new_y = 411154.45)

Step6Tracks$MaashorstTrack4 <- ReplacePoint(Step6Tracks$MaashorstTrack4, 
                                            time = "2016-12-28 21:30:00",
                                            new_x = 172538.33,
                                            new_y = 411184.20)

Step6Tracks$MaashorstTrack4 <- ReplacePoint(Step6Tracks$MaashorstTrack4, 
                                            time = "2016-12-28 22:30:00",
                                            new_x = 172574.76,
                                            new_y = 411213.04)

Step6Tracks$MaashorstTrack4 <- ReplacePoint(Step6Tracks$MaashorstTrack4, 
                                            time = "2017-01-06 11:30:00",
                                            new_x = 172248.12,
                                            new_y = 410921.31)

Step6Tracks$MaashorstTrack4 <- ReplacePoint(Step6Tracks$MaashorstTrack4, 
                                            time = "2017-01-06 12:30:00",
                                            new_x = 172274.15,
                                            new_y = 410926.51)

Step6Tracks$MaashorstTrack4 <- ReplacePoint(Step6Tracks$MaashorstTrack4, 
                                            time = "2017-01-06 13:30:00",
                                            new_x = 172293.66,
                                            new_y = 410940.82)

Step6Tracks$MaashorstTrack4 <- ReplacePoint(Step6Tracks$MaashorstTrack4, 
                                            time = "2017-01-06 14:30:00",
                                            new_x = 172308.63,
                                            new_y = 410956.44)

Step6Tracks$MaashorstTrack4 <- ReplacePoint(Step6Tracks$MaashorstTrack4, 
                                            time = "2017-01-06 15:30:00",
                                            new_x = 172328.79,
                                            new_y = 410970.10)

# Replace the 8 interpolated trackpoints outside the study area of MaashorstTrack6
Step6Tracks$MaashorstTrack6 <- ReplacePoint(Step6Tracks$MaashorstTrack6, 
                                            time = "2018-04-20 20:00:00",
                                            new_x = 171290.60,
                                            new_y = 412278.30)

Step6Tracks$MaashorstTrack6 <- ReplacePoint(Step6Tracks$MaashorstTrack6, 
                                            time = "2018-04-21 18:00:00",
                                            new_x = 171705.46,
                                            new_y = 412134.51)

Step6Tracks$MaashorstTrack6 <- ReplacePoint(Step6Tracks$MaashorstTrack6, 
                                            time = "2018-04-22 05:00:00",
                                            new_x = 171759.12,
                                            new_y = 412240.98)

Step6Tracks$MaashorstTrack6 <- ReplacePoint(Step6Tracks$MaashorstTrack6, 
                                            time = "2018-05-09 09:00:00",
                                            new_x = 172500.588,
                                            new_y = 411130.575)

Step6Tracks$MaashorstTrack6 <- ReplacePoint(Step6Tracks$MaashorstTrack6, 
                                            time = "2018-05-20 15:00:00",
                                            new_x = 172527.524,
                                            new_y = 411158.878)

Step6Tracks$MaashorstTrack6 <- ReplacePoint(Step6Tracks$MaashorstTrack6, 
                                            time = "2018-05-27 07:00:00",
                                            new_x = 172538.447,
                                            new_y = 411171.807)

Step6Tracks$MaashorstTrack6 <- ReplacePoint(Step6Tracks$MaashorstTrack6, 
                                            time = "2018-06-13 03:00:00",
                                            new_x = 171307.979,
                                            new_y = 411683.628)

Step6Tracks$MaashorstTrack6 <- ReplacePoint(Step6Tracks$MaashorstTrack6, 
                                            time = "2018-06-20 02:00:00",
                                            new_x = 172442.724,
                                            new_y = 411072.641)

# Replace the 5 interpolated trackpoints outside the study area of MaashorstTrack7
Step6Tracks$MaashorstTrack7 <- ReplacePoint(Step6Tracks$MaashorstTrack7, 
                                            time = "2019-03-25 19:00:00",
                                            new_x = 171673.0,
                                            new_y = 411914.4)

Step6Tracks$MaashorstTrack7 <- ReplacePoint(Step6Tracks$MaashorstTrack7, 
                                            time = "2019-06-17 09:00:00",
                                            new_x = 171834.96,
                                            new_y = 412242.68)

Step6Tracks$MaashorstTrack7 <- ReplacePoint(Step6Tracks$MaashorstTrack7, 
                                            time = "2019-06-17 10:00:00",
                                            new_x = 171727.62,
                                            new_y = 412158.68)

Step6Tracks$MaashorstTrack7 <- ReplacePoint(Step6Tracks$MaashorstTrack7, 
                                            time = "2019-09-30 17:00:00",
                                            new_x = 171704.60,
                                            new_y = 412124.62)

Step6Tracks$MaashorstTrack7 <- ReplacePoint(Step6Tracks$MaashorstTrack7, 
                                            time = "2019-10-02 19:00:00",
                                            new_x = 171922.60,
                                            new_y = 411850.25)

# Replace the 6 interpolated trackpoints outside the study area of MaashorstTrack9
Step6Tracks$MaashorstTrack9 <- ReplacePoint(Step6Tracks$MaashorstTrack9, 
                                            time = "2021-04-07 13:00:00",
                                            new_x = 171711.88,
                                            new_y = 412061.41)

Step6Tracks$MaashorstTrack9 <- ReplacePoint(Step6Tracks$MaashorstTrack9, 
                                            time = "2021-04-29 07:00:00",
                                            new_x = 172040.01,
                                            new_y = 410850.44)

Step6Tracks$MaashorstTrack9 <- ReplacePoint(Step6Tracks$MaashorstTrack9, 
                                            time = "2021-05-01 22:00:00",
                                            new_x = 171718.99,
                                            new_y = 411227.24)

Step6Tracks$MaashorstTrack9 <- ReplacePoint(Step6Tracks$MaashorstTrack9, 
                                            time = "2021-08-04 05:00:00",
                                            new_x = 171938.46,
                                            new_y = 411823.36)

Step6Tracks$MaashorstTrack9 <- ReplacePoint(Step6Tracks$MaashorstTrack9, 
                                            time = "2021-10-08 12:00:00",
                                            new_x = 172534.66,
                                            new_y = 411171.34)

Step6Tracks$MaashorstTrack9 <- ReplacePoint(Step6Tracks$MaashorstTrack9, 
                                            time = "2022-01-08 01:00:00",
                                            new_x = 171707.47,
                                            new_y = 412090.80)

# Replace the 1 interpolated trackpoint outside the study area of MaashorstTrack10
Step6Tracks$MaashorstTrack10 <- ReplacePoint(Step6Tracks$MaashorstTrack10, 
                                            time = "2022-03-02 15:52:40",
                                            new_x = 172164.94,
                                            new_y = 412353.40)

write_csv(Step6Tracks$MaashorstTrack10, file = "~/WisentWishes/MScThesisData/GPS location data/Old/UpdatedMaashorstTrack10.csv")

