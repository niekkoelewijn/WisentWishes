library(tidyverse)

dat <- read_csv("data/Kraansvlak.csv")
dat

momentuHMM::crawlWrap()

x <- data.frame(t = seq(from=10000, length.out=10, by=10000),
                temp = runif(10, 20, 30))
plot(x$t, x$temp)

?approx()
?approxfun()

# Example
temp_fun <- approxfun(x=x$t, y=x$temp, method = "linear")
temp_fun(15000)

# Tryout
temp_fun <- approxfun(x=InterpolatedTrackList$KraansvlakTrack1$time_coded, y=InterpolatedTrackList$KraansvlakTrack1$temp, method = "linear")
interpolated_temp_timestemp <- as.numeric(as.POSIXct(strptime("2020-10-25 15:56:43", format = "%Y-%m-%d %H:%M:%S")))
temp_fun(interpolated_temp_timestemp)

tseq <- seq(from=min(x$t), to=max(x$t), length.out=100)
points(tseq, temp_fun(tseq), col=2, cex=0.5, pch=16)


library(rgdal)
library(sp)
library(raster)

dat <- dat %>%
  drop_na(longitude, latitude)
coordinates(dat) = ~ longitude + latitude
dat
proj4string(dat) <- "+init=epsg:4326"

datrd <- spTransform(dat, "+init=epsg:28992")
datrdcoords <- coordinates(datrd)
datrdcoords <- as.data.frame(datrdcoords)
head(datrdcoords)

dat$x <- datrdcoords$longitude
dat$y <- datrdcoords$latitude

myFun <- function(x){
  x$timestamp <-
    
    return(y)
}

myFun(x = kraansvlakxxxx)
myFun(x = maashorstxxxx)

for(i in seq_along(listDF)) {
  listDF[[i]]$timestamp <-
}

do.call(rbind, listDF)

dat %>%
  group_by(ID) %>%
  distinct() %>%
  arrange(dttm) %>%
  mutate(dx = lead(x) - x,
         dy = lead(y) - y,
         dt = t - lag(t),
         dist = sqrt(dx^2 + dy^2)) %>%
  summarise(meanDist = mean(dist))