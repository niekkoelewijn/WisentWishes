### Decimal time
library(lubridate)

# Generate sequence with time (5min spacing)
times <- seq(from=Sys.time(), to=Sys.time()+3*24*60*60, by="5 mins")
times

# Convert to decimal time (0.0 = 0:00, 0.5 = 12:00, 1.0 = 24:00)
times_decimal <- as.numeric(difftime(times, floor_date(times, "days"), units="days"))
plot(times, times_decimal, type="l")

# Compute waveforms (decimal time converted to 2pi radians, then cos/sin)
plot(times, -cos(2*pi*times_decimal), type="l")
plot(times, sin(2*pi*times_decimal), type="l")


### Predict a fitted GLM on new data (that you can specify yourself)

# generate combination of data
predData <- expand.grid(CCI = c(-1,0,1),
                        habitat = letters[1:5])

# if "rsf" is a fitted glm, then you can predict using:
predict(rsf) # using the data that were used while fitting the model
predict(rsf, newdata = predData) # predicting on new data, here returning the linear predictor (thus on the logit scale)
predict(rsf, newdata = predData, type = "response") # idem, but now on the probability scale
