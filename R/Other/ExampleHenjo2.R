LUTLNG
LUTUserDefined



r <- KraansvlakLandUse
rnew <- factor(LUTLNG$landuse_class, levels=unique(LUTLNG$landuse_class)) %>% 
  as.integer()

r[] <- as.integer(factor(r[], levels=unique(LUTLNG$landuse_code)))
r
table(r[])
r[] <- rnew[r[]]
plot(r)

r <- MaashorstLandUse
rnew <- factor(LUTLNG$landuse_class, levels=unique(LUTLNG$landuse_class)) %>% 
  as.integer()

r[] <- as.integer(factor(r[], levels=unique(LUTLNG$landuse_code)))
r
table(r[])
r[] <- rnew[r[]]
plot(r)

r <- VeluweLandUse
rnew <- factor(LUTLNG$landuse_class, levels=unique(LUTLNG$landuse_class)) %>% 
  as.integer()

r[] <- as.integer(factor(r[], levels=unique(LUTLNG$landuse_code)))
r
table(r[])
r[] <- rnew[r[]]
plot(r)

r <- SlikkenvdHeenLandUse
rnew <- factor(LUTLNG$landuse_class, levels=unique(LUTLNG$landuse_class)) %>% 
  as.integer()

r[] <- as.integer(factor(r[], levels=unique(LUTLNG$landuse_code)))
r
table(r[])
r[] <- rnew[r[]]
plot(r)


r2 <- KraansvlakLandUse
r2[] <- as.numeric(r2[], levels=unique(LUTLNG$landuse_class))
freq(r2)
r2
plot(r2)
