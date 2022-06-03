# Course: REG-80436 / MSc Thesis
# Project: Wisent Wishes
# Name: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: Henjo de Knegt
# Year: 2022

# Practice script with Fieberg 2021 data and steps

# Install packages
if(!"raster" %in% rownames(installed.packages())){install.packages("raster")}
if(!"amt" %in% rownames(installed.packages())){install.packages("amt")}
if(!"forcats" %in% rownames(installed.packages())){install.packages("forcats")}
if(!"permutations" %in% rownames(installed.packages())){install.packages("permutations")}
if(!"ggplot2" %in% rownames(installed.packages())){install.packages("ggplot2")}
if(!"dplyr" %in% rownames(installed.packages())){install.packages("dplyr")}


# Load packages
library(raster)
library(amt)
library(forcats)
library(permutations)
library(ggplot2)
library(dplyr)


# Load fisher location data and spatial covariate data
fisher <- amt_fisher
landuse <- amt_fisher_covar$landuse
elevation <- amt_fisher_covar$elevation
popden <- amt_fisher_covar$popden

# Filter for individual Lupe and sort the locations using the timestamps of the observations
Lupe <- fisher %>% 
  filter(name == "Lupe") %>% 
  arrange(t_)

# Create function to reclassify landuse types
reclass_landuse <- function(x) {
  fct_collapse(factor(x),
               forest = c("30","40","50","60", "70","80", "90","100"),
               grass = c("120", "140"),
               wet = c("160"))
}

### HSA ###

# Location data time intervals
summarize_sampling_rate(Lupe)

ssf_Lupe <- Lupe %>% 
  track_resample(rate = minutes(2), tolerance = seconds(20)) %>% 
  steps_by_burst() %>% 
  random_steps()

# Plot Lupe location data on elevation map
plot(elevation)
points(Lupe$x_, Lupe$y_, add = T)

# Generate Available Points
n.frac <- c(1, 5, 10, 50, 100)
n.pts <- ceiling(nrow(dat) * n.frac)
n.rep <- 20

rsf_Lupe <- Lupe %>% 
  random_points() %>% 
  extract_covariates(landuse) %>% 
  extract_covariates(elevation) %>% 
  extract_covariates(popden) %>% 
  mutate(elevation = scale(elevation)[, 1], 
         popden = scale(popden)[, 1],
         landuseC = reclass_landuse(landuse), 
         forest = landuseC == "forest",
         weight = ifelse(case_, 1, 1e3)
  )

print(Lupe.dat, n = 3, width = Inf)

# Explore sensitivity of HSF coefficients to the number of available points
n.frac <- c(1, 5, 10, 50, 100)
n.pts <- ceiling(nrow(Lupe) * n.frac)
n.rep <- 20

res1 <- tibble(
  n.pts = rep(n.pts, n.rep), 
  frac = rep(n.frac, n.rep), 
  res = map(
    n.pts, ~
      Lupe %>% random_points() %>% 
      extract_covariates(landuse) %>% 
      extract_covariates(elevation) %>% 
      extract_covariates(popden) %>% 
      mutate(landuseC = reclass_landuse(landuse), 
             elevation = scale(elevation), 
             popden = scale(popden),
             w = ifelse(case_, 1, 5000)) %>% 
      glm(case_ ~ elevation + popden + landuseC, 
          weight = w, data = ., family = binomial()) %>% 
      tidy()))

res1 <- tibble(
  n.pts = rep(n.pts, n.rep), 
  frac = rep(n.frac, n.rep), 
  res = map(
    n.pts, ~
      rsf_Lupe %>% random_points(n = 10*nrow(rsf_Lupe)) %>% 
      extract_covariates(landuse) %>% 
      extract_covariates(elevation) %>% 
      extract_covariates(popden) %>% 
      mutate(landuseC = reclass_landuse(landuse), 
             elevation = scale(elevation), 
             popden = scale(popden),
             w = ifelse(case_, 1, 5000)) %>% 
      glm(case_ ~ elevation + popden + landuseC, 
          weight = w, data = Lupe, family = binomial()) %>% 
      tidy()))

#' Use the largest sample size here for the rest of the paper
Lupe.dat <- Lupe %>% 
  random_points(n = max(n.pts)) %>% 
  extract_covariates(landuse) %>% 
  extract_covariates(elevation) %>% 
  extract_covariates(popden) %>% 
  mutate(landuseC = reclass_landuse(landuse), 
         elevation = scale(elevation), 
         popden = scale(popden))

Lupe.dat %>% 
  group_by(case_, landuseC) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), 
         label = paste0(round(prop * 100, 1), "%")) %>% 
  ggplot(aes(landuseC, prop, fill = case_, group=case_,label = label)) + 
  geom_col(position = position_dodge2()) +
  geom_text(size = 4, vjust = -0.25, position = position_dodge(width = 1)) +
  labs(x = "Land use class", y = "Proportion", fill = "case_")+
  scale_fill_brewer(palette = "Paired", name="case_", 
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used")) +
  theme_light()

Lupe.dat$w <- ifelse(Lupe.dat$case_, 1, 5000)
HSF.Lupe1 <- glm(case_ ~ elevation + popden + landuseC, 
                 data = Lupe.dat, weight = w,
                 family = binomial(link = "logit"))

summary(HSF.Lupe1)

exp(3 * coef(HSF.Lupe1)["elevation"] + 
      1.5 * coef(HSF.Lupe1)["popden"] + coef(HSF.Lupe1)["landuseCwet"]) /
  exp(2 * coef(HSF.Lupe1)["elevation"] + 1.5 * coef(HSF.Lupe1)["popden"] + 
        coef(HSF.Lupe1)["landuseCwet"])

exp(coef(HSF.Lupe1)["elevation"])

ggplot(Lupe.dat, aes(landuseC, elevation, fill=case_))+
  geom_boxplot() +    
  scale_fill_brewer(palette = "Paired", name="case_", 
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used"))+ 
  theme_light() 

HSF.Lupe3 <- glm(
  case_ ~ elevation + popden + landuseC + elevation:landuseC,
  data = Lupe.dat, 
  weight=w,
  family = binomial)
summary(HSF.Lupe3)

# Availability of forest and wet within Lupe's MCP
a.forest <- with(Lupe.dat[Lupe.dat$case_ == 0, ], sum(landuseC == "forest")) 
a.wet <- with(Lupe.dat[Lupe.dat$case_ == 0, ], sum(landuseC == "wet"))
# Multiply by the ratio of availabilities
exp(coef(HSF.Lupe1)["landuseCwet"])*a.wet/a.forest


### iSSA ###

# doin this again because of constant crashes
library(raster)
library(amt)

# Load fisher location data and spatial covariate data
fisher <- amt_fisher
landuse <- amt_fisher_covar$landuse
elevation <- amt_fisher_covar$elevation
popden <- amt_fisher_covar$popden

# Filter for individual Lupe and sort the locations using the timestamps of the observations
dat <- fisher %>% 
  filter(name == "Lupe") %>% 
  arrange(t_)

# Create function to reclassify landuse types
reclass_landuse <- function(x) {
  fct_collapse(factor(x),
               forest = c("30","40","50","60", "70","80", "90","100"),
               grass = c("120", "140"),
               wet = c("160"))
}

ssf_dat <- dat %>% 
  track_resample(rate = minutes(2), tolerance = seconds(20)) %>% 
  steps_by_burst() %>% 
  random_steps() %>% 
  extract_covariates(landuse, where = "both") %>% 
  extract_covariates(elevation, where = "both") %>% 
  extract_covariates(popden, where = "both") %>% 
  mutate(elevation_start = scale(elevation_start), 
         elevation_end = scale(elevation_end),
         popden_start = scale(popden_start),
         popden_end = scale(popden_end),
         landuseC_start = reclass_landuse(landuse_start), 
         landuseC_end = reclass_landuse(landuse_end), 
         forest_start = landuseC_start == "forest",
         forest_end = landuseC_end == "forest",
         cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_)
  ) %>% 
  filter(!is.na(ta_))

