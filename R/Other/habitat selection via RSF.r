### Goal: habitat selection via logistic regression (RSF) using use-available design
# account for sampling in offset of GLM

# see e.g.
# https://terpconnect.umd.edu/~egurarie/teaching/SpatialModelling_AKTWS2018/index.html
# Modern Tools for Spatial Modeling and Animal Movement Analysis
# Alaska - The Wildlife Society - Annual Meeting
# Anchorage, AK - March 2018

# https://media.adelaide.edu.au/institutes/enviroment/2010/ACEBB-%202010%20seminar15.pdf

# A reformulation of the selection ratio shed light on resource selection functions and leads to a unified framework for habitat selection studies
# https://www.biorxiv.org/content/10.1101/565838v3

# Relative Selection Strength: Quantifying effect size in habitat- and step-selection inference
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5528224/


### Prelims
library(raster)
library(tidyverse)


### Simulation settings
nPoints <- 1e5 # nr of animal locations (used points)
nPerLoc <- 10   # nr of pseudo absences per used point
seed <- 1234567
set.seed(seed)


### Get class probabilities (range: 0.1 - 0.9)
classes <- 1:5
classProbs <- 0.1 + c(0:4)*0.2
names(classProbs) <- paste0("c",classes)
# classProbs


### Simulate landscape: raster 0-100, res=1, with 5 land cover classes
{
  r <- raster(xmn=0, ymn=0, xmx=100, ymx=100, res=1)
  rxy <- xyFromCell(r, seq_len(ncell(r))) %>% 
    as_tibble() %>% 
    mutate(xbin = 1L + (x %% 100) %/% 20,
           ybin = 1L + (y %% 100) %/% 20,
           bin = (ybin - 1) * 5 + xbin)
  # rxy
  # rxy %>% count(bin)
  # rxy %>% count(bin) %>% count(n)
  # now: 25 discrete "bins" each 20x20 pixels (400)
  
  # length(unique(rxy$bin))
  classes <- rep(1:5,5)
  classes <- sample(classes, size=25L, replace=FALSE)
  classes <- LETTERS[1:5][classes]
  # classes
  # table(classes)
  rxy$class <- as.factor(classes[rxy$bin])
  r[] <- rxy$class
  # r
  # plot(r)
}
r
plot(r)


### Sample points over this landscape
{
  ### Sample pseudo-absences
  PA <- tibble(pid = rep(seq_len(nPoints), each = nPerLoc),
               i = rep(seq_len(nPerLoc), times = nPoints),
               x = runif(n = nPoints * nPerLoc, min = 0, max = 100),
               y = runif(n = nPoints * nPerLoc, min = 0, max = 100)) %>% 
    mutate(class = raster::extract(r, cbind(x,y)))
  # PA
  # PA %>% count(class)
  
  
  ### Sample used points (first sample many, then downsample with probs of classes)
  USED <- tibble(pid = rep(seq_len(nPoints), each = nPerLoc),
               i = rep(seq_len(nPerLoc), times = nPoints),
               x = runif(n = nPoints * nPerLoc, min = 0, max = 100),
               y = runif(n = nPoints * nPerLoc, min = 0, max = 100)) %>% 
    mutate(class = raster::extract(r, cbind(x,y)))
  # USED
  # USED %>% count(class)
  USED <- USED %>% 
    mutate(classprob = classProbs[class]) %>% 
    slice_sample(n = nPoints, weight_by = classprob)
  # USED %>% count(class, classprob) %>% arrange(classprob)
  
  ### Merge
  PA$label <- 0L
  USED$label <- 1L
  USED$i <- 0L
  dat <- bind_rows(PA, select(USED, -classprob)) %>% 
    as_tibble() %>% 
    arrange(pid, i)
  # dat
  
  ### Clear memory
  rm(PA); rm(USED)
}
dat
classProbs
dat %>% count(class, label)


# r
# plot(r); points(filter(dat, label==0L)$x, filter(dat, label==0L)$y, pch=16, cex=0.4, col=1)
# plot(rn); points(filter(dat, label==1L)$x, filter(dat, label==1L)$y, pch=16, cex=0.4, col=1)


### Describe using selection ratios
{
  selectionRatios <- dat %>% 
    group_by(label) %>% 
    mutate(classWeight = 1 / n()) %>% 
    ungroup() %>% 
    group_by(class, label) %>% 
    summarize(sumWeights = sum(classWeight), .groups="drop_last") %>% 
    summarize(selectionRatio = sumWeights[2] /sumWeights[1], .groups="drop") %>% 
    mutate(logSR = log(selectionRatio))
  selectionRatios
  
  plot(classProbs, selectionRatios$selectionRatio)
  srLm <- lm(selectionRatios$selectionRatio ~ classProbs)
  summary(srLm)
  abline(srLm, col=2, lty=2)
}


### Analyse using RSF with class as factor
{
  # Convert class to factor
  dat %>% count(class)
  dat$class = as.integer(as.character(dat$class))
  
  
  ### EITHER: use current dtaa
  {
    classLevels <- c(1:5)
    datRSF <- dat %>%
      mutate(class = factor(class, levels=classLevels))
    classP <- classProbs[classLevels]
  }
  
  
  ### OR: ADD a 6th "fake" class
  {
    # data to be added
    datREF <- tibble(pid = -999L,
                     i = 0:nPerLoc,
                     x = -999,
                     y = -999,
                     class = 999,
                     label = c(1, rep(0, nPerLoc)))
    classLevels <- c(999, 1:5)
    datRSF <- dat %>%
      bind_rows(datREF) %>% 
      mutate(class = factor(class, levels=classLevels)) 
    classP <- c(0.5, classProbs)
  }
  

  ### Check levels/contrast  
  levels(datRSF$class)
  contrasts(datRSF$class)

  # Checks
  datRSF %>% count(class)
  datRSF %>% filter(label == 1L) %>% count(class)
  plot(datRSF %>% filter(label == 1L) %>% count(class) %>% pull(n), classP)
  
  
  ### FIT RSF WITH OFFSET
  rsf <- glm(label ~ class, data = datRSF,
             family = binomial("logit"),
             offset = rep(qlogis(1/(nPerLoc+1)), nrow(datRSF)))
  summary(rsf)

  
  ### Separate intercept from coefficients
  intrcpt <- rsf$coefficients[[1]]
  coeffs <- rsf$coefficients
  coeffs[[1]] <- 0
  coeffs
  intrcpt
  intrcpt + coeffs # This is the linear predictor on the logit scale
  
  # Plot
  ftlm <- lm(classP ~ eval(intrcpt + coeffs))
  plot(intrcpt + coeffs, classP, main=paste("R2:", round(summary(ftlm)$r.squared, 3)))
  abline(ftlm, lty=2, col=2)
  # THUS: linear predictor is NOT proportional to the probability of use:
  # namely, studies demonstrate that RSF is exponential model where the exponent is proportional
  
  # Plot on exponential form
  ftlm <- lm(classP ~ exp(intrcpt + coeffs))
  plot(exp(intrcpt + coeffs), classP, main=paste("R2:", round(summary(ftlm)$r.squared, 3)))
  abline(ftlm, lty=2, col=2)
  # INDEED: now exp(linear predictor) is proportional to the probability of use!


  ### Get variable importance  
  # https://koalaverse.github.io/vip/articles/vip.html
  library(vip)
  vip(rsf)
}


### END