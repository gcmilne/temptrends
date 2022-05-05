###############################
## Data cleaning & wrangling ##
###############################

## Load packages
library(dplyr)
library(binom)

## Read in data
dat <- read.csv("data/dat.csv")
nrow(dat)

## Remove rows where both n and 95%CIs are missing
row_index <- rowSums( is.na(dat[,c("low_ci", "up_ci", "n")])) != ncol (dat[,c("low_ci", "up_ci", "n")])
clean_dat <- dat[row_index, ]

## Clean data 
clean_dat <- clean_dat                                                            %>%
  group_by(country, region, population, subpopulation)                            %>%
  filter(n() > 1)                                                                 %>%  #select groups with >1 comparable datapoints
  rowwise()                                                                       %>%  #by row...
  mutate(k = ifelse(test=is.na(k) & !is.na(n), yes=round((prev/100)*n, 0), no=k)) %>%  #estimate k (if n is available)
  mutate(prev = ifelse(all(test=!is.na(k) & !is.na(n) & round(k/n)*100, 2 != prev), 
                           yes=round((k/n)*100, 2), no=prev))                     %>%  #recalculate prevalence given estimated k
  mutate(national = ifelse(test= region=="national", yes="National", 
                           no="Regional"))                                        %>%  #dichotomous variable: national data or not?
  mutate(medyr = median(c(sampleyr1, sampleyr2), na.rm=T))                             #calculate median sampling year


# Check median sampling year calculated correctly
clean_dat$sampleyr1[which(is.na(clean_dat$sampleyr2))] == clean_dat$medyr[which(is.na(clean_dat$sampleyr2))]

## Calculate 95% CIs
indices <- which(is.na(clean_dat$low_ci))  #which rows to calculate for?
cis <- binom.confint(x=clean_dat$k[indices], n=clean_dat$n[indices], conf.level=0.95, methods="exact")
clean_dat$low_ci[indices] <- round(cis$lower*100, 2)
clean_dat$up_ci[indices]  <- round(cis$upper*100, 2)

## Save ouput to file
saveRDS(clean_dat, "data/cleandat.RDS")

