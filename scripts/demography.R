#######################################
## Country-specific demographic data ##
#######################################

### set country & year ###
country <- setcountry

# 5-year band
year <- "2015-2020"

# single year (first timepoint)
year1 <- "2020"

library(wpp2019)

### load required datasets
data(percentASFR) # age distribution of fertility rates (percentage)
data(popF) # population distribution females

### generate age categories
tmpdf <- subset(popF, name==country)

## Extract min & max age from character strings
min_age <- max_age <- vector(length=nrow(tmpdf["age"]))

for(i in 1:nrow(tmpdf["age"])) {
  
  if( nchar(tmpdf["age"][i,]) < 4 ) {
    
  min_age[i] <- substr(tmpdf["age"][i,], 1, 1)
  max_age[i] <- substr(tmpdf["age"][i,], 3, 3)
  
  } else if ( nchar(tmpdf["age"][i,]) > 4 ) {
    
    min_age[i] <- substr(tmpdf["age"][i,], 1, 2)
    max_age[i] <- substr(tmpdf["age"][i,], 4, 5)
    
  }
  
}

## Create age midpoints
# remove final age group (=NA)
min_age <- min_age[-length(min_age)]
max_age <- max_age[-length(max_age)]

# make numeric
min_age <- as.numeric(min_age)
max_age <- as.numeric(max_age)

# calculate midpoints
mid_age <- min_age + (max_age - min_age)/2

# remove old objects
rm(min_age, max_age)

### age-specific births rates
age_pop_cat <- tmpdf[,"age"]
tmpdf <- subset(percentASFR, name==country)
age_fert_cat <- tmpdf[,"age"]
index <- !is.na(match(age_pop_cat,age_fert_cat))
index <- index[-length(index)]

prop_fert_age <- rep(0,length=length(index))
prop_fert_age[index] <- tmpdf[,year]/100
rm(index, age_pop_cat, age_fert_cat)
