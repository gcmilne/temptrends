###########
## Plots ##
###########

## Load packages
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)  #to show colour palette

## Read in cleaned data
dat <- readRDS("data/cleandat.RDS")

## Basic summary stats:
## (1) How many people in the dataset?
sum(dat$n[which(!is.na(dat$n))]) + 2209  #2209 is n for QC, Canada (n not available separately for each timepoint)

## (2) Years over which the data were collected?
print(paste(min(dat$sampleyr1), sort(dat$sampleyr2)[length(sort(dat$sampleyr2))], sep="-"))

## (3) World Bank income statuses of countries in the dataset (as of 2021)?  
wb <- read.csv("data/WB21-data.csv", fileEncoding="UTF-8-BOM")

# make country names comparable between datasets
sorted_countries           <- as.character(sort(unique(dat$country)))
sorted_countries[c(which(sorted_countries == "England"), 
                   which(sorted_countries == "Scotland"))] <- "United Kingdom"
sorted_countries[which(sorted_countries == "South Korea")] <- "Korea, Rep."
sorted_countries[which(sorted_countries == "USA")]         <- "United States"

# find income status for each country
incomes             <- vector("character", length(sorted_countries))

for (i in 1:length(sorted_countries)) {
  incomes[i] <- wb$Income.group [ which(wb$Country == sorted_countries[i])]
}

# how many countries in each income bracket?
length(incomes [which(incomes == "High income")])          #how many high income countries?
length(incomes [which(incomes == "Upper middle income")])  #how many upper-middle income countries?

## Create colour scale for plots
dat$country <- as.factor(dat$country)
nlevels(dat$country)   #how many countries in the dataset?

# https://medialab.github.io/iwanthue/
palette <- c("#bf4a5c",
             "#cc5339",
             "#a04730",
             "#c28140",
             "#d1972c",
             "#9f9743",
             "#99b43f",
             "#54903d",
             "#59bf7a",
             "#43c8ac",
             "#3ba7e5",
             "#5e7ecb",
             "#6f71d9",
             "#503183",
             "#9d6dbf",
             "#d88ad2",
             "#bf55b2",
             "#892961",
             "#d76092")
# show_col(palette)  #visualise palette

names(palette) <- levels(dat$country)  #create fixed factor colours
my_scale <- scale_colour_manual(name = "Country", values = palette, limits=force)  #specify scale_fill_manual


## SUBPOPULATION PLOTS ##
p <- vector("list", length=6)

# aesthetics
line_size  <- 0.7
point_size <- 1.2

## Plots for pregnant women
p[[1]] <- dat %>%
  filter(population == "Pregnant women") %>%
  ggplot(aes(x=medyr, y=prev, col=country, line=region, shape=national)) + 
  geom_point(size=point_size) +
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), width=0.3) + 
  geom_smooth(method="lm", se=F, size=line_size) +
  labs(title="Pregnant women", x="Year", y="Seroprevalence (%)", shape="Region") + 
  scale_x_continuous(limits=c(1960, 2020), breaks=seq(1960, 2020, 20), expand = c(0,0)) + 
  scale_y_continuous(limits=c(0, 75), breaks=seq(0,75,25), expand = c(0,0)) + 
  theme_light() +
  my_scale

## Plots for childbearing-age women
p[[2]] <- dat %>%
  filter(population == "Childbearing-age women") %>%
  ggplot(aes(x=medyr, y=prev, col=country,  shape=national)) + 
  geom_point(size=point_size) +
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), width=0.3) + 
  geom_smooth(method="lm", se=F, size=line_size) +
  labs(title="Childbearing-age women", x="Year", y="Seroprevalence (%)", shape="Region") + 
  scale_x_continuous(limits=c(1960, 2020), breaks=seq(1960, 2020, 20), expand = c(0,0)) + 
  scale_y_continuous(limits=c(0, 75), breaks=seq(0,75,25), expand = c(0,0)) + 
  theme_light() +
  my_scale


## Plots for general population
p[[3]] <- dat %>%
  filter(population == "General population") %>%
  ggplot(aes(x=medyr, y=prev, col=country, line=region, shape=national)) + 
  geom_point(size=point_size) + 
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), width=0.3) + 
  geom_smooth(method="lm", se=F, size=line_size, formula = y~x) +
  labs(title="General population", x="Year", y="Seroprevalence (%)", shape="Region") + 
  scale_x_continuous(limits=c(1960, 2020), breaks=seq(1960, 2020, 20), expand = c(0,0)) + 
  scale_y_continuous(limits=c(0, 75), breaks=seq(0,75,25), expand = c(0,0)) + 
  theme_light() + 
  my_scale
  
  
## Plots for blood donors
p[[4]] <- dat %>%
  filter(population == "Blood donors") %>%
  ggplot(aes(x=medyr, y=prev, col=country, line=region, shape=national)) + 
  geom_point(size=point_size) + 
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), width=0.3) + 
  geom_smooth(method="lm", se=F, size=line_size) +
  labs(title="Blood donors", x="Year", y="Seroprevalence (%)", shape="Region") + 
  scale_x_continuous(limits=c(1960, 2020), breaks=seq(1960, 2020, 20), expand = c(0,0)) + 
  scale_y_continuous(limits=c(0, 75), breaks=seq(0,75,25), expand = c(0,0)) + 
  theme_light() + 
  my_scale

## Plots for hospital attendees
p[[5]] <- dat %>%
  filter(population == "Hospital antendees") %>%
  ggplot(aes(x=medyr, y=prev, col=country)) + 
  geom_point(size=point_size) + 
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), width=0.3) + 
  geom_smooth(method="lm", se=F, size=line_size) +
  labs(title="Hospital antendees", x="Year", y="Seroprevalence (%)") + 
  scale_x_continuous(limits=c(1960, 2020), breaks=seq(1960, 2020, 20), expand = c(0,0)) + 
  scale_y_continuous(limits=c(0, 75), breaks=seq(0,75,25), expand = c(0,0)) + 
  theme_light() +
  my_scale


## Plots for military personnel
p[[6]] <- dat %>%
  filter(population == "Military personnel") %>%
  ggplot(aes(x=medyr, y=prev, col=country, shape=subpopulation)) + 
  geom_point(size=point_size) + 
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), width=0.3) + 
  geom_smooth(method="lm", se=F, size=line_size) +
  labs(title="Military personnel", x="Year", y="Seroprevalence (%)", shape="Popuation") + 
  scale_x_continuous(limits=c(1960, 2020), breaks=seq(1960, 2020, 20), expand = c(0,0)) + 
  scale_y_continuous(limits=c(0, 75), breaks=seq(0,75,25), expand = c(0,0)) + 
  theme_light() +
  my_scale

# population multipanel plot
wrap_plots(p, ncol=2) &  
  plot_annotation(tag_levels = list(c('(a)', '(b)', '(c)', '(d)', '(e)', '(f)'))) & 
  theme(legend.spacing.y = unit(0.05, "cm"))  #reduce space between fig legends

## Save
# PDF
ggsave(filename = "plots/pop_trends.pdf", width = 10, height = 10, 
       device = cairo_pdf, units = "in")

# PNG
ggsave(filename = "plots/pop_trends.png", width = 10, height = 10, dpi=600,
       units = "in")
