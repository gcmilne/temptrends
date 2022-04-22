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

## Create colour scale
dat$country <- as.factor(dat$country)
nlevels(dat$country)  #how many countries?

# https://medialab.github.io/iwanthue/
palette <- c("#bf6376",
             "#d54146",
             "#c56659",
             "#d86222",
             "#bf773b",
             "#a78858",
             "#c5ad2e",
             "#92903c",
             "#62b242",
             "#5a9f5d",
             "#45af9c",
             "#7689c8",
             "#7069d5",
             "#ba5cc0",
             "#bf6ca4",
             "#d04080")

show_col(palette)

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
  ggplot(aes(x=medyr, y=prev, col=country)) + 
  geom_point(size=point_size) +
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), width=0.3) + 
  geom_smooth(method="lm", se=F, size=line_size) +
  labs(title="Pregnant women", x="Year", y="Seroprevalence (%)") + 
  scale_x_continuous(limits=c(1960, 2020), breaks=seq(1960, 2020, 20), expand = c(0,0)) + 
  scale_y_continuous(limits=c(0, 75), breaks=seq(0,75,25), expand = c(0,0)) + 
  theme_light() +
  my_scale

## Plots for childbearing-age women
p[[2]] <- dat %>%
  filter(population == "Childbearing-age women") %>%
  ggplot(aes(x=medyr, y=prev, col=country)) + 
  geom_point(size=point_size) +
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), width=0.3) + 
  geom_smooth(method="lm", se=F, size=line_size) +
  labs(title="Childbearing-age women", x="Year", y="Seroprevalence (%)") + 
  scale_x_continuous(limits=c(1960, 2020), breaks=seq(1960, 2020, 20), expand = c(0,0)) + 
  scale_y_continuous(limits=c(0, 75), breaks=seq(0,75,25), expand = c(0,0)) + 
  theme_light() +
  my_scale


## Plots for general population
p[[3]] <- dat %>%
  filter(population == "General population") %>%
  ggplot(aes(x=medyr, y=prev, col=country, shape=national)) + 
  geom_point(size=point_size) + 
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), width=0.3) + 
  geom_smooth(method="lm", se=F, size=line_size) +
  labs(title="General population", x="Year", y="Seroprevalence (%)", shape="") + 
  scale_x_continuous(limits=c(1960, 2020), breaks=seq(1960, 2020, 20), expand = c(0,0)) + 
  scale_y_continuous(limits=c(0, 75), breaks=seq(0,75,25), expand = c(0,0)) + 
  theme_light() + 
  my_scale
  

## Plots for blood donors
p[[4]] <- dat %>%
  filter(population == "Blood donors") %>%
  ggplot(aes(x=medyr, y=prev, col=country)) + 
  geom_point(size=point_size) + 
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), width=0.3) + 
  geom_smooth(method="lm", se=F, size=line_size) +
  labs(title="Blood donors", x="Year", y="Seroprevalence (%)") + 
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
  plot_annotation(tag_levels = list(c('(a)', '(b)', '(c)', '(d)', '(e)', '(f)')))
  

## Save as PDF
ggsave(filename = "plots/pop_trends.pdf", width = 8, height = 8, 
       device = cairo_pdf, units = "in")

