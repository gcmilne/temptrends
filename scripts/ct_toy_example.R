##############################################
## Seroprevalence-CT incidence relationship ##
##############################################

## Load packages
library(ggplot2)
library(patchwork)

## Set the country
setcountry <- "United Kingdom"

## Load demographic data
source("scripts/demography.R")

## Create data
age <- seq(0,60, by=0.75)  #age in years (increases in 9-month intervals)
dat <- setNames(data.frame(matrix(nrow=length(age), ncol=4)), 
                c("age", "n", "new_infections", "prev"))
dat$age <- age
dat$prev[1] <- 0            #everyone seronegative at birth
dat$new_infections[1] <- 0  #no new infections at age 0
dat$n <- rep(100, length(dat$new_infections)) #100 women in each age cat
# plot(dat$age, dat$n)

mctr <- 0.44 #mother-child transmission rate

# estimate fertility distribution
propfert <- approx(mid_age, prop_fert_age, xout=dat$age)$y
propfert[is.na(propfert)] <- 0
propfert <- propfert/sum(propfert)

# create df to store output from simulations
out     <- setNames(data.frame(matrix(ncol=3, nrow=100)), 
                    c("foi", "prev", "ct"))
out$foi <- seq(0, 0.2, length.out=nrow(out))

## make data as list to store age-seroprevalence curves
dat <- rep(list(dat), nrow(out))

## loop through each foi value to calculate prevalence & CT incidence
for (k in 1:nrow(out)){
  
  ## Generate age-seroprevalence curve
  for(i in 2:nrow(dat[[k]])) {
    
    dat[[k]]$new_infections[i] <- dat[[k]]$n[i-1] * (1-dat[[k]]$prev[i-1]) * out$foi[k]
    dat[[k]]$prev[i] <- sum(dat[[k]]$new_infections[1:i]) / dat[[k]]$n[i]
    if(dat[[k]]$prev[i]   > 1) dat[[k]]$prev[i] <- 1  #not possible to get prevalence >100%

  }
  
  ## Estimate no. CT cases
  seroconv  <- dat[[k]]$new_infections * propfert
  ct_cases  <- seroconv*mctr
  no_births <- sum(dat[[k]]$n * propfert)
  
  # Incidence of CT per 10,000 live births
  out$ct[k] <- (sum(ct_cases)/no_births)*10000
  
  # Demographically weighted seroprevalence in childbearing ages
  out$prev[k] <- round(weighted.mean(x = dat[[k]]$prev, w = propfert)*100, 2)

}


## Plot relationship between seroprevalence and CT incidence
p1 <- ggplot(data=out, aes(x=prev, y=ct)) + 
  geom_point() + 
  labs(x="Seroprevalence in childbearing-age women (%)", y="CT incidence per 10,000 births") + 
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 20), expand = expansion(mult=c(0, .005))) + #make sure x-axis visible on rhs
  scale_y_continuous(limits=c(0, 60), breaks=seq(0, 60, 10), expand = c(0,0)) +
  theme_light()


### Other plots
## Generate new variables
out$avginf  <- round(1/out$foi, 1)    #avg age of 1st infection
out$probinf <- round(out$foi*10000, 2)  #annual prob of infection per 10,000 people

# Trim data for plotting
plotdat       <- out
row_index <- which(plotdat[,"avginf"] < 100) #include only those with avg infection age <100 years
plotdat       <- plotdat[row_index, ]

# Add % pregnant & age
indices <- which(propfert != 0)

pfert <- propfert[indices]
afert <- dat[[1]]$age[indices]
datfert <- data.frame(afert, pfert)

## Annual risk vs. avg age of infection

## express as per 10,000 live births
p2 <- ggplot(data=plotdat, aes(x=probinf, y=avginf)) + 
  geom_point(size=1) + 
  # geom_smooth(formula=y~1/I(x/100), se=T, n=1000) + 
  labs(x="Incidence of infection per 10,000", y="Average age of first infection") + 
  # scale_x_continuous(limits=c(1, 20), breaks=c(seq(1, 4, 1), seq(5, 20, 5)), expand = c(0,0)) + 
  scale_y_continuous(limits=c(0, 100), breaks=c(0, seq(20, 40, 10), seq(60, 100, 20)), expand = c(0,0)) + 
  theme_light()

## Fertility distribution
p3 <- ggplot(data=datfert, aes(x=afert, y=pfert)) + 
  geom_area(alpha=0.5) + 
  labs(x="Age (years)", y="Probability density") + 
  scale_x_continuous(limits=c(10, 52), breaks=seq(10, 50, 10), expand = c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_light() +
  theme(plot.margin=grid::unit(c(0,0,0,0),"cm"))

# panel layout
design <- "11
           23
           23"

## make multipanel plot
wrap_plots(p1, p2, p3) + plot_layout(design = design) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(face = "bold"))

## Save plot
#PDF
ggsave(filename = "plots/box1_multipanel.pdf",
       device = cairo_pdf, height = 8, width = 8, units = "in")

# PNG
ggsave(filename = "plots/box1_multipanel.png",
       height = 8, width = 8, units = "in", dpi=600)

## Save age-seroprevalence curves
saveRDS(dat, file="data/CTout.RDS")
