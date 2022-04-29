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
age <- seq(0,60)  #age in years
dat <- setNames(data.frame(matrix(nrow=length(age), ncol=4)), 
                c("age", "n", "new_infections", "prev"))
dat$age <- age
dat$prev[1] <- 0            #everyone seronegative at birth
dat$new_infections[1] <- 0  #no new infections at age 0

# estimate female population size in each age category
f     <- spline(mid_age, pop_f, xout=dat$age)
dat$n <- f$y
mctr <- 0.4 #mother-child transmission rate
# plot(dat$age, dat$n)

# estimate fertility distribution
propfert <- approx(mid_age, prop_fert_age, xout=dat$age)$y
propfert[is.na(propfert)] <- 0
propfert <- propfert/sum(propfert)
# plot(dat$age, propfert)

## Create output dataset
out <- setNames(data.frame(matrix(ncol=3, nrow=100)), 
                c("foi", "prev", "ct"))

out$foi <- seq(0, 0.2, length.out=nrow(out))

## loop through each foi value to calculate prevalence & CT incidence
for (k in 1:nrow(out)){
  
  ## Generate age-seroprevalence curve
  for(i in 2:nrow(dat)) {
    
    dat$new_infections[i] <- dat$n[i] * (1-dat$prev[i-1]) * out$foi[k]
    dat$prev[i] <- sum(dat$new_infections[1:i]) / dat$n[i]
    
  }
  
  # plot(dat$age, dat$prev, 'l', ylim=c(0,1), xlab="age", ylab="seroprevalence",
  #      main=paste("FoI=", out$foi[k], sep=""))  #plot curve
  
  ## Estimate no. CT cases
  seroconv  <- dat$new_infections * propfert
  ct_cases  <- seroconv*mctr
  no_births <- sum(dat$n * propfert)
  
  # Incidence of CT per 10,000 live births
  out$ct[k] <- (sum(ct_cases)/no_births)*10000
  
  indices     <- which(propfert !=0)
  
  # Demographically weighted seroprevalence in childbearing ages
  out$prev[k] <- round(weighted.mean(x = dat$prev[indices], w = propfert[indices] * dat$n[indices])*100, 2)
  
}

## Plot relationship between seroprevalence and CT incidence
figIa <- ggplot(data=out, aes(x=prev, y=ct)) + 
  geom_line() + 
  labs(x="Seroprevalence in childbearing-age women (%)", y="CT incidence") + 
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 20), expand = expansion(mult=c(0, .005))) + #make sure x-axis visible on rhs
  scale_y_continuous(limits=c(0, 65), breaks=seq(0, 60, 20), expand = c(0,0)) +
  theme_light()

### Other plots
## Generate new variables
out$avginf  <- round(1/out$foi, 1)    #avg age of 1st infection
out$probinf <- round(out$foi*100, 2)  #annual prob (%) of infection

# Trim data for plotting
plotdat       <- out
row_index <- which(plotdat[,"avginf"] < 100) #include only those with avg infection age <100 years
plotdat       <- plotdat[row_index, ]

# Add % pregnant & age
indices <- which(propfert != 0)

pfert <- propfert[indices]*100
afert <- dat$age[indices]
datfert <- data.frame(afert, pfert)

## Annual risk vs. avg age of infection
p1 <- ggplot(data=plotdat, aes(x=probinf, y=avginf)) + 
  geom_point(size=1) + 
  # geom_smooth(formula=y~1/I(x/100), se=T, n=1000) + 
  labs(x="Annual per capita probability of infection (%)", y="Average age of first infection") + 
  scale_x_continuous(limits=c(1, 20), breaks=c(seq(1, 4, 1), seq(5, 20, 5)), expand = c(0,0)) + 
  scale_y_continuous(limits=c(0, 100), breaks=c(0, seq(20, 40, 10), seq(60, 100, 20)), expand = c(0,0)) + 
  theme_light()

## Fertility distribution
p2 <- ggplot(data=datfert, aes(x=afert, y=pfert)) + 
  geom_area(alpha=0.5) + 
  labs(x="Age (years)", y="Proportion pregnant (%)") + 
  scale_x_continuous(limits=c(10, 51), breaks=seq(10, 50, 10), expand = c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_light() +
  coord_flip() + 
  theme(plot.margin=grid::unit(c(0,0,0,0),"cm"))

## Combine 2 plots together
figIb <- p1 + 
  annotation_custom(
    ggplotGrob(p2), 
    xmin = 7.5, xmax = 17.5, ymin = min(datfert$afert)+2, ymax =  max(datfert$afert)
  )

## Combine in panel
# wrap_plots(figIa, figIb, nrow=1, heights = 20, widths=40) &  
#   plot_annotation(tag_levels = list(c('(a)', '(b)')))

# panel layout
design <- "11
           23
           23"

wrap_plots(figIa, p1, p2) + plot_layout(design = design) + 
  plot_annotation(tag_levels = list(c('(a)', '(b)', '(c)')))


## Save plot
#PDF
ggsave(filename = "plots/box1_multipanel.pdf",
       device = cairo_pdf, height = 8, width = 8, units = "in")

# PNG
ggsave(filename = "plots/box1_multipanel.png",
       height = 8, width = 8, units = "in", dpi=600)

