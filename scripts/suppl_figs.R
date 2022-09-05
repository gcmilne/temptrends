###########################
## Supplementary figures ##
###########################

## Load packages
library(ggplot2)
library("dplyr")

## Read in data
df <- readRDS("data/CTout.RDS")

plot(df[[1]]$age, df[[1]]$prev, 'l', ylim=c(-0.1, 1.1),  xlab="Age (years)", ylab="Seroprevalence")
for (k in 2:nrow(out)) lines(df[[k]]$age, df[[k]]$prev)

# Plot
ggplot(bind_rows(df, .id="data_frame"), aes(x=age, y=prev*100, group=data_frame)) +
  geom_line(alpha=0.4) + 
  labs(x="Age (years)", y="Seroprevalence (%)") + 
  scale_x_continuous(limits=c(0, 60), breaks=seq(0, 60, 20), expand = c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_light()

## Save plot
#PDF
ggsave(filename = "plots/age-seroprev.pdf",
       device = cairo_pdf, height = 6, width = 6, units = "in")

# PNG
ggsave(filename = "plots/age-seroprev.png",
       height = 6, width = 6, units = "in", dpi=600)
