###########################
## Supplementary figures ##
###########################

## Load packages
library(ggplot2)
library("dplyr")

## Read in data
df <- readRDS("data/CTout.RDS")

# Plot
ggplot(bind_rows(df, .id="data_frame"), aes(x=age, y=prev*100, group=data_frame)) +
  geom_line(alpha=0.45) + 
  labs(x="Age (years)", y="Seroprevalence (%)") + 
  scale_x_continuous(limits=c(0, 60), breaks=seq(0, 60, 20), expand = c(0,0)) +
  scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, 25), expand=c(0,0)) +
  theme_light()

## Save plot
#PDF
ggsave(filename = "plots/age-seroprev.pdf",
       device = cairo_pdf, height = 6, width = 6, units = "in")

# PNG
ggsave(filename = "plots/age-seroprev.png",
       height = 6, width = 6, units = "in", dpi=600)
