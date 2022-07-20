TempTrends
===================
### Authors

Gregory C. Milne, Joanne P. Webster, Martin Walker

### Project description
Scripts used for investigating temporal trends in anti-Toxoplasma gondii seroprevalence in human populations around the world. 

### Description of R scripts

#### (1) clean_data.R

A script for cleaning the imported data (.csv) file, including removing rows containing important missing information, and calculating median sampling years and 95% confidence intervals. The output of this script is a saved .RDS file which is imported into subsequent scripts.

#### (2) plots.R

A script to plot the cleaned data (from clean_data.R) according to different populations. The ouput of this script are .PNG and .PDF multipanel plots of seroprevalence over time among various populations.


#### (3) ct_toy_example.R

For illustrating the relationship between the rate of exposure, seroprevalence, and incidence of congenital toxoplasmosis using a toy model. Inputs are the year and country for the demographic data (which are loaded from "demography.R"). The ouput of the script is multipanel figure (shown in Box 1 in the article). 

#### (4) demography.R

A script to calculate demographic data (age-specific population sizes & fertility rates) for a given country and year (set in "ct_toy_example.R"). This script is source()'d in "ct_toy_example.R". 
