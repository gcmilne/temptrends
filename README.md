TempTrends
===================
### Authors

Gregory C. Milne, Joanne P. Webster, Martin Walker

### Project description
Investigating temporal trends in anti-Toxoplasma gondii IgG seroprevalence in human populations in different countries, regions, populations, and subpopulations. 

### Description of R scripts

#### (1) clean_data.R

A script for cleaning the imported data (.csv) file, including removing rows containing important missing information, and calculating median sampling years and 95% confidence intervals. The output of this script is an .RDS file that is imported into subsequent scripts.

#### (2) plots.R

A script to plot the cleaned data (from clean_data.R) according to different populations. The ouputs of this script are .PNG and .PDF multipanel plots of seroprevalence over time among various countries, regions, populations, and subpopulations.


#### (3) ct_toy_example.R

For illustrating the relationship between the rate of exposure, seroprevalence, and incidence of congenital toxoplasmosis using a toy model. Inputs are the year and country for the demographic data (which are loaded from "demography.R"). The ouput of the script is a multipanel figure (shown in Box I in the article). 

#### (4) demography.R

A script to calculate demographic data (age-specific population sizes & fertility rates) for a given country and year (set in "ct_toy_example.R"). This script is source()'d in "ct_toy_example.R". 
