# READ AND WRITE XLSX FILES
library(readxl)
library(writexl)

# VISUALIZATION TOOLS
library(ggplot2); theme_set(theme_bw())
library(ggtext)
library(ggrepel)
library(gridExtra)
library(grid)
library(scales)
library(viridis)
library(readr)

# DATA ANALYSIS
library(data.table)
library(dplyr)
library(car)
library(lubridate)
library(tidyr)

# INEQUALITY METRICS
library(ineq)

# LOGISTIC + LINEAR REGRESSION
library(mfx)
library(effects)
library(sandwich)

# OUTPUT TABLES AND REGRESSION RESULTS
library(stargazer)
library(xtable)
library(texreg)

# FITTING LOGNORMAL MIXTURE MODEL
library(mixtools)

# DECISION TREE
library(tree)
library(rpart)
library(rpart.plot)



#### OTHER UNIVERSAL HELPERS ####

# tech size buckets in portfolio
bucket_fun <- function(x){
  
  ifelse(x <= 10^(-8), '0', ifelse(
    x > 10^(-8) & x <= 0.25, '1', ifelse(
      x > 0.25 & x <= 0.5, '2', ifelse(
        x > 0.5 & x <= 0.75, '3', ifelse(
          x > 0.5 & x <= 0.999999, '4', '5'
        )
      ))))
}


tech_rank_short <- c("PV", "Solar", "Offshore", "Onshore", "Bio", "Hydro", "Geo", "Waste", "Nuclear", "Oil", "Gas", "Coal")
tech_col_short <- c('orange2', 'yellow', 'lightgreen', 'darkgreen', 'burlywood2', 'dodgerblue', 'firebrick', 'grey79',  'magenta',  'brown2',  'darkgrey', 'black') 
names(tech_col_short) <- tech_rank_short


tech_rankings <- c("Solar PV", "Solar Thermal", "Wind Offshore", "Wind Onshore", "Biomass", "Hydro", "Geothermal", "Waste", "Nuclear", "Oil", "Gas", "Coal")
tech_col <- c('orange2', 'yellow', 'lightgreen', 'darkgreen', 'burlywood2', 'dodgerblue', 'firebrick', 'grey79',  'magenta',  'brown2',  'darkgrey', 'black') 
names(tech_col) <- tech_rankings

fnorm <- function(x) x/sum(x)

yrs <- 2001:2024
yrs_ch <- as.character(yrs)
