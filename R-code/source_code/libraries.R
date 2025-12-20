library(readxl)
library(writexl)
library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(data.table)
library(gridExtra)
library(grid)
library(scales)
library(stargazer)
library(ineq)
library(xtable)
library(mfx)
library(texreg)
library(effects)
library(sandwich)
library(car)
library(ggrepel)
library(lubridate)
library(readr)

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

yrs <- 2001:2023
yrs_ch <- as.character(yrs)
