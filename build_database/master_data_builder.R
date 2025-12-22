rm(list=ls())

source('build_database/0_get_all_owners.R')
source('build_database/1_build_basic_data.R')
source('build_database/2_company_aggregation.R')
source('build_database/3_build_regression_data.R')



#### DATA OVERVIEW TABLE 1 #### 
source('R-code/source_code/libraries.R')

d1 <- fread('data_processed/R-data-output/unit_tech_info_detail.csv')
d1$unit %>% unique() %>% length
d1$plant_id %>% unique %>% length()


d2 <- fread('data_processed/R-data-output/unit_owner_year.csv')
d2$owner_id %>% unique() %>% length()
d3 <- fread('data_processed/R-data-output/unit_parent_year.csv')
d3$firm_id %>% unique() %>% length()


d3temp1 <- d3[,.(unit_id, firm_id)]
d3temp1[!duplicated(d3temp1)] %>% nrow

d4 <- fread("data_processed/R-data-output/unit_timeseries_detail.csv")
d4temp1 <- d4[, .(firm_id, tech)]
d4temp1[!duplicated(d4temp1)] %>% nrow

d4temp2 <- d4[, .(firm_id, tech, year)]
d4temp2[!duplicated(d4temp2)] %>% nrow


d4invest <- dcast( d4, firm_id + unit_id  ~ year, value.var = 'mw', fill = 0 )
capchange <- d4invest[, .SD, .SDcols = as.character(2002:2024)] - d4invest[, .SD, .SDcols = as.character(2001:2023)]

sum(capchange>0)
sum(capchange<0)
