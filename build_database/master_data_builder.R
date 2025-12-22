rm(list=ls())

source('build_database/0_get_all_owners.R')
source('build_database/1_build_basic_data.R')
source('build_database/2_company_aggregation.R')
source('build_database/3_build_regression_data.R')
