rm(list=ls())

library(readxl)
library(writexl)
library(data.table)
library(dplyr)

d1 <- read_xlsx('../CIQ_download/all_plants_by_owner.xlsx', sheet = 'values') 
d2 <- read_xlsx('../CIQ_download/all_units_by_owner.xlsx', sheet = 'values')

all_firms1 <- sort( unique( unlist( d1[,c('Owner Key', 'Ultimate Parent Key')]  ) ) )
all_firms2 <- sort( unique( unlist( d2[,c('Owner Key', 'Ultimate Parent Key')]  ) ) )

# complement with previously company data
dext <- read_xlsx('../CIQ_download/company_data_extended/VALUES_parent_information.xlsx') %>% as.data.table()

firms3_raw <- sort( unique( dext$`Entity ID` ) ) # note that sometimes there are multiples

firms3_messy <- as.integer( unlist( strsplit(gsub(pattern = ' ',replacement = '', firms3_raw) , ';') ) )
firms3_messy <- firms3_messy[!is.na(firms3_messy)]
all_firms3 <- sort( unique( firms3_messy))

all_firms <- unique(c(all_firms1, all_firms2, all_firms3) )

write_xlsx(data.frame(firm_id = all_firms), 'R-data-output/all_firm_keys.xlsx')

