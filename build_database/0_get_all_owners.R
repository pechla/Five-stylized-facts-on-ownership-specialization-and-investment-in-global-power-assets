rm(list=ls())

source('R-code/source_code/libraries.R')

# all firms from existing datasets
d1 <- read_xlsx('data_CIQ/all_plants_by_owner.xlsx', sheet = 'values') 
d2 <- read_xlsx('data_CIQ/all_units_by_owner.xlsx', sheet = 'values')

all_firms1 <- sort( unique( unlist( d1[,c('Owner Key', 'Ultimate Parent Key')]  ) ) )
all_firms2 <- sort( unique( unlist( d2[,c('Owner Key', 'Ultimate Parent Key')]  ) ) )

# complement with previously company data
dext <- read_xlsx('data_processed/data_temp/SP_parent_information.xlsx', sheet = 'values') %>% as.data.table()

firms3_raw <- sort( unique( dext$`Entity ID` ) ) # note that sometimes there are multiples

firms3_messy <- as.integer( unlist( strsplit(gsub(pattern = ' ',replacement = '', firms3_raw) , ';') ) )
firms3_messy <- firms3_messy[!is.na(firms3_messy)]
all_firms3 <- sort( unique( firms3_messy))

all_firms <- unique(c(all_firms1, all_firms2, all_firms3) )

write_xlsx(data.frame(firm_id = all_firms), 'data_processed/data_temp/all_firm_keys_new.xlsx')


# merge other firm data in that has been collected previously
k1 <- read_xlsx('data_processed/data_temp/all_firm_keys_new.xlsx') %>% as.data.table()
k2 <- read_xlsx('../empirical_investment_dynamics_v3/data_CIQ/build_database_v2/R-data-output/all_firm_keys.xlsx') %>% as.data.table()

k <- rbind( k1, k2)
KEYS <- k[!duplicated(k)]


# save cleaned firm keys
write_xlsx(KEYS, 'data_processed/R-data-output/all_firm_keys.xlsx')


# save all units
d2 <- d2 %>% as.data.table()
write_xlsx( data.table( unit_key = unique( d2[,`Power Plant Unit Key`] ) ), 'data_processed/R-data-output/all_unit_keys.xlsx' )
