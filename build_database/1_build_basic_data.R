rm(list = ls())

options(encoding = "UTF-8")

library(readxl)
library(writexl)
library(data.table)
library(dplyr)
library(lubridate)

#### THIS FILE WORKS AS FOLLOWS ####

# 1. Construct asset-owner level data
# 2. Construct asset-tech data
# 3. Restrict asset-owner level data based on asset that could be mapped to tech (2.)
# 4. Take owners from (3.) and map them to parents

# Now we can build data on parent and asset level:
# 1. make asset-parent data
# 2. make asset-tech data
# 3. make parent-info data



#### 1. CONSTRUCT ASSET-OWNER DATA ####
d1 <- read_xlsx('../CIQ_download/all_units_by_owner.xlsx', sheet = 'values') %>% as.data.table()

# easier to access
setnames(d1, 
         c("Ownership Event Date (dd.mm.yyyy)", "Ownership Ended (dd.mm.yyyy)", 
           'Year Unit in Service', 'Year Unit Retired from Service'), 
         c('OD1', 'OD2', 'birth', 'death') )
d1$MW_owned <- d1$`Ownership (%)`/100 * d1$`Current Operating Capacity (MW)`

# reduced data
own_unit_raw <- d1[birth !=0, c('Power Plant Unit Key', 'Owner Key', 'Ultimate Parent Key', 'birth', 'death', 'OD1', 'OD2', 'MW_owned')]
own_unit_raw[death ==0, death := 2025] # assume that if there is no death data plant is still operating
own_unit_raw$OD1 <- as.integer( substr( floor_date( as.Date(own_unit_raw$OD1), 'year' ), 1, 4) )
own_unit_raw$OD2 <- as.integer( substr( floor_date( as.Date(own_unit_raw$OD2), 'year' ), 1, 4) )
own_unit_raw[is.na(OD1), OD1 := birth]
own_unit_raw[is.na(OD2), OD2 := death]
own_unit_raw <- own_unit_raw[!(OD1>OD2),] # sometimes inconsistency as ownership might be already tracked during construction phase before actual birth

own_unit_raw_short <- as.data.frame(own_unit_raw[,c('Power Plant Unit Key', 'Owner Key', 'OD1', 'OD2', 'MW_owned', 'Ultimate Parent Key')])

tt <- Sys.time() 
own_unit_l_raw <- do.call(rbind, apply( own_unit_raw_short, 1, function(x) data.table( unit_id= x[1], 
                                                                                       owner_id= x[2], ultimate_id = x[6],
                                                                                       year = x[3]:x[4], mw = x[5] ) ) )
Sys.time()- tt # Time difference of 34.32764 secs

# make first version of owner-asset level data
own_unit <- own_unit_l_raw[year <=2023 & year >= 2001, .(mw=sum(mw)), by = c('unit_id','owner_id', 'year')]
own_unit <- own_unit[mw>0,]





#### 2. CONSTRUCT ASSET-TECH DATA ####
d2 <- read_xlsx('../CIQ_download/tech_detail_unit_level-by_unit.xlsx', sheet = 'values') %>% as.data.table()
# easier to access
setnames(d2, c(names(d2)[1:12],'Fuel Type', 'Is the Plant Offshore? Yes/No', 'Country / Region Name', 'Country/Region Code', 'Primary Fuel Type', 'Primary Fuel Group'),
         c('plant', 'unit_id', 'tech_type', 'gen_tech', 'tech_detail', 'plant_id', 'name_mw', 'oper_mw', 
           'birth', 'birth_month', 'death', 'death_month',
           'fuel', 'offshore', 'country_name', 'country', 'prim_fuel_type', 'prim_fuel_group') )

# only include plants that are included in ownership data
units <- sort( unique( own_unit$unit_id ) )
d2 <- d2[unit_id %in% units,]
d2[death ==0, death := NA]

### Create aggregate technology categories
fulltech <- d2[, .(N=length(oper_mw), mw=sum(oper_mw)), by = .(tech_type, gen_tech, tech_detail, fuel, prim_fuel_group, prim_fuel_type, offshore)]

fulltech$tech <- as.character()
fulltech[tech_type == 'Geothermal' | fuel == 'Geothermal' | prim_fuel_type == 'Geothermal', tech := 'Geo' ]
fulltech[prim_fuel_type == 'Water',  tech := 'Hydro' ]
fulltech[tech_type == 'Nuclear', tech := 'Nuclear' ]
fulltech[gen_tech == 'Photovoltaic', tech := 'PV' ]
fulltech[gen_tech == 'Solar Thermal',  tech := 'Solar']
fulltech[prim_fuel_type == 'Solar' & tech_type == 'Steam Turbine', tech := 'Solar']
fulltech[fuel == 'Wind' & offshore == 'No',  tech := 'Onshore' ]
fulltech[fuel == 'Wind' & offshore == 'Yes', tech := 'Offshore' ]

# note that fuel and primary_fuel_group can be misleading for thermal plants (e.g., see plant 65589 which is mixed bio and gas but fuel always says Biomass!)
# thus, assign by primary fuel type
fulltech[prim_fuel_type == 'Natural Gas',tech := 'Gas']
fulltech[grep('Nat Gas', fulltech$prim_fuel_type), tech := 'Gas']

fulltech[prim_fuel_type == 'Coal', tech := 'Coal']
fulltech[prim_fuel_type %in% c('Oil', 'Refinery Gas'), tech := 'Oil']

fulltech[is.na(tech) & prim_fuel_group == 'Coal', tech := 'Coal'] # they are all coal-based
fulltech[grep('Coal-Derived', fulltech$prim_fuel_type), tech := 'Coal']
fulltech[is.na(tech) & prim_fuel_type %in% c('Coke Oven Gas', 'Blast Furnace Gas'), tech := 'Coal'] # they are all coal-based

fulltech[is.na(tech) & prim_fuel_group == 'Oil', tech := 'Oil'] # they are all oil-based
fulltech[grep('Pet Coke', fulltech$prim_fuel_type), tech := 'Oil']

fulltech[ grep('bio', fulltech$prim_fuel_type, ignore.case = T) , tech:= 'Bio']
fulltech[prim_fuel_type %in% c('Agri Crop Byproducts', 'Wood', 'Wood Waste Liquids', 'Wood Waste Solids', 'Black Liquor'), tech:= 'Bio']

fulltech[prim_fuel_type %in% c('Landfill Gas', 'Muni Solid Waste', 'Sludge Waste', 'Tires'), tech := 'Waste']

fulltech[prim_fuel_type == 'Waste Heat' & is.na(tech) & fuel == 'Coal', tech := 'Coal' ]
fulltech[prim_fuel_type == 'Waste Heat' & is.na(tech) & fuel == 'Gas', tech := 'Gas' ]
fulltech[prim_fuel_type == 'Waste Heat' & is.na(tech) & fuel == 'Oil', tech := 'Oil' ]
fulltech[prim_fuel_type == 'Waste Heat' & is.na(tech) & fuel == 'Biomass', tech := 'Bio' ]

fulltech[is.na(tech), ][order(mw)] # these we'll remove

# now merge tech into d2
fulltech$identifier <- paste( fulltech$tech_type, fulltech$gen_tech, fulltech$tech_detail, fulltech$fuel, fulltech$prim_fuel_group, fulltech$prim_fuel_type, fulltech$offshore)
d2$identifier <- paste( d2$tech_type, d2$gen_tech, d2$tech_detail, d2$fuel, d2$prim_fuel_group, d2$prim_fuel_type, d2$offshore)


d2 <- merge(d2, fulltech[,c('identifier', 'tech')], by = 'identifier')
d2 <- d2[!is.na(tech)] # remove assets for which we don't have aggregate tech


# make first version of tech-asset data
tech_unit <- d2[, c('unit_id', 'plant_id', 'plant', 'tech', 'oper_mw', 'birth', 'death', 'country')]
tech_unit_clean <- d2[, c('unit_id', 'plant_id', 'plant', 'tech', 'oper_mw', 'birth', 'death', 'country_name', 'country', 'Latitude (degrees)','Longitude (degrees)', 'Power Control Area', 'Regional Transmission Organization', 'Interconnected Utility Company')]
setnames(tech_unit_clean, c('Latitude (degrees)','Longitude (degrees)'), c('lat', 'long'))


#### 3. restrict owner-asset data to only include tech-mapped assets ####
own_unit <- own_unit[unit_id %in% unique(tech_unit$unit_id),]

# recall that we now have create two basic data sets:
# 1. own_unit
# 2. tech_unit

# next, we need to map owners to ultimate parents
d3 <- read_xlsx('../CIQ_download/owners_information.xlsx', sheet = 'values') %>% as.data.table()
new_cols <- c('entity_id', 'name', 'short', 'mi_key', 'isin', 'lei', 'type', 'establ', 'incorp', 'status', 
              'sic', 'ind', 'ind_prim', 'ind1', 'ind2', 'ind3', 'ind4', 
              'geo', 'country_name', 'country', 'subregion', 'region', 'state_own', 'priv_comp_own', 'pub_comp_own',
              'parent', 'parent_id', 'parent_share', 'ultimate', 'ultimate_id')
data.table(names(d3), new_cols)
setnames(d3, new= new_cols)
# remove unknowns
d3 <- d3[name != '#INVALID COMPANY ID']
# fix those per definition that do not have parent
d3[parent_id ==0 & status == 'Operating Subsidiary', `:=`(status = 'Operating')  ] # not many

# map all operating subsidiaries which have single parents 
# (for multiple parent owners we use the ultimate parents)
sub_single <- d3[ !grepl(';', parent_id) & status == 'Operating Subsidiary', c('entity_id', 'parent_id')]
sub_single[, `:=`(parent_id = as.integer(parent_id)) ]

# we will snowball through ownership tree
# we use the d3 data as baseline but remove all multiple parent firms
d3_edit <- d3[ -grep(';', parent_id), ]
d3_edit[, `:=`(parent_id = as.integer(parent_id)) ]

ownerstruct <- data.table( lev0 = sub_single$entity_id )
mapped_owners_list <- list()
for (i in 1:10) {
  #  i<-1
  baselevel <- ownerstruct[,.SD, .SDcols = i]
  x.merge <- colnames(ownerstruct)[i]
  
  ownerstruct <- merge(ownerstruct, d3_edit[,c('entity_id', 'parent_id')], by.x = x.merge, by.y = 'entity_id', all.x=T)
  
  setnames(ownerstruct, old = 'parent_id', new = paste0( 'lev', i ) )
  
  d3_edit[ entity_id %in% ownerstruct[, get(paste0( 'lev', i ))] & status != "Operating Subsidiary", ]
  
  ultimate_idx <- ownerstruct[, get(paste0( 'lev', i ))] %in% d3[status != 'Operating Subsidiary', entity_id]
  
  mapped_owners_list[[i]] <- ownerstruct[ultimate_idx, .SD, .SDcols = c('lev0', paste0( 'lev', i ))]
  
  ownerstruct <- ownerstruct[!ultimate_idx,]
  
  
}
# in this file are now all the firms that we could not map
ownerstruct <- ownerstruct[, .SD, .SDcols = paste0('lev',0:(ncol(ownerstruct)-1))]

# in mapped_owners_list all the mapped relationships are given

# the ones in ownerstruct are not mapped yet -> we use their ultimate parents in this case
d3_custom <- d3[entity_id %in% ownerstruct$lev0 & ultimate_id ==0, ultimate_id := parent_id ] # only applies to a single entity
mapped_owner_df <- merge( ownerstruct[,'lev0'], d3[,c('entity_id', 'ultimate_id')], by.x = 'lev0', by.y = 'entity_id', all.x =T )

## constructing the full map
setnames(mapped_owner_df, 'ultimate_id', 'lev_final')

owner_map <- 
  rbind(
    do.call(rbind, lapply( mapped_owners_list, function(x) setnames(x, new = c('lev0', 'lev_final')) ) ),
    mapped_owner_df )
owner_map[, lev_final := as.integer(lev_final)]


# now bring owner-asset data at level of parent-asset
par_unit_raw_v1 <- copy( own_unit_l_raw )
par_unit_raw_v1 <- merge(par_unit_raw_v1, owner_map, by.x = 'owner_id', by.y = 'lev0', all.x = T )
par_unit_raw_v1[!is.na(lev_final), ultimate_id := lev_final]

# # example where new ultimate is assigned to asset-level data (only works before redefining above)
# par_unit_raw_v1[!is.na(lev_final) & ultimate_id != lev_final,]
# d3[entity_id %in% c(102000,113983), c('name', 'status', 'parent', 'ultimate')]
# d3[entity_id %in% c(119863911,5001439), c('entity_id','name', 'status', 'parent', 'ultimate')]


# for multi-owned subsidiaries we use asset-level data to aggregate

# now aggregate the parent-asset data
par_unit <- par_unit_raw_v1[year <=2023 & year >= 2001, .(mw=sum(mw)), by = c('unit_id', 'ultimate_id', 'year')]
par_unit <- par_unit[mw>0,]

# restrict owner-asset data to only include tech-mapped assets
par_unit <- par_unit[unit_id %in% unique(tech_unit$unit_id),]
setnames(par_unit, 'ultimate_id', 'firm_id')

# check those matched w\ ultimate parents
notmatched_mw <- par_unit[firm_id%in% mapped_owner_df[, lev_final], .(na_mw = sum(mw)), by ='year' ]
tot_mw <- par_unit[, .(tot_mw=sum(mw)), by = 'year']
# merge(notmatched_mw, tot_mw)[, na_mw/tot_mw] %>% range
#sapply( mapped_owners_list, nrow)[1] / sum( sapply( mapped_owners_list, nrow) )





#### 4 clean firm-level info data
d3[, `:=`(establ = as.integer(establ), incorp = as.integer(incorp), state_own = as.numeric(state_own)) ]
d3[incorp ==0, incorp := NA]
d3[, birth:= pmin(establ, incorp, na.rm = T)]
d3[is.na(short), short := name]
apply(d3[,c('birth', 'incorp', 'establ')], 2, function(x) sum(!is.na(x)))

firmdat_detail <- d3[entity_id %in% par_unit$firm_id, c('entity_id', 'name', 'short', 'type', 'status', 'birth',  'isin', 'lei', 'sic', 'ind', 'ind_prim', paste0('ind',1:4), 'geo', 'country_name', 'country', 'subregion', 'region', 'state_own')]
setnames(firmdat_detail, 'entity_id', 'firm_id')
apply(firmdat_detail, 2, function(x) sum(is.na(x)))

firmdat <- firmdat_detail[, c('firm_id', 'name', 'short', 'type', 'status', 'birth', 'country', 'state_own')]

# make custom industry classification
firmdat$industry <- firmdat_detail[, ind_prim]
firmdat[industry %in% c('Renewable Electricity', 'Independent Power Producers and Energy Traders'), industry := 'IPPs']
firmdat[industry %in% c('Gas Utilities', 'Multi-Utilities', 'Water Utilities'), industry := 'Other Utilities']
firmdat[industry %in% c('Oil and Gas Exploration and Production', 
                    'Oil and Gas Drilling', 
                    'Oil and Gas Storage and Transportation', 
                    'Integrated Oil and Gas', 'Oil and Gas Refining and Marketing',
                    'Coal and Consumable Fuels'), industry := 'Coal, Oil and Gas']
firmdat[industry %in% c('Government Institution', 'Diversified Support Services') | type == 'Government Institution', industry := 'Government']
firmdat[industry %in% c('Government Institution', 'Diversified Support Services') | type == 'Government Institution', industry := 'Government']
core_industries <- c("Other Utilities", "Unclassified","Asset Management", "Coal, Oil and Gas", "Government", "IPPs", "Electric Utilities")
firmdat[!(industry %in% core_industries), industry := 'Other']

firmdat$industry %>% table %>% sort

unit_detail <- merge(par_unit, tech_unit)
unit_detail[, oper_mw := NULL]

#### 5. SAVE all data ####
write.csv(par_unit, 'R-data-output/unit_parent_year.csv', row.names = F)
write.csv(own_unit, 'R-data-output/unit_owner_year.csv', row.names = F)
write.csv(tech_unit, 'R-data-output/unit_tech_info.csv', row.names = F)
write.csv(tech_unit_clean, 'R-data-output/unit_tech_info_detail.csv', row.names = F)
write.csv(firmdat, 'R-data-output/parent_info.csv', row.names = F)
write.csv(firmdat_detail, 'R-data-output/parent_info_detail.csv', row.names = F)
write.csv(unit_detail, 'R-data-output/unit_timeseries_detail.csv', row.names = F)


# contains all firms (direct owners + parents)
write.csv(d3, 'R-data-output/all_firms_info_detail.csv.csv', row.names = F)



# # basic checks
# # dat <- merge(par_unit, firmdat)
# # check <- (dat[year==2022, sum(mw), by='industry']) 
# # check[, share := V1/sum(V1)*100]
# # tail( check[order(V1), ], 15)
# firmdat$status %>% table(., useNA = 'ifany')
# firmdat$type %>% table(., useNA = 'ifany')
# firmdat_detail$ind %>% table(., useNA = 'ifany')
# firmdat_detail$ind_prim %>% table(., useNA = 'ifany') %>% sort
# firmdat_detail$ind2 %>% table(., useNA = 'ifany') %>% sort
# 
# plot( firmdat_detail$establ ~ firmdat_detail$incorp )
# 
# 
# 
# #### 4. Create datasets for parent level ####
# 
# 
# 
# # check data
# #tech_unit_clean
# all(tech_unit$unit_id %in% par_unit$unit_id )
# # basic stats
# unique(par_unit$ultimate_id) %>% length
# unique(own_unit$owner_id) %>% length
# 
# # 
# unique(par_unit$unit_id) %>% length
# unique(own_unit$unit_id) %>% length
# 
# sum(!(unique(par_unit$unit_id) %in% unique(own_unit$unit_id)) )
# sum(!(unique(own_unit$unit_id) %in% unique(par_unit$unit_id)) )
# 
# par_unit[unit_id %in% unique(par_unit$unit_id)[!(unique(par_unit$unit_id) %in% unique(own_unit$unit_id))], ]
# own_unit[unit_id %in% unique(par_unit$unit_id)[!(unique(par_unit$unit_id) %in% unique(own_unit$unit_id))], ]