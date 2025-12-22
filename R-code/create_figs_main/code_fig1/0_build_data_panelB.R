rm(list=ls())

# required:
source('R-code/source_code/libraries.R')

# unit level data (already mapped to ultimate owner)
unit <- fread('data_processed/R-data-output/unit_timeseries_detail.csv')

# make aggregate tech categories
unit$tech_agg <- ifelse(unit$tech %in% c('Onshore', 'Offshore'), 'Wind', unit$tech)
unit$tech_agg <- ifelse(unit$tech %in% c('PV', 'Thermal'), 'Solar', unit$tech_agg)
unit$tech <- unit$tech_agg
unit[,tech_agg := NULL]

# make wide on unit level but ignore firms to really get actually capacity expansions (not sales)
capw <- dcast(unit, formula = unit_id ~ year, value.var = 'mw', fun.aggregate = sum)
setnafill(capw, fill = 0)
diffw <- cbind( capw[,.SD, .SDcols = c('unit_id')], 
                capw[,.SD,.SDcols = as.character(2002:2024)] - capw[,.SD,.SDcols = as.character(2001:2023)] )


# long format of unit installations
uinstal <- melt(diffw, id.vars = c('unit_id'), variable.name = 'year', value.name = 'install')
uinstal$year <- as.integer(as.character(uinstal$year)) 

# separate installations and retirements
uinstal[,retire := ifelse(install<0, (-1)*install, 0)]
uinstal[,install := ifelse(install<0,0,install)]
summary(uinstal)

# merge back into unit dataset
unit <- merge(unit, uinstal, by = c('unit_id','year'), all = T )


# firm-tech-specific capacity expansions (no acquisitions)
DAT <- unit[, lapply(.SD, function(x)sum(x,na.rm=T)), by = c('firm_id', 'year', 'tech'), .SDcols = c('install', 'retire', 'mw')]
setnames(DAT, 'mw', 'tech_mw')



# integrate firm focus from physical data
phys <- fread('data_processed/R-data-output/firm_tech_year.csv')
phys <- phys[total_mw > 0 |invest_mw > 0,]

# compute tech shares
tech_share <- sweep( phys[, .SD, .SDcols = tech_rank_short], 1, rowSums(phys[, .SD, .SDcols = tech_rank_short]), '/' ) %>% as.data.table()
setnames(tech_share, paste0(tech_rank_short, '_share'))

phys <- cbind(phys,tech_share)

# compute firm focus
RE <- tech_rank_short[c(1:5, 7:8)] # hydro excluded
FF <- tech_rankings[10:12]
HY <- 'Hydro'
NU <- 'Nuclear'

phys$FF_share <- rowSums( tech_share[, .SD, .SDcols = paste0(FF,'_share')] )
phys$RE_share <- rowSums( tech_share[, .SD, .SDcols = paste0(RE,'_share')] )
phys$NU_share <- rowSums( tech_share[, .SD, .SDcols = paste0(NU,'_share')] )
phys$HY_share <- rowSums( tech_share[, .SD, .SDcols = paste0(HY,'_share')] )

phys[, focus := character()]
phys[FF_share >0.5, focus := 'Fossil']
phys[RE_share >0.5, focus := 'Renewable']
phys[HY_share >0.5, focus := 'Hydro']
phys[NU_share >0.5, focus := 'Nuclear']
phys[NU_share <=0.5 & HY_share <=0.5 & RE_share <= 0.5 & FF_share <= 0.5, focus := 'Mixed']

# any firm that shows up after 2013 is treated as a new firm
firm_mw_w <- dcast(phys, firm_id ~year, value.var = 'total_mw', fill = 0)
not_existing_before13 <- firm_mw_w$firm_id[ apply( firm_mw_w[, .SD,.SDcols = as.character(2001:2014)], 1, function(x) all(x==0) ) ]
phys[firm_id %in% not_existing_before13, focus := 'New']


# make focus of year 2013
foc13 <- phys[year==2013, .SD, .SDcols = c('firm_id', 'focus')]
setnames(foc13, 'focus', 'focus_hist')
phys <- merge(phys, foc13[,c('firm_id', 'focus_hist')], all.x = T)

# merge back into installation data
DAT <- merge(DAT, phys[,c('firm_id', 'year', 'focus', 'focus_hist')], by = c('firm_id','year') )

# prepare data for nice plotting
all_install <- DAT[install>0 & year > 2013,]
all_install$focus <- factor(all_install$focus, levels = c('Renewable', 'Hydro', 'New', 'Mixed', 'Nuclear', 'Fossil'))
all_install$tech <- factor(all_install$tech, levels = c('Solar', 'Wind', 'Bio', 'Hydro', 'Geo', 'Waste', 'Nuclear', 'Oil', 'Gas', 'Coal'))


# TECH FOCUS: aggregate over last 10 years
tech_install_focus <- all_install[year > 2013, .(install = sum(install)), by =c('tech','focus')]
tech_install_focus[,install_rel := install / sum(install), by = 'tech']
setnames(tech_install_focus, 'focus', 'Firm focus:')


saveRDS(tech_install_focus, 'data_processed/data_temp/firmtype_capchange_data.RDS')