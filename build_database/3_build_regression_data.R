rm(list=ls())
source('R-code/source_code/libraries.R')

# database
par <- fread('data_processed/R-data-output/parent_info_detail.csv')
techfirm_full <- fread('data_processed/R-data-output/firm_tech_year.csv')
unit <- fread('data_processed/R-data-output/unit_tech_info.csv')

# compute tech shares
techfirm <- techfirm_full[, .SD, .SDcols = c('firm_id', 'year', 'invest_mw', 'nr.tech', 'total_mw', tech_rank_short)] 
setnames(techfirm, tech_rank_short, paste0(tech_rank_short,'_cap') )

tech_shares <- techfirm_full[, lapply(.SD, function(x) x/total_mw ), .SDcols = tech_rank_short]
setnafill(tech_shares, fill = 0)
techfirm <- cbind(techfirm, tech_shares)

# dependent var
techfirm[, invest_TF := ifelse(invest_mw>0,T,F)]

# regression data
dat <- merge(techfirm, par, by = c('firm_id'), all = T )

# now we transform some vars

# simplify types
par$type %>% table
dat$type <- ifelse(dat$type %in% c('Private Company'), 'Private firm', ifelse(
  dat$type %in% c('Public Company'), 'Public firm', 'Other' ))
dat$type <- factor(dat$type, levels = c('Private firm', 'Public firm', 'Other'))
dat$type %>% table


# group tech shares into buckets: 
sharecols <- paste0(tech_rank_short)

for (i in 1:length(sharecols)) {
  dat[, paste0(tech_rank_short[i], '_bucket') :=bucket_fun(.SD), .SDcols = sharecols[i] ]
}

# nr.tech classes
dat$nr.tech_class <- ifelse(dat$nr.tech >= 2 & dat$nr.tech < 4, '[2,4)', ifelse(dat$nr.tech >= 4 & dat$nr.tech < 6, '[4,6)', ifelse( dat$nr.tech >= 6, '>=6', dat$nr.tech)))
dat$nr.tech_class <- factor(dat$nr.tech_class, levels = c('0','1','[2,4)','[4,6)', '>=6'))


# create firm focus
dat[, focus := 'Mixed']
dat[Bio >0.5, focus := 'Bio']
dat[Hydro >0.5, focus := 'Hydro']
dat[PV>0.5, focus := 'Solar PV']
dat[Solar>0.5, focus := 'Solar thermal']
dat[Offshore>0.5, focus := 'Offshore wind']
dat[Onshore>0.5, focus := 'Onshore wind']
dat[Geo>0.5, focus := 'Geo']
dat[Waste>0.5, focus := 'Waste']
dat[Nuclear >0.5, focus := 'Nuclear']
dat[Coal >0.5, focus := 'Coal']
dat[Gas>0.5, focus := 'Gas']
dat[Oil>0.5, focus := 'Oil']
dat[Coal+Gas+Oil >0.5 & Coal<0.5 & Gas < 0.5 & Oil < 0.5, focus := 'Mixed-FF']
dat[Bio+Hydro+PV+Solar+Offshore+Onshore+Geo+Waste >0.5 & Bio<0.5 & Hydro< 0.5 & PV< 0.5 & Solar< 0.5 & Offshore< 0.5 & Onshore< 0.5 & Geo < 0.5 & Waste< 0.5, focus := 'Mixed-RE']
table(dat$focus)

# focus as factors
dat$focus_class <- ifelse(dat$focus %in% c('Onshore wind', 'Solar PV', 'Coal', 'Gas', 'Hydro', 'Mixed', 'Mixed-FF', 'Mixed-RE', 'Nuclear'), dat$focus, 'Other')

# age
dat$age_raw <- dat$year- dat$birth 
dat$age <- ifelse(dat$age_raw <0 | dat$age_raw >200, NA, dat$age_raw)
dat$age_class <- ifelse( dat$age_raw < 5, '<5', ifelse(dat$age_raw < 10 & dat$age_raw >=5, '[5,10)', ifelse(dat$age_raw < 20 & dat$age_raw >=10, '[10,20)', '>=20')) )

# state-ownership class
dat$state_own_class <- ifelse(dat$state_own > 50 | is.na(dat$state_own), 'state', 'no state')
dat$state_own_class <- factor(dat$state_own_class, levels = c('no state', 'state'))

#### add (tech-specific) investment information of previous 5 years
source('build_database/3X_build_regression_data_helper.R')
# creates data.table "dinvest"

#### MERGE DATA ####
dat <- merge(dat, dinvest, by = c('firm_id', 'year'), all = T)


## note no investments in 2024
## note no 5y investments prior to 2006
dat <- dat[total_mw>0 & year > 2005 & year < 2024,]


## regression table
write.csv(dat, 'data_processed/R-data-output/regtable.csv', row.names = F)

