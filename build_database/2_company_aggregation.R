rm(list = ls())

source('R-code/source_code/libraries.R')

units <- fread('data_processed/R-data-output/unit_timeseries_detail.csv')

# total firm size
firm_size_l <- units[, .(total_mw = sum(mw)) , by = c('firm_id', 'year')]


### MAKE INVESTMENT DATA AT UNIT LEVEL
# wide format unit-company level
unit_w <- dcast(data = units, firm_id + unit_id ~ year, value.var = 'mw', fill = 0)
unit_diff_w <- cbind(unit_w[, c('firm_id', 'unit_id')], t( apply( unit_w[, -c('firm_id', 'unit_id')], 1, diff) ) )

unit_diff_mat <- as.matrix( unit_diff_w[,-c('firm_id', 'unit_id')])
unit_invest_w <- cbind(unit_diff_w[,c('firm_id', 'unit_id')], ifelse( unit_diff_mat >0, unit_diff_mat, 0  ) )
firm_invest_w <- unit_invest_w[, lapply(.SD, function(x) sum(x)), by = 'firm_id', .SDcols = yrs_ch[-1]]

firm_invest_l <- melt(firm_invest_w, id.vars = c('firm_id'), variable.name = 'year', value.name = 'invest_mw', variable.factor = F)
firm_invest_l[, year := as.integer(year)-1] # note that investment happens already in year before capacity change observed

### MAKE INVESTMENT DATA AT TECH LEVEL
# changes in tech-company level
tech_w <- dcast(data = units, firm_id + tech ~ year, value.var = 'mw', fun.aggregate = sum, fill = 0)
tech_diff_w <- cbind(tech_w[, c('firm_id', 'tech')], t( apply( tech_w[, -c('firm_id', 'tech')], 1, diff) ) )

tech_diff_pre <- melt(tech_diff_w, id.vars = c('firm_id', 'tech'), variable.name = 'year', value.name = 'invest_mw', variable.factor = F)
tech_diff_pre[, year := as.integer(year)-1] # note that investment happens already in year before capacity change observed

tech_invest <- dcast(tech_diff_pre, firm_id + year ~ tech, value.var = 'invest_mw', fill = 0)
setnames( tech_invest, old = tech_rank_short, new = paste0(tech_rank_short, '_invest') )


# nr of technologies
nr_tech_l <- units[, .(nr.tech = length(unique(tech))), by = c('firm_id', 'year')]
#table(nr_tech_l$nr.tech, useNA = 'ifany')

# tech capacity ownership
tech_own <- dcast( units[, .(mw = sum(mw)), by = c('firm_id', 'year', 'tech') ], firm_id + year ~ tech, value.var = 'mw', fill = 0 )


# merge all data
d <- merge(firm_size_l, firm_invest_l, by = c('firm_id', 'year'), all = T)
d <- merge(d, nr_tech_l, by = c('firm_id', 'year'), all = T)
d <- merge(d, tech_own, by = c('firm_id', 'year'), all = T)
d <- merge(d, tech_invest, by = c('firm_id', 'year'), all = T)
setnafill(d, fill = 0)

write.csv(d, 'data_processed/R-data-output/firm_tech_year.csv', row.names = F)

# firm_year_invest <- unit_diff_w[, lapply(.SD, function(x) sum(x>0)), by = 'firm_id', .SDcols = yrs_ch[-1]]
# firm_year_retire <- unit_diff_w[, lapply(.SD, function(x) sum(x<0)), by = 'firm_id', .SDcols = yrs_ch[-1]]



