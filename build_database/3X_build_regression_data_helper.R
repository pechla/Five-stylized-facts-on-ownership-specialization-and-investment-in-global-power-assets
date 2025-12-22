# get unit-parent data and merge with unit-tech info data
up <- fread('data_processed/R-data-output/unit_parent_year.csv')
up <- merge(up, unit[,c('unit_id', 'tech', 'birth', 'death', 'country')], by = 'unit_id' )


# wide-format capacity data on unit_id level (including firm and tech info)
unitcap_w <- dcast(up, formula = firm_id + unit_id + tech ~ year, value.var = 'mw') # note that a unit_id capacity is NA if not built yet
# the same but setting NAs equal to zero:
unitcap_w0 <- cbind( unitcap_w[,c(1,2,3)], apply(unitcap_w[,-c(1,2,3)], 1, function(x) ifelse(is.na(x), 0, x)) %>% t() %>% as.data.table() )

# wide-format capacity change data on unit_id level
unitchange_w <- cbind( unitcap_w[,c(1,2,3)], apply(unitcap_w0[,-c(1,2,3)], 1, function(x) diff(x)) %>% t() %>% as.data.table() )
setnames(unitchange_w, old = as.character(yrs[-1]), new = as.character(yrs[-length(yrs)])) # change yrs
# note that year is set back by 1 -> since we want to use e.g. 2010 capacity year to explain 2010-2011 capacity change 

# making unit_id-level capacity-change long
d <- melt(unitchange_w, id.vars = c('firm_id','unit_id','tech'), variable.name = 'year', value.name = 'capchange')
d$year <- as.integer(as.character(d$year))


## tech specific investments and retirements
invest_tech <- dcast( d[capchange > 0, .(investsize = sum(capchange) ), by = c('firm_id','year', 'tech')], 
                      firm_id + year ~ tech, value.var = 'investsize', fill = 0)
setnames(invest_tech, old = tech_rank_short, new = paste0(tech_rank_short, '_investsize') )


retire_tech <- dcast( d[capchange < 0, .(retiresize = sum(capchange) ), by = c('firm_id','year', 'tech')], 
                      firm_id + year ~ tech, value.var = 'retiresize', fill = 0)
setnames(retire_tech, old = tech_rank_short, new = paste0(tech_rank_short, '_retiresize') )

invest_tot <- d[capchange > 0, .(investsize = sum(capchange) ), by = c('firm_id','year')]
retire_tot <- d[capchange < 0, .(retiresize = sum(capchange) ), by = c('firm_id','year')]

# number of assets added
unitlevel_invest <- dcast( d[capchange > 0,], firm_id + unit_id ~ year, value.var = 'capchange')
unitlevel_investcount <- unitlevel_invest[, lapply(.SD, function(x)sum(!is.na(x))), 
                                          by = 'firm_id', .SDcols = as.character(yrs[-length(yrs)])]
unitlevel_investcount_l <- melt(unitlevel_investcount, id.vars = 'firm_id', variable.name = 'year', value.name = 'assets.adding')
unitlevel_investcount_l$year <- as.integer(as.character(unitlevel_investcount_l$year))

# number of assets removed
unitlevel_retire <- dcast( d[capchange < 0,], firm_id + unit_id ~ year, value.var = 'capchange')
unitlevel_retirecount <- unitlevel_retire[, lapply(.SD, function(x)sum(!is.na(x))), 
                                          by = 'firm_id', .SDcols = as.character(yrs[-length(yrs)])]
unitlevel_retirecount_l <- melt(unitlevel_retirecount, id.vars = 'firm_id', variable.name = 'year', value.name = 'assets.retiring')
unitlevel_retirecount_l$year <- as.integer(as.character(unitlevel_retirecount_l$year))

# number of technologies added
techlevel_invest <- dcast( d[capchange > 0,], firm_id + tech ~ year, fun.aggregate = sum, value.var = 'capchange')
techlevel_investcount <- techlevel_invest[, lapply(.SD, function(x) sum(x>0)), 
                                          by = 'firm_id', .SDcols = as.character(yrs[-length(yrs)])]
techlevel_investcount_l <- melt(techlevel_investcount, id.vars = 'firm_id', variable.name = 'year', value.name = 'nr.tech.adding')
techlevel_investcount_l$year <- as.integer(as.character(techlevel_investcount_l$year))

#### THE BASIC DATA FILE ####
dinvest <- d[, .(invest = any(capchange>0), retire = any(capchange<0)), by = c('firm_id','year')]

dinvest <- merge(dinvest, invest_tot, all = T )
dinvest <- merge(dinvest, retire_tot, all = T )
dinvest <- merge( dinvest, invest_tech, all = T)
dinvest <- merge( dinvest, retire_tech, all = T)

dinvest <- merge(dinvest, unitlevel_investcount_l, all = T)
dinvest <- merge(dinvest, unitlevel_retirecount_l, all = T)
dinvest <- merge(dinvest, techlevel_investcount_l, all = T)

#### NUMBER OF PREVIOUS INVESTMENTS (differentiate technologies) ####
# add number of investments in previous 5 yrs

d$invest <- ifelse(d$capchange>0,1,0)

winlen <- 5
yrs_rel <- yrs[-c(1:winlen)]
ltemp <- NULL
for (t in 1:length(yrs_rel)) {
  #t<-1
  
  dtemp <- d[(year < yrs_rel[t]) & (year >= (yrs_rel[t]-winlen)),]
  dtemp2 <- dtemp#[capchange>0, ] # only investments
  
  # nr of yrs firm has invested
  dtemp3 <- dtemp2[, c('firm_id','year', 'invest')]
  dtemp3 <- dtemp3[!duplicated(dtemp3),]
  nr.invest.df <- cbind(year = yrs_rel[t], dtemp3[, .(nr.5yr_invest = sum(invest)), by = 'firm_id'] )
  
  # nr of tech-yrs firms has invested
  dtemp4 <- dtemp2[, c('firm_id','year', 'tech', 'invest')]
  dtemp4 <- dtemp4[!duplicated(dtemp4),]
  nr.techinvest.l <- dtemp4[, .(nr.invest = sum(invest)), by = c('firm_id', 'tech')]
  nr.techinvest.df <- cbind(year = yrs_rel[t], dcast(nr.techinvest.l, formula = firm_id ~ tech, value.var = 'nr.invest', fill = 0) )
  setnames(nr.techinvest.df, tech_rank_short, paste0('nr.5yr_invest_', tech_rank_short) )
  
  
  ltemp <- rbind(ltemp, merge(nr.invest.df, nr.techinvest.df, all = T)  )
  
  
}

#### MERGE DATA ####
dinvest <- merge(dinvest, ltemp, by = c('firm_id', 'year'), all.x = T)

# remove NAs
setnafill(dinvest, fill = 0, cols = names(dinvest)[-c(1,2,3,4)])

