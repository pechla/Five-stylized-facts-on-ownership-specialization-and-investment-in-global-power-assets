D[, invest_indicator := ifelse(invest_mw>0, 1, 0)]

# mean invest_mw share
inv.perc <- D[total_mw>0 & year < 2024, .(share =sum(invest_mw>0)/sum(total_mw>0)), by = 'year']
inv.perc_sum <- c( min( inv.perc[, share] ), mean( inv.perc[, share] ), max( inv.perc[, share] ) )


### SHARE OF FIRMS THAT DO NOT invest_mw iN GIVEN TIME HORIZON
f_exist <- D[year < 2024 & year > 2013, sum(total_mw>0), by = 'firm_id']
f_exist <- f_exist[V1==(2024-2014),]
# firms investing at least once:
ninv <- D[firm_id %in% f_exist$firm_id & year < 2024 & year > 2014,  .(inv = sum(invest_mw >0)), by = 'firm_id']
once.perc_sum <- ninv[, sum(inv>0)/length(firm_id)]


## number of tech invested
D$nr.tech.adding <- apply( D[, .SD, .SDcols = paste0(tech_rank_short, '_invest') ], 1, function(x) sum(x>0) )
nradd2.perc <- D[invest_mw>0, .(share= sum(nr.tech.adding >1)/length(nr.tech.adding)), by = 'year']
nradd2.perc_sum <- c( min( nradd2.perc[, share] ), mean( nradd2.perc[, share] ), max( nradd2.perc[, share] ) )


# share of new investors
newinvest.perc <- D[invest_mw>0 & year < 2024, .(share = sum(total_mw ==0)/length(total_mw)), by = 'year']
newinvest.perc_sum <- c( min( newinvest.perc[, share] ), mean( newinvest.perc[, share] ), max( newinvest.perc[, share] ) )


#### NUMBER OF TECHNOLOGIES OWNED == 1 ####
nrtech1.perc <- D[total_mw>0, .(share = sum(nr.tech ==1)/length(nr.tech)), by = 'year']
nrtech1.perc_sum <- c( min( nrtech1.perc[, share] ), mean( nrtech1.perc[, share] ), max( nrtech1.perc[, share] ) )


#### NUMBER OF TECHNOLOGIES OWNED > 2 ####
nrtech2.perc <- D[total_mw>0, .(share = sum(nr.tech >2)/length(nr.tech)), by = 'year']
nrtech2.perc_sum <- c( min( nrtech2.perc[, share] ), mean( nrtech2.perc[, share] ), max( nrtech2.perc[, share] ) )




#### SHARE OF INVESTING FIRMS INVESTNG IN NEW TECH ####
investingfirms <- D[total_mw>0 & invest_mw >0, ]
tech_notown <- apply( investingfirms[, .SD, .SDcols = tech_rank_short], 1, function(x) x==0) %>% t
tech_invest <- apply( investingfirms[, .SD, .SDcols = paste0(tech_rank_short, '_invest')], 1, function(x) x>0) %>% t

newtech_invest_id <- cbind( investingfirms[,1:2], newtechinvest = apply( tech_notown * tech_invest, 1, function(x) any(x>0) ) )
newtech_invest.perc <- newtech_invest_id[, .(share = sum(newtechinvest)/length(newtechinvest)), by = 'year']
newtech_invest.perc_sum <- c( min( newtech_invest.perc[, share] ), mean( newtech_invest.perc[, share] ), max( newtech_invest.perc[, share] ) )


#### share of frequent investors ####
f_exist <- D[year < 2024 & year > 2013, sum(total_mw>0), by = 'firm_id'] # already defined above
f_exist <- f_exist[V1==(2024-2014),]
invfreq <- D[firm_id %in% f_exist$firm_id & year < 2024 & year > 2014,  .(inv.freq = sum(invest_indicator), inv.sum = sum(invest_mw)), by = 'firm_id']
invfreqD <- merge(invfreq, D[year==2014, c('firm_id', 'total_mw')])

invfreqD[, freq.investor := ifelse(inv.freq>=5, 1,0)]
freqinvestor.perc_sum <- invfreqD[, sum(freq.investor)]/nrow(invfreqD) # count
#freqinvestor.perc_sum <- invfreqD[, sum(freq.investor*inv.sum)]/sum(invfreqD$inv.sum) # weight


#### all together ####
sharetable.count <- rbind(nrtech1.perc_sum, nrtech2.perc_sum, inv.perc_sum, c(NA, once.perc_sum, NA), c(NA, freqinvestor.perc_sum, NA), nradd2.perc_sum, newinvest.perc_sum, newtech_invest.perc_sum)
