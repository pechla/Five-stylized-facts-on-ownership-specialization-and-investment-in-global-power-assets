# integrate firm focus from physical data
phys <- fread('data_processed/R-data-output/firm_tech_year.csv')
phys <- phys[total_mw > 0 |invest_mw > 0,]

# compute tech shares
tech_share <- sweep( phys[, .SD, .SDcols = tech_rank_short], 1, rowSums(phys[, .SD, .SDcols = tech_rank_short]), '/' ) %>% as.data.table()
setnames(tech_share, paste0(tech_rank_short, '_share'))

phys <- cbind(phys,tech_share)


RE <- tech_rank_short[c(1:7)] # hydro included, no waste, no nuclear
FF <- tech_rankings[10:12]
#HY <- 'Hydro'
#NU <- 'Nuclear'

phys$FF_share <- rowSums( tech_share[, .SD, .SDcols = paste0(FF,'_share')] )
phys$RE_share <- rowSums( tech_share[, .SD, .SDcols = paste0(RE,'_share')] )


FF_THRESH <- 0.5

years <- (2001:2023)
rolling_windows <- embed(years, 5)[, 5:1]
rollff_list <- list()
for (i in 1:nrow(rolling_windows)) {
  rollff_list[[i]] <- phys[year %in% rolling_windows[i,], sum(FF_share > FF_THRESH) >=5, by = 'firm_id']
  setnames( rollff_list[[i]] , 'V1', as.character(rolling_windows[i,5]) )
  
}


FF_roll_indicator <- Reduce(function(x, y) merge(x, y, by = "firm_id"), rollff_list)
FF_indicator <- cbind(FF_roll_indicator[,1], FFany = apply( FF_roll_indicator[, .SD, .SDcols = as.character(years[-c(1:(ncol(rolling_windows)-1))])], 1, function(x) any(x) ) )

FF_year_indicator <- melt( FF_roll_indicator, 
                           id.vars = 'firm_id', variable.name = 'year', value.name = 'FF' )
FF_year_indicator$year <- as.integer(as.character(FF_year_indicator$year))


#RE_THRESH <- c(0.5, 0.6, 0.7, 0.8, 0.9)
reslist1 <- reslist2 <- list()
for (k in 1:length(RE_THRESH)) {
  
  REN_indicator <- phys[year %in% 2022:2024, .(REN = sum(RE_share > RE_THRESH[k]) >=3), by = 'firm_id']
  
  d <- merge(FF_indicator, REN_indicator, by = 'firm_id')
  d$trans <- d$FFany & d$REN
  
  # timing of transition
  dx <- phys[firm_id %in% d[trans==T, firm_id],  .(REN = RE_share > RE_THRESH[k]), by = c('firm_id', 'year')]
  dx <- merge(d, FF_year_indicator[FF==T, .(transyear = max(year)), by = 'firm_id'], all.x = T )
  # alternative definition
  # dx <- merge(d, dx[, .(transyear = min(year)), by = 'firm_id'], by = 'firm_id', all.x = T)
  
  dx$mw_transyear <- NA_real_
  dx$mw_23 <- NA_real_
  
  trans_firms <- dx[trans==T, firm_id]
  trans_year <- dx[trans==T, transyear]
  
  dx <- as.data.frame(dx)
  idx <- which( dx$trans==T)
  
  if (length(trans_firms) > 0) {
    for (i in 1:length(trans_firms)) {
      
      dx$mw_transyear[idx[i]] <- phys[firm_id == trans_firms[i] & year == trans_year[i], total_mw]
      dx$mw_23[idx[i]] <- phys[firm_id == trans_firms[i] & year == 2024, total_mw]
      
    }
  }
  
  dx <- as.data.table(dx)
  
  TRANS <- dx[trans==T, c('firm_id', 'transyear', 'mw_transyear', 'mw_23')]
  dagg <- TRANS[, .(mw_transyear = sum(mw_transyear), mw_23 = sum(mw_23), count = nrow(TRANS)), ]
  
  reslist1[[k]] <- data.table( ff_thres = FF_THRESH, re_thresh = RE_THRESH[k], TRANS)
  reslist2[[k]] <- data.table( ff_thres = FF_THRESH, re_thresh = RE_THRESH[k], dagg)
  
}

resagg <- do.call(rbind, reslist2)
resdetail <- do.call(rbind, reslist1)
resagg$plotshare <- paste0('>', resagg$re_thresh)
