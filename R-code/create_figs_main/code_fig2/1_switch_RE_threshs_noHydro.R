# integrate firm focus from physical data
phys <- fread('data_processed/R-data-output/firm_tech_year.csv')
phys <- phys[total_mw > 0 |invest_mw > 0,]

# compute tech shares
tech_share <- sweep( phys[, .SD, .SDcols = tech_rank_short], 1, rowSums(phys[, .SD, .SDcols = tech_rank_short]), '/' ) %>% as.data.table()
setnames(tech_share, paste0(tech_rank_short, '_share'))

phys <- cbind(phys,tech_share)

RE <- tech_rank_short[c(1:5, 7)] # hydro excluded, no waste, no nuclear
FF <- tech_rankings[10:12]

phys$FF_share <- rowSums( tech_share[, .SD, .SDcols = paste0(FF,'_share')] )
phys$RE_share <- rowSums( tech_share[, .SD, .SDcols = paste0(RE,'_share')] )


FF_THRESH <- 0.5

years <- (2001:2021)
rolling_windows <- embed(years, 5)[, 5:1]
rollff_list <- list()
for (i in 1:nrow(rolling_windows)) {
  rollff_list[[i]] <- phys[year %in% rolling_windows[i,], sum(FF_share > FF_THRESH) >=5, by = 'firm_id']
  rollff_list[[i]]$year <- rolling_windows[i,5]
}


# Did firm satisfy FF criteria in a given rolling window?
FF_year_indicator <- Reduce(rbind, rollff_list)       
setnames(FF_year_indicator, 'V1', 'FF') 
# Was firm a FF firm in any rolling window?
FF_indicator <- na.omit(FF_year_indicator)[, any(FF), by = 'firm_id']
setnames(FF_indicator, 'V1', 'FFany') 


#RE_THRESH <- c(seq(0.5,.999, by = 0.05), 0.999)
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



# =============================================================================
# Transition statistics for main text
# =============================================================================

# Transition statistics at 50% renewable threshold
trans_stats <- resagg[re_thresh == 0.5]
nr_transitioning <- trans_stats$count
gw_in_2024 <- trans_stats$mw_23 / 1000

# Print results
cat("\n")
cat("=== Corporate Energy Transition Statistics ===\n")
cat("\n")
cat(sprintf("Transitioning firms (to ≥50%% renewables):
             %d firms\n", nr_transitioning))
cat("\n")
cat(sprintf("Capacity owned by transitioning firms:
             %.0f GW in 2024",gw_in_2024))
cat("\n")