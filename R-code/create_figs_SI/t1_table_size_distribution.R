rm(list=ls())
source('R-code/source_code/libraries.R')

firm_info <- fread('data_processed/R-data-output/parent_info.csv')

dx <- fread('data_processed/R-data-output/firm_tech_year.csv')
dx <- dx[total_mw > 0 & year == 2024,]

dx23 <- merge(dx, firm_info[,c('firm_id', 'short', 'birth', 'industry', 'state_own', 'country')], by = 'firm_id')
dx23[,year:=NULL,]

# relative shares
dx23[, Share := total_mw / sum(total_mw)]

###
D <- dx23[,.SD, .SDcols = c(tech_rank_short, 'total_mw')]

checktech <- c('total_mw', tech_rank_short)
check_cols <- tech_col[ match(checktech,tech_rank_short) ]
check_cols[1] <- 'firebrick'

rownames <- c('top.1', 'top1', 'top5', '80%owned', 'Gini')
arr_res <- array(NA, dim = c(length(rownames), length(checktech)), dimnames = list(rownames, checktech))
nr.obs <- c()
sharelist <- list()
for (i in 1:length(checktech)) {
  
  abs <- D[,.SD, .SDcols = checktech[i]] %>% unlist()
  abs <- abs[abs>0]
  share <- abs / sum(abs)
  share <- sort(share, decreasing = T)
  cumshare <- cumsum(share)
  
  arr_res[1,i] <- cumshare[ round( quantile(1:length(share), probs = c(0.001)) ) ] * 100
  arr_res[2,i] <- cumshare[ round( quantile(1:length(share), probs = c(0.01)) ) ] * 100
  arr_res[3,i] <- cumshare[ round( quantile(1:length(share), probs = c(0.05)) ) ] * 100
  arr_res[4,i] <- which( cumshare == cumshare[cumshare>0.8][1] ) / length(cumshare) * 100
  arr_res[5,i] <- Gini(share, corr = T) * 100
  
  nr.obs[i] <- length(abs)
  sharelist[[i]] <- share
  
  print( paste0( 'Herfindahl ', checktech[i], ': ', round(Herfindahl(share) * 100,2) ) )
  print( paste0( 'Max ', checktech[i], ': ', round(max(share) * 100,2) ) )
  
  
}
colnames(arr_res)[1] <- 'All'
names(sharelist) <- colnames(arr_res)

xtable( (rbind( arr_res, nr.obs)), digits = 1)


#### TOP 25 FIRMS
tech_shares <- dx23[, lapply(.SD, function(x) x/total_mw), .SDcols = tech_rank_short] 
setnames(tech_shares, new = paste0(tech_rank_short, '_share') )

DX <- cbind(dx23, tech_shares)
DX[, WINDSUM := `Onshore_share` + `Offshore_share`]
DX[, SOLARSUM := `PV_share` + `Solar_share`]

X <- DX[,c('firm_id', 'short', 'country', 'total_mw', 'Share',
             paste0(c('Coal','Gas', 'Oil', 'Nuclear','Hydro'), '_share'),'SOLARSUM', 'WINDSUM')]
X[, c('firm_id') := NULL]
 
setnames(X, new= c('Owner', 'Country','GW', 'Share', 'Coal','Gas', 'Oil', 'Nuclear','Hydro','Solar', 'Wind'))

techcol <- c('Coal', 'Gas', 'Oil', 'Nuclear', 'Hydro', 'Solar', 'Wind')
X[,(techcol) := 100*.SD, .SDcols = techcol] 
X <- X[order(GW, decreasing = T)]
X[, GW := round(GW/1000,1)]
X[, Share := round(GW/sum(GW)*100,1)]


## TOP 25 TABLE
TOP <- X[1:25,]
 
## TOP 25 SUMMARy
sumtop <- data.table(
  Owner = 'Total (Top 25)', Country = NA,
  TOP[, lapply(.SD, sum), .SDcols = c('GW', 'Share')],
  TOP[, lapply(.SD, function(x) sum(GW*x/100, na.rm=T)), .SDcols = techcol]
)
sumtop[, (techcol) := round(100*.SD/GW,1), .SDcols = techcol]

## ALL SUMMARY
sumall <- data.table(
  Owner = 'Total (All)', Country = NA,
  X[, lapply(.SD, sum), .SDcols = c('GW', 'Share')],
  X[, lapply(.SD, function(x) sum(GW*x/100, na.rm=T)), .SDcols = techcol]
)
sumall[, (techcol) := round(100*.SD/GW,1), .SDcols = techcol]


xtable( rbind(TOP, sumtop, sumall), digits = 1 )
