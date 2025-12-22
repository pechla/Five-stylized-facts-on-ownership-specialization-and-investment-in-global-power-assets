rm(list=ls())
source('R-code/source_code/libraries.R')

dreg <- fread('data_processed/R-data-output/regtable.csv')

# create factors
dreg$type <- factor(dreg$type, levels = c('Private firm', 'Public firm', 'Other'))
dreg$nr.tech_class <- factor(dreg$nr.tech_class, levels = c('0','1','[2,4)','[4,6)', '>=6'))
dreg$focus_class <- factor(dreg$focus_class, levels = c('Mixed', 'Solar PV', 'Onshore wind', 'Hydro', 'Gas', 'Coal', 'Mixed-FF', 'Mixed-RE', 'Other'))

dreg$state_own_class <- ifelse(dreg$state_own > 50 | is.na(dreg$state_own), 'state', 'no state')
dreg$state_own_class <- factor(dreg$state_own_class, levels = c('no state', 'state'))

dreg$year <- factor(dreg$year)
dreg$nr.5yr_invest <- factor(dreg$nr.5yr_invest)
dreg$log_total_mw <- log10(dreg$total_mw)

DREGL <- list(
  Full = dreg,
  decade = dreg[year %in% 2014:2023, ],      # note that no investments in 2024
  quinquennium = dreg[year %in% 2019:2023, ]
)


res <- res_mp <- list()
nrinvests <- c()
for (i in 1:length(DREGL)) {
  
  if( DREGL[[i]]$year %>% unique() %>% length() > 1){
    res[[i]] <- glm(invest_TF ~ 
                      year +
                      log_total_mw +
                      nr.tech_class +
                      type + 
                      nr.5yr_invest +
                      focus_class, 
                    family = 'binomial',
                    data = DREGL[[i]]) 
  } else{
    res[[i]] <- glm(invest_TF ~ 
                      log_total_mw +
                      nr.tech_class +
                      type + 
                      nr.5yr_invest +
                      focus_class,
                    family = 'binomial',
                    data = DREGL[[i]]) 
  }
  
  res_mp[[i]] <- logitmfx(res[[i]], data = DREGL[[i]], atmean = F, robust = TRUE)
  
  nrinvests[i] <- sum(DREGL[[i]]$invest_TF==T)
  print(i)
}


## print results
COVS <- 
  c("Log total capacity",
    "Nr of tech owned: [2,4)",
    "Nr of tech owned: [4,6)",
    "Nr of tech owned: $/ge$6",
    "Public firm",
    "Other entity",
    "Invested 1x in past 5 yrs",
    "Invested 2x in past 5 yrs",
    "Invested 3x in past 5 yrs",
    "Invested 4x in past 5 yrs",
    "Invested 5x in past 5 yrs",
    "Focus: Solar PV",
    "Focus: Wind Onshore",
    "Focus: Hydro",
    "Focus: Gas",
    "Focus: Coal",
    "Focus: Mixed-FF",
    "Focus: Mixed-RE",
    "Focus: Any other tech"
  )


pseudorsq <- 1 - sapply( res_mp, function(x) round(x$fit$deviance,1)) / sapply( res_mp, function(x) round(x$fit$null.deviance,1))

stargazer( lapply(res_mp, function(x) x$fit),
           star.cutoffs = c(5*10^(-2), 10^(-2), 10^(-16)),
           coef = lapply(res_mp, function(x) c(x$mfxest[,1]) ),
           se = lapply(res_mp, function(x) c(x$mfxest[,2]) ),
           p = lapply(res_mp, function(x) c(x$mfxest[,4]) ),
           type = 'latex',
           single.row = F,
           no.space = T,
           omit = c("year", 'Constant'),
           covariate.labels=COVS,
           omit.stat =c("LL", 'f', 'rsq', 'ser', 'aic'),
           add.lines = list(
             c("Pseudo R2", round(pseudorsq,3)),
             c("Nr of investments", nrinvests)
           ),
           label = c("t:reg_invest"
           )
)
