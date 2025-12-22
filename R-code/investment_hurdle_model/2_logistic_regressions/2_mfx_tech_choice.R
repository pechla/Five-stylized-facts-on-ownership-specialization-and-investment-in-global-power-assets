rm(list=ls())
source('R-code/source_code/libraries.R')

dreg <- fread('data_processed/R-data-output/regtable.csv')

##### Dep Var: Probability investing in wind conditional on investing at all ####
sharecols <- paste0(tech_rankings, '_share')
techs <- c('PV', 'Onshore', 'Hydro', 'Nuclear', 'Oil', 'Gas', 'Coal')
techs <- tech_rank_short

# create factors
dreg$nr.5yr_invest <- factor(dreg$nr.5yr_invest)
dreg$year_cat <- factor(dreg$year)

dreg$state_own_class <- ifelse(dreg$state_own_class == 'state', 'state-owned', dreg$state_own_class)
dreg$state_own_class <- factor(dreg$state_own_class, levels = c('no state', 'state-owned') )

# log cap
dreg$log_total_mw <- log(dreg$total_mw)

# make simpler nr.tech class (as less investment observations)
dreg$nr.tech_class2 <- ifelse(dreg$nr.tech >= 2, '>1', dreg$nr.tech  )
dreg$nr.tech_class2 <- factor(dreg$nr.tech_class2, levels = c('0','1', '>1'))


reg_type <- list()
nrinvests <- c()
for (i in 1:length(techs)) {
  #  i<-1
  
  dreg_sub <- dreg[invest_TF == T & total_mw>0 & year %in% 2014:2023,] # no investment decisions in 2024
  # make tech-specific
  setnames(dreg_sub, paste0(techs[i], '_bucket'), 'bucket_tech')
  setnames(dreg_sub, paste0(techs[i],'_cap'), 'tech_capacity')
  dreg_sub$log_tech_cap <- log(dreg_sub$tech_capacity+1)
  
  dreg_sub$tech_own_indicator  <- log(pmax(dreg_sub$tech_capacity, 1e-3))
  
  dreg_sub$invest_tech <- ifelse(dreg_sub[, .SD, .SDcols = paste0(techs[i], '_investsize')] > 0, 1, 0)
  
  ## previous 5-year investments
  tech_other <- tech_rank_short[ !(tech_rank_short %in% techs[i]) ]
  check_tech <- apply( dreg_sub[, .SD, .SDcols = paste0('nr.5yr_invest_',techs[i])], 1, function(x) any(x>0) )
  check_other <- apply( dreg_sub[, .SD, .SDcols = paste0('nr.5yr_invest_',tech_other)], 1, function(x) any(x>0) )
  
  dreg_sub$invest_type.5yrs <- ifelse( (!check_tech) & (!check_other), 'no_invest', 
                                       ifelse(  check_tech & check_other, 'both_invest', 
                                                ifelse(  check_tech & (!check_other), 'tech_only', 'other_only')))
  
  dreg_sub$invest_type.5yrs <- factor(dreg_sub$invest_type.5yrs, levels = c("no_invest", "both_invest","tech_only", "other_only"))
  
  
  ##### FULL MODEL INCLUDING YEARS ####
  m1 <- glm(invest_tech ~ 
              year_cat +
              log_total_mw +
              #              log_tech_cap + # removed due to multicolinearity
              #              nr.tech_class2 + # # removed due to multicolinearity
              type + 
              state_own_class +
              invest_type.5yrs +
              factor(bucket_tech), family = 'binomial',
            data = dreg_sub)
  
  reg_type[[i]] <- logitmfx(m1, data = dreg_sub, atmean = F, robust = TRUE)
  
  # size effects
  nrinvests[i] <- sum(dreg_sub$invest_tech > 0)
  dreg_sub[, tech_share:=tech_capacity / total_mw]

  print(i)
}

names(nrinvests) <- names(reg_type) <- techs

## TECH CHOICE REPORTING
COVS2 <- c("Log total capacity",
           #                    "Log tech capacity",
           #                    "Nr of tech owned: >1",
           "Public firm",
           "Other entity",
           "Invested in focal and other tech (5 yrs)",
           "Invested in focal tech only (5 yrs)",
           "Invested in other tech only (5 yrs)",
           "Focal tech share: (0,0.25]",
           "Focal tech share: (0.25,0.5]",
           "Focal tech share: (0.5,0.75]",
           "Focal tech share: (0.75,1)",
           "Focal tech share: 1",
           'Constant'
)

pseudorsq <- 1 - sapply( reg_type, function(x) round(x$fit$deviance,1)) / sapply( reg_type, function(x) round(x$fit$null.deviance,1))

stargazer( lapply(reg_type, function(x) x$fit),
           star.cutoffs = c(5*10^(-2), 10^(-2), 10^(-16)),
           coef = lapply(reg_type, function(x) c(x$mfxest[,1]) ),
           se = lapply(reg_type, function(x) c(x$mfxest[,2]) ),
           p = lapply(reg_type, function(x) c(x$mfxest[,4]) ),
           type = 'latex',
           single.row = F,
           omit = c("year", 'Constant'),
           covariate.labels=COVS2,
           omit.stat =c("LL", 'f', 'rsq', 'ser', 'aic'),
           no.space=T,
           add.lines = list(
             c("Pseudo R2", round(pseudorsq,3)),
             c("Nr of investments in $k$", nrinvests)
           ),
           column.labels = tech_rank_short,
           label = c("t:regall_type")
)
