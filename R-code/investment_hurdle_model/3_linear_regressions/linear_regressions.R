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


regdat <- robust_se <- reg_size <- list()
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
  
  # size effects
  nrinvests[i] <- sum(dreg_sub$invest_tech > 0)
  dreg_sub[, tech_share:=tech_capacity / total_mw]
  dreg_sub2 <- dreg_sub[invest_tech >0, ]
  
  # linear regression  
  m2 <- lm( log(investsize) ~ 
              year_cat + 
              log_total_mw +
              type +
              factor(bucket_tech),
            data = dreg_sub2) 
  
  
  reg_size[[i]] <- m2
  robust_se[[i]] <- sqrt(diag(vcovHC(m2, type = "HC0")))
  
  regdat[[i]] <- dreg_sub2
  
  print(i)
}
vif(m2)


names(regdat) <- names(robust_se) <- names(nrinvests) <- names(reg_size) <- techs

COVS1<- c("Log total capacity",
          "Public firm",
          "Other entity",
          "Focal tech share: (0,0.25]",
          "Focal tech share: (0.25,0.5]",
          "Focal tech share: (0.5,0.75]",
          "Focal tech share: (0.75,1)",
          "Focal tech share: 1"#,
          #'Constant'
)

stargazer( reg_size,
           star.cutoffs = c(5*10^(-2), 10^(-2), 10^(-16)),
           se = robust_se,
           type = 'latex',
           single.row = F,
           omit = c("year", "Constant"),
           covariate.labels=COVS1,
           omit.stat =c("LL", 'rsq', 'f', 'ser'),
           no.space=T,
           column.labels = tech_rank_short,
           label = c("t:regall_size"
           )
)


dt <- rbind(
  data.table( regdat[["Onshore"]][,c('investsize', 'year','log_total_mw', 'log_tech_cap')], tech = 'Wind'),
  data.table( regdat[["PV"]][,c('investsize', 'year','log_total_mw', 'log_tech_cap')], tech = 'PV'),
  data.table( regdat[["Gas"]][,c('investsize', 'year','log_total_mw', 'log_tech_cap')], tech = 'Gas'),
  data.table( regdat[["Coal"]][,c('investsize', 'year','log_total_mw', 'log_tech_cap')], tech = 'Coal')
)
dt[, Year:=year+1] # since investment years are shifted by 1

dt[, tech := factor(tech, levels = c('PV', 'Wind', 'Gas', 'Coal'))]

years <- sort(unique(dt$Year))  # Extract unique years
colors <- viridis(length(years))  # Same number of colors as years

dt[, year := year + 1] # for visualizatin only: note that investment decision in 2023 -> capacity change 2023->2024

pdf('R-code/investment_hurdle_model/fig/regsize_vis.pdf', width = 7, height = 2.5)
ggplot(dt, aes(x = exp(log_total_mw), y = investsize, color = factor(Year))) + 
  geom_abline(slope = 1, linetype = 2) + 
  geom_point(size = .5) +
  scale_color_manual(values = setNames(colors, rev(years)), name = "Year") +
  guides(color = guide_legend(
    title.position = "top",
    title.hjust = 0.5, 
    reverse = T,
    override.aes = list(size = 1.5)
  )) +
  facet_wrap(~tech, ncol = 4) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))) +
  #  annotation_logticks(sides = 'lb') +
  xlab('Firm size (MW)') +
  ylab('Investment amount (MW)') +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid = element_blank(),
    legend.position = 'right',
    legend.text = element_text(size = 10),
    legend.key.height = unit(.5,'cm'),
    legend.title = element_text(size = 10, hjust = 0.5),
    legend.box.just = "center",
    legend.margin = margin(0, 0, 0, 0)
  )

dev.off()



# SI TABLE
print(xtable(
  cbind(data.frame(`Focal tech share` = c('0', '(0,0.25]', '(0.25,0.5]', '(0.5,0.75]', '(0.75,1)', '1')),
        sapply(regdat, function(x) table(x[, bucket_tech]))
  )
), include.rownames = FALSE)

