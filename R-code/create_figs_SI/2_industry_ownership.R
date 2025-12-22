rm(list=ls())
source('R-code/source_code/libraries.R')


firm_info <- fread('data_processed/R-data-output/parent_info.csv')
firm_tech <- fread('data_processed/R-data-output/firm_tech_year.csv')


# merge the two datasets
dat <- merge(firm_tech, firm_info[,c('firm_id','industry')], by.x = 'firm_id', by.y='firm_id')
dat <- dat[total_mw > 0,]
dat <- dat[ year %in% c(2001,2024) ]
setnames(dat, 'total_mw', 'Total')

dat$industry <- ifelse(dat$industry %in% c('Other','Unclassified'), 'Other', dat$industry)

# MAKE DISGGREGATE OWNERSHIP DISTRIBUTION
ind_mw <- dat[, lapply(.SD, sum), by =c('industry', 'year'), .SDcols = c('Total', tech_rank_short)]
setnames(ind_mw, tech_rank_short, tech_rankings)

d <- melt(ind_mw, id.vars = c('industry','year'), variable.factor = F)
d[, share := fnorm(value), by = c('variable','year')]

d$variable <- factor(d$variable, levels = rev(c('Total', tech_rankings) ) )
d$industry <- factor(d$industry, levels = rev(c('Electric Utilities', 'Government', 'Coal, Oil and Gas', 'Asset Management', 'Other Utilities', 'Other', 'IPPs') ) )
d$year <- paste('Year:', d$year)

p1 <- ggplot(d, aes(y=variable, x = share*100, fill = industry)) +  
  geom_col() +
  facet_wrap('year') +
  scale_fill_manual(values = c('firebrick', 'lightgreen', 'dodgerblue', 'orange2', 
                                   'black', 'burlywood', 'darkblue') ) +
  xlab('Ownership (%)') + ylab('Technology') +
  theme(
    axis.title = element_text(size=20),
    axis.text = element_text(size = 20),
    panel.grid = element_blank(),
    legend.text = element_text(size=20),
    legend.title = element_text(size=20),
    legend.position = 'top',
    strip.text = element_text(size = 20),
    strip.background = element_blank()
  ) + 
  guides(fill = guide_legend(title = 'Industry class:', reverse=T, ncol = 3))

p1

dat_temp <- dat[, lapply(.SD, sum), by =c('industry', 'year'), .SDcols = c('Total', tech_rank_short)]

dat2 <- dat_temp[,c('industry','year', 'Total', 'Nuclear', 'Hydro')]
dat2$`Non-hydro RE` <- rowSums( dat_temp[, .SD, .SDcols = tech_rank_short[c(1:5, 7:8)]] )
dat2$FF <- rowSums( dat_temp[, .SD, .SDcols = tech_rank_short[c(10:12)]] )

d2 <- melt(dat2, id.vars = c('industry','year'), variable.factor = F)
d2[, share := fnorm(value), by = c('variable', 'year')]

d2$industry <- factor(d2$industry, levels = rev(c('Electric Utilities', 'Government', 'Coal, Oil and Gas', 'Asset Management', 'Other Utilities', 'Other', 'IPPs') ) )
d2$variable <- factor(d2$variable, levels = rev(c('Total', 'FF', 'Nuclear', 'Hydro', 'Non-hydro RE') ) )
d2$year <- paste('Year:', d2$year)

p2 <- ggplot(d2, aes(y=variable, x = share*100, fill = industry)) + 
  geom_col() +
  facet_wrap('year') +
  scale_fill_manual(values = c('firebrick', 'lightgreen', 'dodgerblue', 'orange2', 
                               'black', 'burlywood', 'darkblue') ) +
  xlab('Ownership (%)') + ylab('Technology') +
  theme(
    axis.title = element_text(size=20),
    axis.text = element_text(size = 20),
    panel.grid = element_blank(),
    legend.text = element_text(size=20),
    legend.title = element_text(size=20),
    legend.position = 'bottom',
    strip.text = element_text(size = 20),
    strip.background = element_blank()
  ) + 
  guides(fill = guide_legend(title = 'Industry class:', reverse=T, ncol = 4))



# only export the less detailed figure for paper
pdf('R-code/create_figs_SI/fig/industry_ownership.pdf', width = 13, height = 5)
p2
dev.off()
