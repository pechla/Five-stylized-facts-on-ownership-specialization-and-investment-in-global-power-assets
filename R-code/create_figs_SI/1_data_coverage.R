rm(list=ls())
source('R-code/source_code/libraries.R')

dunit <- fread('data_processed/R-data-output/unit_timeseries_detail.csv')

datadat <- dunit[, .(`Capacity in GW` =sum(mw)/1000,
                     `Number of assets` = length(unique(unit_id)),
                     `Number of plants` = length(unique(plant_id)),
                     `Number of owners` = length(unique(firm_id)) ), by = 'year']


datal <- melt(datadat, id.vars = 'year')

plain <- function(x,...) {
  format(x, ..., scientific = FALSE, trim = TRUE)
  
}

p1 <- ggplot(datal[variable != 'Capacity in GW',], aes(x=year, color = variable, y = value, shape = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 5) +
  scale_shape_manual(values = c(16,17,18,4)) +
  scale_color_manual(values = c('dodgerblue', 'firebrick', 'darkgreen', 'black')) +
  theme(
    axis.title = element_text(size=22),
    axis.text = element_text(size = 22),
    #    panel.grid = element_blank(),
    plot.title = element_text(size = 22),
    legend.position = c(0,1),
    legend.justification = c(0,1),
    #  legend.position = 'none',
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key.width = unit(1.2, 'cm'),
    legend.text = element_text(size = 22)
  ) + 
  scale_y_continuous(labels = plain) +
  scale_x_continuous(breaks = seq(2002,2025, 4)) +
  # scale_y_log10( 
  #   labels = plain) +
  #  annotation_logticks(sides = 'l') +
  xlab("") + ylab("Count per year")



#### GROWTH RATES ####
gd <- cbind.data.frame(year = datadat$year[-1], apply(log(datadat[,-1]), 2, diff) ) %>% as.data.table()
gd_l <- melt(gd, id.vars = c('year'))

GDL <- gd_l[variable != 'Capacity in GW',]

p2 <- ggplot(GDL, aes(x=year, color = variable, y = value, shape = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 5) +
  scale_color_manual(values = c('dodgerblue', 'firebrick', 'darkgreen', 'black')) +
  scale_y_continuous(limits = c(0, max(GDL$value)+0.005)) +
  scale_x_continuous(breaks = seq(2002,2025, 4)) +
  theme(
    axis.title = element_text(size=22),
    axis.text = element_text(size = 22),
    #    panel.grid = element_blank(),
    plot.title = element_text(size = 22),
    legend.position = c(0,0),
    legend.justification = c(0,0),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key.width = unit(1.4, 'cm'),
    legend.text = element_text(size = 22)
  ) +
  xlab("") + ylab("Annual log growth rates")




pdf('R-code/create_figs_SI/fig/data_evol_panels.pdf', width = 16, height = 6)
grid.arrange(p1,p2,ncol = 2, widths = c(1,1))
grid.text("A", x = 0.01, y = 0.96, gp = gpar(fontsize = 25, fontface = "bold"))
grid.text("B", x = 0.51, y = 0.96, gp = gpar(fontsize = 25, fontface = "bold"))
dev.off()


## tech specific increases
tech_stats <- dunit[, .(`Capacity in GW` =sum(mw)/1000,
                        `Number of assets` = length(unique(unit_id)),
                        `Number of plants` = length(unique(plant_id)),
                        `Number of owners` = length(unique(firm_id)) ), by = c('year', 'tech')]


tech_stats_l <- melt(tech_stats, id.vars = c('year', 'tech') )
tech_stats_l$tech <- factor( tech_rankings[ match( tech_stats_l$tech, tech_rank_short ) ], levels = tech_rankings)

plain <- function(x,...) {
  format(x, ..., scientific = FALSE, trim = TRUE)
  
}

p3 <- ggplot(tech_stats_l[variable != 'Capacity in GW',], aes(x=year, color = variable, y = value, shape = variable)) +
  facet_wrap('tech', ncol = 4) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(16,17,18,4)) +
  scale_color_manual(values = c('dodgerblue', 'firebrick', 'darkgreen', 'black')) +
  theme(
    axis.title = element_text(size=22),
    axis.text = element_text(size = 22),
    panel.grid = element_blank(),
    legend.position = 'bottom',
    # legend.position = c(0,1),
    # legend.justification = c(0,1),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key.width = unit(1.2, 'cm'),
    legend.text = element_text(size = 22),
    strip.background = element_blank(),
    strip.text = element_text(size = 22)
  ) +
  scale_y_log10(
    labels = plain) +
  annotation_logticks(sides = 'l') +
  scale_x_continuous(breaks = seq(2002, 2025, 5)) +
  xlab("") + ylab("Count per year")
p3


pdf('R-code/create_figs_SI/fig/nr_asset_year_spiq.pdf', width = 19, height = 13)
p3
dev.off()




#### COMPARE WITH EMBER ####
techs <- unique( dunit$tech )


# EMBER data
ember_genfull <- read_xlsx('data_processed/Ember/Ember-GER-2022-Data.xlsx', sheet = 'Generation') %>% as.data.table()
ember2 <- ember_genfull[`Country or region` == 'World', ]# %>% unique()
ember <- data.table(year = ember2$Year,
                    variable = ember2$Variable,
                    GW = ember2$`Installed capacity (GW)`
)
# Coal, Gas, Hydro, Nuclear the same in both datasets
ember$variable <- ifelse(ember$variable == 'Bioenergy', 'Bio', ember$variable)
ember_relevant <- ember[variable %in% c('Bio', 'Gas', 'Coal', 'Solar', 'Wind', 'Nuclear', 'Hydro'),]


# also get total
ember_tot2 <- read_xlsx('data_processed/Ember/Ember-GER-2022-Data.xlsx', sheet = 'Pivot – Capacity Absolute', range = 'A5:K26') %>% as.data.table()
ember_tot <- ember_tot2[,c('Zeilenbeschriftungen', 'Gesamtergebnis')]
setnames(ember_tot, c('year', 'GW'))
ember_tot <- data.table( ember_tot[,'year'], variable = 'Total', ember_tot[,'GW'] )

EMBER <- rbind( ember_relevant, ember_tot )


# CIQ DATA
dunit$tech2 <- ifelse(dunit$tech %in% c('Solar', 'PV'), 'Solar', dunit$tech)
dunit$tech2 <- ifelse(dunit$tech2 %in% c('Onshore', 'Offshore'), 'Wind', dunit$tech2)

ciq_cap <- dunit[, .(GW_CIQ = sum(mw)/1000), by = c('year', 'tech2') ]
setnames(ciq_cap, 'tech2', 'variable')
CIQ <- rbind(ciq_cap,
             data.table( datadat[,c('year')], variable = 'Total', GW_CIQ = datadat[, `Capacity in GW`] )
)
CIQ <- CIQ[variable %in% EMBER$variable,]


dcomp <- merge( EMBER, CIQ, by = c('year','variable'))
dcomp[, coverage := GW_CIQ/GW]
dcomp <- dcomp[year < 2021, ]

dcomp$variable <- factor(dcomp$variable, levels = c("Coal", "Gas", "Nuclear", "Hydro", "Bio", "Solar", "Wind", "Total"))


p4 <- ggplot(dcomp, aes(x=year, y = coverage*100)) +
  facet_wrap('variable', ncol = 4) +
  geom_line(linewidth = 1) +
  geom_point(size = 5, shape = 1) +
  scale_color_manual(values = c('dodgerblue', 'firebrick', 'darkgreen')) +
  scale_x_continuous(limits = c(2000,2020), breaks = seq(2002, 2022, 5)) +
  scale_y_continuous(limits= c(0,max(dcomp$coverage)*100), breaks = seq(0,125,25)) +
  theme(
    axis.title = element_text(size=20),
    axis.text = element_text(size = 20),
    panel.grid.minor = element_blank(),
    # legend.position = c(0,1),
    # legend.justification = c(0,1),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key.width = unit(1.2, 'cm'),
    legend.text = element_text(size = 20), 
    strip.background = element_blank(),
    strip.text = element_text(size = 20),
    plot.margin = margin(t = 0,  # Top margin
                         r = 20,  # Right margin
                         b = 0,  # Bottom margin
                         l = 0) # Left margin
  ) + 
  xlab("") + ylab("Capacity coverage in %")

pdf('R-code/create_figs_SI/fig/spiq_coverage.pdf', width = 15, height = 6)
p4
dev.off()







