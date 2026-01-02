rm(list=ls())

source('R-code/source_code/libraries.R')

#### DATA LOAD ####
phys <- fread('data_processed/R-data-output/firm_tech_year.csv')

# compute tech shares
tech_share <- sweep( phys[, .SD, .SDcols = tech_rank_short], 1, rowSums(phys[, .SD, .SDcols = tech_rank_short]), '/' ) %>% as.data.table()
setnames(tech_share, paste0(tech_rank_short, '_share'))

phys <- cbind(phys, tech_share)
phys <- phys[total_mw > 0,]

WI <- tech_rank_short[3:4] # Wind
SO <- tech_rank_short[1:2] # Solar

phys$WI_share <- rowSums( phys[, .SD, .SDcols = paste0(WI,'_share')] )
phys$SO_share <- rowSums( phys[, .SD, .SDcols = paste0(SO,'_share')] )

# compute firm focus
phys$focus <- character()
phys[WI_share > 0.5, focus := 'Wind']
phys[SO_share > 0.5, focus := 'Solar']
phys[SO_share < 0.5 & WI_share < 0.5 & (SO_share + WI_share) > 0.5, focus := 'So+Wi']
phys[Hydro_share > 0.5, focus := 'Hydro']
phys[Geo_share > 0.5, focus := 'Geo']
phys[Bio_share > 0.5, focus := 'Bio']
phys[Waste_share > 0.5, focus := 'Waste']
phys[Nuclear_share > 0.5, focus := 'Nuclear']
phys[Oil_share > 0.5, focus := 'Oil']
phys[Gas_share > 0.5, focus := 'Gas']
phys[Coal_share > 0.5, focus := 'Coal']
phys[Coal_share < 0.5 & Gas_share < 0.5 & Oil_share < 0.5 & (Coal_share + Gas_share + Oil_share) > 0.5, focus := 'FF-Mix']

remix_idx <- rowSums( phys[is.na(focus), .SD, .SDcols = paste0(tech_rank_short[c(1:7)], '_share')] ) > 0.5
phys[is.na(focus), ]$focus[remix_idx] <- 'RE-Mix'

apply( phys[is.na(focus), .SD, .SDcols = paste0(tech_rank_short, '_share')], 1, function(x) max(x)) %>% summary # all <= 0.5
phys[is.na(focus), focus := 'Mixed']

PHYS <- copy(phys)

# load transitioning firms for thresholds that we are interested in
# firms with RE share [0.5, 0.75]
source('R-code/create_figs_main/code_fig2/2_diversify_RE_threshs_withHydro.R')
phys1 <- PHYS[firm_id %in% res1$firm_id, .SD, .SDcols = c('year', 'firm_id', 'focus', tech_rank_short)]
phys1 <- merge(phys1, res1[,c('firm_id', 'transyear')], all = T)


# firms > 0.75 RE share
RE_THRESH <- 0.75
source('R-code/create_figs_main/code_fig2/1_switch_RE_threshs_withHydro.R')
phys2 <- PHYS[firm_id %in% resdetail$firm_id, .SD, .SDcols = c('year', 'firm_id', 'focus', tech_rank_short)]
phys2 <- merge(phys2, resdetail[,c('firm_id', 'transyear')], all = T)


# nr.transitions
transcount1 <- phys1[year == transyear, 'focus'] %>% table %>% as.data.table()
transcount2 <- phys2[year == transyear, 'focus'] %>% table %>% as.data.table()
dplot1 <- rbind( cbind(transcount1, `Renewable share\n(after transition)`  = '(0.5, 0.75]'),
       cbind(transcount2, `Renewable share\n(after transition)`  = '>0.75') )

dplot1$focus <- factor(dplot1$focus, levels = c('Gas', 'Coal', 'Oil', 'FF-Mix'))

p1.1 <- ggplot(dplot1, aes(x=focus, y = N, color = `Renewable share\n(after transition)`, 
                         fill = `Renewable share\n(after transition)` ))  +
  geom_col(position = 'dodge') +
  scale_fill_manual(values = c('firebrick', 'dodgerblue')) +
  scale_color_manual(values = c('firebrick', 'dodgerblue')) +
  theme(
    axis.title = element_text(size=22),
    axis.text = element_text(size = 22),
    legend.text = element_text(size=22),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.background = element_blank(),
    legend.title = element_text(size = 22),
    legend.key.width = unit(2, 'cm')
  ) +
  xlab('Technology focus prior to transition') +
  ylab('# transitioning firms')





## add renewable sizes
PHYS$RE_share <- rowSums( PHYS[, .SD, .SDcols = paste0(tech_rank_short[1:7], '_share')] )
PHYS[, RE_mw := total_mw * RE_share]
PHYS$FF_share <- rowSums( PHYS[, .SD, .SDcols = paste0(tech_rank_short[10:12], '_share')] )
PHYS[, FF_mw := total_mw * FF_share]


# <0.75 transition firms
transfoc1 <- phys1[year == transyear, c('firm_id', 'focus')]
transexp1 <- merge( transfoc1, PHYS[year %in% c(2014, 2023), c('firm_id', 'year', 'RE_mw', 'FF_mw')] )
exp_agg1 <- transexp1[, lapply(.SD, diff), .SDcols = c("RE_mw", "FF_mw"), by = c('firm_id', 'focus')][, .(`RE expansion` = sum(RE_mw),
                                                                                                          `FF expansion` = sum(FF_mw)), by = 'focus']
exp_agg1[, `Renewable share\n(after transition)` := '(0.5, 0.75]']

# >0.75 transition firms
transfoc2 <- phys2[year == transyear, c('firm_id', 'focus')]
transexp2 <- merge( transfoc2, PHYS[year %in% c(2014, 2023), c('firm_id', 'year', 'RE_mw', 'FF_mw')] )
exp_agg2 <- transexp2[, lapply(.SD, diff), .SDcols = c("RE_mw", "FF_mw"), by = c('firm_id', 'focus')][, .(`RE expansion` = sum(RE_mw),
                                                                                                          `FF expansion` = sum(FF_mw)), by = 'focus']
exp_agg2[, `Renewable share\n(after transition)` := '>0.75']

exp_agg <- rbind(exp_agg1, exp_agg2)

exp_agg$focus <- factor(exp_agg$focus, levels = c('Gas', 'Coal', 'Oil', 'FF-Mix'))


dplot2 <- merge(exp_agg, dplot1 )
setnames(dplot2, 'focus', 'Tech focus prior\nto transition')

set.seed(1)
p1.2 <- 
  ggplot(dplot2, aes(x=`FF expansion`/1000, y = `RE expansion`/1000, 
                   color = `Renewable share\n(after transition)`, 
                   shape = `Renewable share\n(after transition)`) )  +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point(size = 5) +
  geom_text_repel(aes(label = `Tech focus prior\nto transition`), 
          nudge_y = c(3,-2,3,0,0,0,0,1),
           nudge_x = c(0,-1,0,-1,.6,-.7,0.5,-2),
            size = 7, show.legend = F) +
  scale_fill_manual(values = c('firebrick', 'dodgerblue')) +
  scale_color_manual(values = c('firebrick', 'dodgerblue')) +
  scale_shape_manual(values = c(15:16)) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size=22),
    axis.text = element_text(size = 22),
    legend.text = element_text(size=22),
    legend.position = c(.99,0.99),
    legend.justification = c(1,1),
    legend.title = element_text(size = 20),
    legend.key.width = unit(2, 'cm')
  ) +
  ylab('Renewable expansion in\n past decade (GW)') +
  xlab('Fossil expansion past decade (GW)') +
  guides(label = "none")
p1.2

## MAKE TRECH TRANSIITION MATRIX
# firms > 0.75 RE share
RE_THRESH <- 0.5
source('R-code/create_figs_main/code_fig2/1_switch_RE_threshs_withHydro.R')
phys3 <- PHYS[firm_id %in% resdetail$firm_id, .SD, .SDcols = c('year', 'firm_id', 'focus', tech_rank_short)]
phys3 <- merge(phys3, resdetail[,c('firm_id', 'transyear')], all = T)

phys3[, maxyear := max(year), by = 'firm_id'] # max year

techtrans <- data.table( focus1 = phys3[year == transyear, focus], focus2 = phys3[year == maxyear, focus]  )
swl <- techtrans %>% table %>% as.data.table
swl[focus1 == 'FF-Mix', focus1:='FF-\nMix']
swl$focus1 <- factor(swl$focus1, levels = c('Gas', 'Coal', 'Gas+Coal', 'Oil', 'FF-\nMix') )

# add total
total_count <- swl[,.(N=sum(N)), by = 'focus1']
total_count[,focus2 := 'Total']
setcolorder(total_count, c(1,3,2))
swl <- rbind(swl, total_count)

swl$focus2 <- factor(swl$focus2, levels = c('Solar', 'Wind', 'So+Wi', 'Hydro', 'Bio', 'Geo', 'RE-Mix', 'Total') )
swl$fill_value <- ifelse(swl$focus2 == "Total", NA, as.integer(swl$N))  # Mask "Total"

p2 <- 
  ggplot(swl, aes(x=focus2, y=focus1, fill = fill_value, label = N ) ) +
  theme_minimal() +
  geom_tile(color = 'black') +
  geom_text(color = 'black', size = 7) +
  scale_y_discrete(limits=rev) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient(low = 'white', high = 'firebrick', na.value = NA) +
  ylab('Technology focus prior transition') + xlab('Technology focus after transition') +
  theme(
    axis.title = element_text(size=22),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    panel.grid = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.25,'cm'),
    legend.key.height = unit(2, 'cm'),
    strip.background = element_rect(fill='white')
  ) +
  guides(fill = guide_colorbar(title = '# firms', title.hjust = 0, frame.colour = "black", frame.linewidth = .5))
p2

pdf('R-code/create_figs_main/fig/nr_tech_trans1.pdf', width = 18, height = 6)
grid.arrange(p2,p1.2, ncol=2, widths = c(0.9,0.6))
grid.text("A", x = 0.01, y = .97, gp = gpar(fontsize = 25, fontface = "bold"))
grid.text("B", x = 0.62, y = .97, gp = gpar(fontsize = 25, fontface = "bold"))
dev.off()



#### EXPANSION BY TECH FOCUS ####
# load transitioning firms for thresholds that we are interested in
# firms with RE share [0.5, 0.75]
source('R-code/create_figs_main/code_fig2/2_diversify_RE_threshs_withHydro.R')
phys1 <- PHYS[firm_id %in% res1$firm_id, .SD, .SDcols = c('year', 'firm_id', 'focus', tech_rank_short)]
phys1 <- merge(phys1, res1[,c('firm_id', 'transyear')], all = T)

firmfoc <- phys1[year == transyear, c('firm_id', 'focus')]
setnames(firmfoc, 'focus', 'focus1')
phys1 <- merge(phys1, firmfoc, by = 'firm_id', all=T)


dphys1 <- melt( phys1[ , lapply(.SD,sum), .SDcols = tech_rank_short, by = c('year', 'focus1')], id.vars = c('year', 'focus1') )
dphys1$focus1 <- factor(dphys1$focus1, levels = c('Gas', 'Coal', 'Oil', 'FF-Mix'))
dphys1$variable <- factor( tech_rankings[ match( as.character( dphys1$variable ), tech_rank_short) ], levels = tech_rankings)

dattext <- dphys1[, .(value=sum(value)), by = c('year','focus1')]
dattext <- dattext[, .SD[value == max(value)], by = 'focus1']  # only keep mak value
dattext$year <- 2001
dattext$variable <- NA
dattext$text <- paste0('Prior focus: ', dattext$focus1)
dattext$value <- dattext$value*0.96

p3 <- ggplot( dphys1, aes(x=year, y=value/1000, fill = variable) ) +
  geom_area() +
  geom_text(data=dattext, aes(x=year, y=value/1000, label=text), size = 7, hjust = 0, inherit.aes = F, show.legend = F) +
  facet_wrap('focus1', scales = 'free_y') +
  scale_fill_manual(values = tech_col) +
  scale_x_continuous(breaks = seq(2001,2026,5))+
  theme(
    axis.title = element_text(size=22),
    axis.text = element_text(size = 22),
    legend.text = element_text(size=22),
    panel.grid = element_blank(),
    legend.position = 'none',
    legend.justification = c(1,1),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key.width = unit(2, 'cm'),
    strip.background = element_blank(),
    strip.text = element_blank()
  ) +
  ylab('GW owned by transitioning firms') + xlab('')
p3

# firms > 0.75 RE share
RE_THRESH <- 0.75
source('R-code/create_figs_main/code_fig2/1_switch_RE_threshs_withHydro.R')
phys2 <- PHYS[firm_id %in% resdetail$firm_id, .SD, .SDcols = c('year', 'firm_id', 'focus', tech_rank_short)]
phys2 <- merge(phys2, resdetail[,c('firm_id', 'transyear')], all = T)
firmfoc <- phys2[year == transyear, c('firm_id', 'focus')]
setnames(firmfoc, 'focus', 'focus1')
phys2 <- merge(phys2, firmfoc, by = 'firm_id', all=T)


dphys2 <- melt( phys2[ , lapply(.SD,sum), .SDcols = tech_rank_short, by = c('year', 'focus1')], id.vars = c('year', 'focus1') )
dphys2$focus1 <- factor(dphys2$focus1, levels = c('Gas', 'Coal', 'Oil', 'FF-Mix'))
dphys2$variable <- factor( tech_rankings[ match( as.character( dphys2$variable ), tech_rank_short) ], levels = tech_rankings)


dattext <- dphys2[, .(value=sum(value)), by = c('year','focus1')]
dattext <- dattext[, .SD[value == max(value)], by = 'focus1']  # only keep mak value
dattext$year <- 2001
dattext$variable <- NA
dattext$text <- paste0('Prior focus: ', dattext$focus1)
dattext$value <- dattext$value*0.96

p4 <- ggplot( dphys2, aes(x=year, y=value/1000, fill = variable) ) +
  geom_area() +
  geom_text(data=dattext, aes(x=year, y=value/1000, label=text), size = 7, hjust = 0, inherit.aes = F, show.legend = F) +
  facet_wrap('focus1', scales = 'free_y') +
  scale_fill_manual(values = tech_col) +
  scale_x_continuous(breaks = seq(2001,2026,5))+
  theme(
    axis.title = element_text(size=22),
    axis.text = element_text(size = 22),
    legend.text = element_text(size=22),
    panel.grid = element_blank(),
    legend.position = 'none',
    legend.justification = c(1,1),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key.width = unit(2, 'cm'),
    strip.background = element_blank(),
    strip.text = element_blank()
  ) +
  ylab('GW owned by transitioning firms') + xlab('')
p4


# plot next to each other
p3_1 <- p3 +
  facet_wrap('focus1', ncol=1, scales = 'free_y')
p4_1 <- p4 +
  facet_wrap('focus1', ncol=1, scales = 'free_y')

spacer <- textGrob("")


g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

p3_withlegend <- p3 +
  theme(legend.position = 'right',
        legend.text = element_text(size=22),
        legend.justification.right = 'left') 

legend <- g_legend(p3_withlegend)

legend_height <- grobHeight(legend)
spacer_grob <- spacer
padding_grob <- rectGrob(gp = gpar(col = NA))  # Transparent padding grob

# Create a layout that combines padding, the legend, and more padding for vertical centering
legend_with_padding <- arrangeGrob(padding_grob, legend, padding_grob,
                                   heights = unit.c(unit(1, "null"), legend_height, unit(1, "null")))





cairo_pdf('R-code/create_figs_main/fig/transitioning_expand_focus.pdf', width = 17, height = 11)
grid.arrange(spacer, spacer, spacer,
             p3_1, p4_1, legend_with_padding, 
             ncol = 3, 
             heights = c(0.035, 1), 
             widths = c(.8,.8,0.35))
grid.text("A: Renewable share (0.5, 0.75]:", x = 0.13, y = .98, gp = gpar(fontsize = 19, fontface = "bold"))
grid.text("B: Renewable share > 0.75:", x = 0.57, y = .98, gp = gpar(fontsize = 19, fontface = "bold"))
dev.off()

