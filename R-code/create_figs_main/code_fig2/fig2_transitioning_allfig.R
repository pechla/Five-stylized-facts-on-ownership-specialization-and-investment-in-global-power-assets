rm(list=ls())
source('R-code/source_code/libraries.R')

RE_THRESH <- seq(0.5,1, by = 0.05)

source('R-code/create_figs_main/code_fig2/1_switch_RE_threshs_withHydro.R')
RES1 <- copy( resagg) 
RES1$plotshare <- paste0('>', RES1$re_thresh)
RES1$type <- 'Including hydro'

RESDET1 <- copy( resdetail )
RESDET1$type <- 'Including hydro'

# overall figure (exclude hydro)
source('R-code/create_figs_main/code_fig2/1_switch_RE_threshs_noHydro.R')
RES2 <- copy( resagg) 
RES2$plotshare <- paste0('>', RES2$re_thresh)
RES2$type <- 'Excluding hydro'

RESDET2 <- copy( resdetail )
RESDET2$type <- 'Excluding hydro'


# combine results for single plot
RES <- rbind(RES1, RES2)
RESDET <- rbind(RESDET1, RESDET2)

p1 <- ggplot( RES, aes(x=re_thresh*100, y=count, color = type, shape = type) ) + 
  geom_point(size = 4)+
  geom_line(linewidth = 1.25) +
  scale_shape_manual(values = c(15:18,8)) +
  scale_color_manual(values = c('firebrick', 'dodgerblue')) +
  theme(
    axis.title = element_text(size=20),
    axis.text = element_text(size = 20),
    legend.text = element_text(size=22),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key.width = unit(2, 'cm')
  ) +
  ylab('# transitioning firms') + xlab('Renewable portfolio share in % (2021-2023)')

RES[type == 'Including hydro' & re_thresh == 0.5,mw_23/count]
RES[type == 'Excluding hydro' & re_thresh == 0.5, mw_23/count]

p2 <- ggplot( RES, aes(x=re_thresh*100, y=mw_23/1000, color = type, shape = type) ) + 
  geom_point(size = 4)+
  geom_line(linewidth = 1.25) +
  scale_shape_manual(values = c(15:18,8)) +
  scale_color_manual(values = c('firebrick', 'dodgerblue')) +
  theme(
    axis.title = element_text(size=20),
    axis.text = element_text(size = 20),
    legend.text = element_text(size=22),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key.width = unit(2, 'cm')
  ) +
  ylab('GW owned by transitioning firms') + xlab('Renewable portfolio share in % (2021-2023)')


#### capacity evolution plot ####


# firms with RE share [0.5, 0.75]
source('R-code/create_figs_main/code_fig2/2_diversify_RE_threshs_withHydro.R')
phys1 <- phys[firm_id %in% res1$firm_id, .SD, .SDcols = c('year', tech_rank_short)]
dphys1 <- melt( phys1[ , lapply(.SD,sum), .SDcols = tech_rank_short, by = 'year'], id.vars = 'year')
dphys1$variable <- factor( tech_rankings[ match( as.character( dphys1$variable ), tech_rank_short) ], levels = tech_rankings)

# firms > 0.75 RE share
phys2 <- phys[firm_id %in% RESDET1[re_thresh == 0.75, firm_id], .SD, .SDcols = c('year', tech_rank_short)]
dphys2 <- melt( phys2[ , lapply(.SD,sum), .SDcols = tech_rank_short, by = 'year'], id.vars = 'year')
dphys2$variable <- factor( tech_rankings[ match( as.character( dphys2$variable ), tech_rank_short) ], levels = tech_rankings)

p3 <-
  ggplot( dphys1, aes(x=year, y=value/1000, fill = variable) ) +
  geom_area() +
  scale_fill_manual(values = tech_col) +
  scale_x_continuous(breaks = seq(2001,2023,5))+
  theme(
    axis.title = element_text(size=22),
    axis.text = element_text(size = 22),
    legend.text = element_text(size=22),
    panel.grid = element_blank(),
    legend.position = 'none',
    legend.justification = c(1,1),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key.width = unit(2, 'cm')
  ) +
  ylab('GW owned by transitioning firms') + xlab('')

p4 <- ggplot( dphys2,
              aes(x=year, y=value/1000, fill = variable) ) +
  geom_area() +
  scale_fill_manual(values = tech_col) +
  scale_x_continuous(breaks = seq(2001,2023,5))+
  theme(
    axis.title = element_text(size=22),
    axis.text = element_text(size = 22),
    legend.text = element_text(size=22),
    panel.grid = element_blank(),
    legend.position = 'none',
    legend.justification = c(1,1),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key.width = unit(2, 'cm')
  ) +
  ylab('GW owned by transitioning firms') + xlab('')

# Extract legend from one of the plots
# Function to extract the legend from a ggplot object
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

p3_withlegend <- p3 + 
  theme(legend.position = 'bottom',
        legend.justification.bottom = "bottom",) +
  guides(fill = guide_legend(nrow=2))

legend <- g_legend(p3_withlegend)

dattext1 <- data.frame(year=2001, value = dphys1[year==2022,sum(value)], variable = NA, 
                       text = 'Renewable share (50%, 75%]' )
dattext2 <- data.frame(year=2001, value = dphys2[year==2022,sum(value)], variable = NA, 
                       text = 'Renewable share > 75%' )

p3_new <- p3+ 
  geom_text(data=dattext1, aes(label = text), size = 8, hjust = 0  )
p4_new <- p4+ 
  geom_text(data=dattext2, aes(label = text), size = 8, hjust = 0  )

# Arrange the two plots and the legend below
cairo_pdf('R-code/create_figs_main/fig/transitioning_allfig.pdf', width = 18, height = 12)
grid.arrange(
  arrangeGrob(p1, p2, p3_new, p4_new, ncol = 2, heights = c(1,1.1)),  # Arrange four plots in a 2x2 grid
  legend,                            # Centered legend below
  nrow = 2,                               # Two rows: one for plots, one for the legend
  heights = c(10, .8)                      # Adjust the heights for proper space for the legend
)
grid.text("A", x = 0.01, y = .98, gp = gpar(fontsize = 25, fontface = "bold"))
grid.text("B", x = 0.53, y = .98, gp = gpar(fontsize = 25, fontface = "bold"))
grid.text("C", x = 0.01, y = .56, gp = gpar(fontsize = 25, fontface = "bold"))
grid.text("D", x = 0.51, y = .56, gp = gpar(fontsize = 25, fontface = "bold"))
dev.off()




## numbers in main text
dphys2[variable %in% c('Coal', 'Gas', 'Oil'), sum(value), by = 'year']
# write.csv(RESDET, file = 'data_processed/data_temp/transitioning_firms.csv')
