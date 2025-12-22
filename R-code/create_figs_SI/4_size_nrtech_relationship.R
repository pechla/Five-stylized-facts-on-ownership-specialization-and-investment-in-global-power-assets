rm(list=ls())
source('R-code/source_code/libraries.R')


dx <- fread('data_processed/R-data-output/firm_tech_year.csv')
datx <- dx[total_mw > 0 & year == 2024,]

# median and mean stats
datx[, median(total_mw)]
datx[, mean(total_mw)]


# create firm focus
datx <- cbind(datx[, c('firm_id', 'nr.tech', 'total_mw')], datx[, lapply(.SD, function(x) x/total_mw ), .SDcols = tech_rank_short] )

datx[, focus := 'Mixed']
datx[Bio >0.5, focus := 'Biopower']
datx[Hydro >0.5, focus := 'Hydropower']
datx[PV>0.5, focus := 'Solar PV']
datx[Solar>0.5, focus := 'Solar thermal']
datx[Offshore>0.5, focus := 'Offshore wind']
datx[Onshore>0.5, focus := 'Onshore wind']
datx[Geo>0.5, focus := 'Geothermal']
datx[Waste>0.5, focus := 'Waste']
datx[Nuclear >0.5, focus := 'Nuclear']
datx[Coal >0.5, focus := 'Coal']
datx[Gas>0.5, focus := 'Gas']
datx[Oil>0.5, focus := 'Oil']
datx[Coal+Gas+Oil >0.5 & Coal<0.5 & Gas < 0.5 & Oil < 0.5, focus := 'Mixed-FF']
datx[Bio+Hydro+PV+Solar+Offshore+Onshore+Geo+Waste >0.5 & Bio<0.5 & Hydro< 0.5 & PV< 0.5 & Solar< 0.5 & Offshore< 0.5 & Onshore< 0.5 & Geo < 0.5 & Waste< 0.5, focus := 'Mixed-RE']
table(datx$focus)

# order based on median size
techmed <- datx[,median(total_mw), by = 'focus']
techmed <- techmed[order(-techmed$V1),]

datx$focus <- factor(datx$focus, levels = techmed$focus)

datn <- datx[, .(n=sum(total_mw > 0)), by = 'focus']
datn$N <- paste0('N=', ifelse( nchar(datn$n) ==4 , paste0( substr(datn$n,1,1), ',',  substr(datn$n,2,4) ), datn$n) )


p1 <- ggplot(datx, aes(y=focus, x= total_mw, fill = focus)) +
  geom_text(data = datn, aes(y=focus, x=10^(-6), label = N), hjust = 'left', size = 7, show.legend = F) +
  geom_boxplot(fill = 'dodgerblue', alpha = 0.5) +
  #scale_fill_manual(values = c(tech_col3[ match(techmed$focus, tech_rankings2) ])) +
  stat_summary(fun = mean, geom = 'point', shape = 4, size = 8, color = "firebrick") +
  scale_x_continuous("Firm capacity in MW",
                     limits = c(10^(-6),1.5*10^5),
                     breaks = scales::trans_breaks("log10", function(x) 10^x),
                     labels = scales::trans_format("log10", scales::math_format(10^.x)),
                     trans = "log10") +
  annotation_logticks(sides = 'b') +
  ylab('Technology focus of firm') +
  scale_y_discrete(limits = rev) +
  theme(
    axis.title = element_text(size=25),
    axis.text = element_text(size = 25),
    panel.grid = element_blank(),
    legend.position = 'none',
    legend.justification = c(1,0),
    legend.background = element_blank(),
    legend.title = element_blank()
  ) 



# size and number of tech
datx[, nr.tech.agg := ifelse(as.integer(nr.tech) >5, '>5', nr.tech)]
datx$nr.tech.agg <- factor(datx$nr.tech.agg, levels = c('1','2','3', '4',  '5', '>5'))

# text

datn <- datx[, table(nr.tech.agg)] %>% as.data.table()
datn[, n:=paste0('N=',N)]
datn[nchar(N) ==4,  n := paste0('N=', substr(N,1,1), ',',  substr(N,2,4)) ]
datn[nchar(N) ==5,  n := paste0('N=', substr(N,1,2), ',',  substr(N,3,5)) ]

p2 <- ggplot(datx, aes(y=nr.tech.agg, x=total_mw, group = nr.tech.agg)) +
  geom_boxplot(fill = "dodgerblue", alpha = 0.5) +
  stat_summary(fun = mean, geom = 'point', shape = 4, size = 8, color = "firebrick") +
  geom_text(data = datn, aes(x=10^(-6), label = n), hjust = 'left', size = 7, show.legend = F) +
  ylab('Number of technologies in portfolio') +
  xlab('Firm capacity in MW') +
  scale_x_continuous(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    trans = "log10") +
  annotation_logticks(sides = 'b') +
  #  scale_x_discrete(labels = c('1','2','3','4', expression(phantom(x) >=5))) +
  theme(
    axis.title = element_text(size=25),
    axis.text = element_text(size = 25),
    plot.title = element_text(size = 25),
    panel.grid = element_blank(),
    legend.position = 'none',
    # legend.justification = c(1,1),
    # legend.key.width = unit(1.2, 'cm'),
    # legend.background = element_blank(),
    # legend.title = element_text(size = 20),
    # legend.text = element_text(size = 20),
    strip.text = element_text(size = 20)
  ) 

pdf('R-code/create_figs_SI/fig/size_panels.pdf', width = 19, height = 8)
grid.arrange(p1,p2, ncol  =2, widths = c(1.2,1) )
grid.text("A", x = 0.01, y = 0.98, gp = gpar(fontsize = 25, fontface = "bold"))
grid.text("B", x = 0.56, y = 0.98, gp = gpar(fontsize = 25, fontface = "bold"))
dev.off()

