rm(list=ls())
source('R-code/source_code/libraries.R')

dx <- fread('data_processed/R-data-output/firm_tech_year.csv')
dx23 <- dx[total_mw > 0 & year == 2024,]

# relative shares
dx23[, Share := total_mw / sum(total_mw)]
dx23


###
D <- dx23[,.SD, .SDcols = c(tech_rank_short, 'total_mw')]
techmap <- data.frame( long = c('All', tech_rankings), short = c('total_mw', tech_rank_short) )

lorenzdat <- NULL
for (i in 1:nrow(techmap)) {
  
  # subset relevant tech
  mw <- D[,.SD, .SDcols = techmap$short[i] ] %>% unlist()
  sort_mw <- sort( mw[mw>0], decreasing = T ) # only positive and sorted
  # compute shares
  share <- sort_mw / sum(sort_mw)
  cumshare <- cumsum(share)
  
  # obs
  n <- length(sort_mw)
  # total cap
  total <- sum(sort_mw)
  
  lorenzdat <- rbind(lorenzdat, 
                     data.table(share_firm = (1:n)/n*100, 
                                cumown = cumshare*100, 
                                tech = techmap$long[i] )
  )
  
}


reltech <- c('All', 'Solar PV', 'Wind Onshore', 'Hydro', 'Gas', 'Coal')
lorplot <- lorenzdat[tech %in% reltech, ]
lorplot$tech <- factor(lorplot$tech, levels = reltech)

share_firms <- NULL
for (i in 1:length(reltech)) {
  
  share_firms <- c(share_firms, lorplot[tech == reltech[i] & cumown<=80, ][order(cumown, decreasing = T),][1, share_firm] )
  
}
names(share_firms) <- reltech

setnames(lorplot, 'tech', 'Technology group:')

p1 <- ggplot(lorplot, aes(x=share_firm, y=cumown, color = `Technology group:`)) +
  geom_line(linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  scale_color_manual(values = c(All = 'firebrick', tech_col) ) +
  scale_y_continuous(breaks = seq(0,100, by= 10), limits = c(0,100)) +
  scale_x_continuous(breaks = seq(0,100, by= 10)) +
  xlab('Cumulative share of firms (%)\n(ordered from largest to smallest)') + ylab('Cumulative share of capacity owned (%)') +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size=16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.key.width = unit(1.2, 'cm'),
    legend.position = c(.99,.02),
    legend.justification = c(1,0),
    legend.background = element_rect(colour = 'black')
  ) 



## density with mixture
### fit log normal mixture
total_mw <- dx23$total_mw
log_capacities <- log(total_mw)
k <- 2
mixture_fit <- normalmixEM(log_capacities, k = k)
log_capacities_df <- data.table(log_capacity = log_capacities)

# Create a sequence for density plotting
component_densities <- data.table( x= seq(min(log_capacities), max(log_capacities), length.out = length(log_capacities)),
                                   capacity = log_capacities )

component_densities[, comp1 := ( mixture_fit$lambda[1] * dnorm(x, mean = mixture_fit$mu[1], sd = mixture_fit$sigma[1]) ) ]
component_densities[, comp2 := ( mixture_fit$lambda[2] * dnorm(x, mean = mixture_fit$mu[2], sd = mixture_fit$sigma[2]) ) ]
component_densities[, mixed := comp1 + comp2 ]


num_firms <- length(total_mw)
total_capacity <- sum(total_mw)
max_capacity <- max(total_mw)
mean_capacity <- mean(total_mw)
median_capacity <- median(total_mw)

p2 <- ggplot(component_densities) + 
  geom_histogram(aes(x = capacity, y = ..density..), fill = "antiquewhite", color = "black", alpha = 0.7) +
  geom_line(aes(x = x, y = mixed, color = "Lognormal mixture"), linewidth = 1.2) +
  scale_color_manual(values = c("Lognormal mixture" = "black")) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0,1),
    legend.justification = c(0,1),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_text(size=16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.key.width = unit(1.2, 'cm'),
  ) +
  xlab('Natural log of firm capacity (MW)') + ylab('Density') +
  annotate("text", x = 6.3, y = 0.25, 
           label = paste0(
             "# firms: ", num_firms, "\n",
             "Total: ", round(total_capacity/10^6, 1), ' TW', "\n",
             "Max: ", round(max_capacity/10^3, 0), ' GW', "\n",
             "Mean: ", round(mean_capacity, 0), ' MW', "\n",
             "Median : ", round(median_capacity, 0), ' MW'
           ), 
           size = 5.5, hjust = 0, vjust = 1, color = "black")




pdf('R-code/create_figs_main/fig/fig0.pdf', width = 13, height = 6)
grid.arrange(p2,p1,ncol=2)
grid.text("A", x = 0.01, y = .97, gp = gpar(fontsize = 19, fontface = "bold"))
grid.text("B", x = 0.51, y = .97, gp = gpar(fontsize = 19, fontface = "bold"))
dev.off()

