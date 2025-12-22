rm(list=ls())
source('R-code/source_code/libraries.R')


d <- fread('data_processed/R-data-output/firm_tech_year.csv')
dx <- d[total_mw > 0 & year == 2024,]

# compute tech shares
dat <- cbind(dx[, c('firm_id', 'nr.tech', 'total_mw')], 
             dx[, lapply(.SD, function(x) x/total_mw ), .SDcols = tech_rank_short] )

dl <- melt(dat, id.vars = c('firm_id', 'nr.tech', 'total_mw'), variable.name = 'tech', value.name = 'share')
dl <- dl[share>0,]


max2tech <- dl[nr.tech == 2, .(`Primary (largest) tech` = max(share)[1]*100, 
                               `Secondary (smallest) tech` = min(share)[1]*100), by = 'firm_id']

# Reshape data to long format
max2tech_long <- melt(max2tech, measure.vars = c("Primary (largest) tech", "Secondary (smallest) tech"), 
                      variable.name = "type", value.name = "share")

# Calculate mean for labeling
median_values <- max2tech_long[, .(median_share = median(share)), by = type]

# Create the plot
pdf('R-code/create_figs_SI/fig/tech_concentr_2tech.pdf', width = 13, height = 5)
ggplot(max2tech_long, aes(x = share)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black", alpha = 0.7) +
  geom_vline(data = median_values, aes(xintercept = median_share), 
             color = "red", linetype = "dashed", size = 1) +
  geom_text(data = median_values, aes(x = median_share, y = 140, label = paste0('Median = ',round(median_share, 1),'%')), 
            color = "red", vjust = -0.5, hjust = c(1.1,-.1), size = 6) +
  facet_wrap(~ type, scales = "free_x") +
  labs(title = "A. 2-technology firms",
       x = "Technology portfolio share (in %)",
       y = "Count") +
  theme(
    strip.text = element_text(size = 20),
    axis.title = element_text(size=20),
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold"),
    strip.background = element_blank()
  )
dev.off()



max3tech <- dl[nr.tech == 3, .(`Primary (largest) tech` = max(share)[1]*100, 
                               `Tertiary (smallest) tech` = min(share)[1]*100), by = 'firm_id']

# Reshape data to long format
max3tech_long <- melt(max3tech, measure.vars = c("Primary (largest) tech", "Tertiary (smallest) tech"), 
                      variable.name = "type", value.name = "share")

# Calculate mean for labeling
median_values <- max3tech_long[, .(median_share = median(share)), by = type]

# Create the plot
pdf('R-code/create_figs_SI/fig/tech_concentr_3tech.pdf', width = 13, height = 5)
ggplot(max3tech_long, aes(x = share)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black", alpha = 0.7) +
  geom_vline(data = median_values, aes(xintercept = median_share), 
             color = "red", linetype = "dashed", size = 1) +
  geom_text(data = median_values, aes(x = median_share, y = 100, label = paste0('Median = ',round(median_share, 1),'%')), 
            color = "red", vjust = -0.5, hjust = c(1.1,-.1), size = 6) +
  facet_wrap(~ type, scales = 'free_x') +
  labs(title = "B. 3-technology firms",
       x = "Technology portfolio share (in %)",
       y = "Count") +
  theme(
    strip.text = element_text(size = 20),
    axis.title = element_text(size=20),
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold"),
    strip.background = element_blank()
  )
dev.off()


max5tech <- dl[nr.tech >= 5, .(`Primary (largest) tech` = max(share)[1]*100, 
                               `Smallest tech` = min(share)[1]*100), by = 'firm_id']

# Reshape data to long format
max5tech_long <- melt(max5tech, measure.vars = c("Primary (largest) tech", "Smallest tech"), 
                      variable.name = "type", value.name = "share")

# Calculate mean for labeling
median_values <- max5tech_long[, .(median_share = median(share)), by = type]

# Create the plot
pdf('R-code/create_figs_SI/fig/tech_concentr_5tech.pdf', width = 13, height = 5)
ggplot(max5tech_long, aes(x = share)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black", alpha = 0.7) +
  geom_vline(data = median_values, aes(xintercept = median_share), 
             color = "red", linetype = "dashed", size = 1) +
  geom_text(data = median_values, aes(x = median_share, y = 100, label = paste0('Median = ',round(median_share, 1),'%')), 
            color = "red", vjust = -0.5, hjust = c(1.1,-.1), size = 6) +
  facet_wrap(~ type, scales = 'free_x') +
  labs(title = "C. >4-technology firms",
       x = "Technology portfolio share (in %)",
       y = "Count") +
  theme(
    strip.text = element_text(size = 20),
    axis.title = element_text(size=20),
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold"),
    strip.background = element_blank()
  )
dev.off()


