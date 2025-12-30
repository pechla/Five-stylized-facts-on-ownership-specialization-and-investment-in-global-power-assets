rm(list=ls())
source('R-code/source_code/libraries.R')

d1 <- readRDS('data_processed/data_temp/Fig1_data.RDS')
d2 <- readRDS('data_processed/data_temp/firmtype_capchange_data.RDS')

d1$`Share based on:` <- ifelse(d1$`Share based on:`  == "Counts\n(number of firms)", 
                               "Counts (# firms)", "Weighted (MW)")

focus_col <- c("orange2", 'dodgerblue', "pink", 'firebrick', "magenta", "black" )
names(focus_col) <- levels(d2$`Firm focus:`)

d1$`Share based on:` <- factor(d1$`Share based on:`, levels = c("Weighted (MW)", "Counts (# firms)"))
d1[, lab := paste0(round(V2*100),'%')]

p1 <- 
ggplot(d1, aes(y=type, x=V2*100, shape = `Share based on:`, fill =`Share based on:`)) +
  geom_col(position='dodge') +
  geom_text(aes(label = lab), position = position_dodge(0.9), hjust = -.4, size = 6) +
  scale_x_continuous(limits = c(0,100), breaks = seq(0,100, by = 20))+
  scale_fill_manual(values = c('firebrick','dodgerblue')) +
  theme(
    axis.title = element_text(size=21),
    axis.text = element_text(size = 19),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = c(0.99,0.02),
    legend.justification = c(1,0.05),
    panel.grid.major.y = element_blank(),
    legend.background = element_rect(color = 'black', size = 0.1),
    legend.key.width = unit(1.5,'cm')
  ) +
  xlab('%') + ylab('Share of ...') +
  guides(fill = guide_legend(reverse=TRUE))

p2 <- ggplot(d2, aes(x = install_rel*100,  y = tech, fill = `Firm focus:`)) + 
  geom_col() +
  scale_fill_manual(values = focus_col) +
  theme(
    axis.title = element_text(size=20),
    axis.text = element_text(size = 20),
    panel.grid = element_blank(),
    legend.position = 'top',
    legend.background = element_blank(),
    legend.title = element_text(size=20),
    legend.text = element_text(size = 20),
    legend.key.width = unit(2,'cm')
  ) + 
  scale_x_continuous(breaks = seq(0,110,10)) +
  scale_y_discrete(limits = rev)+
  guides(fill = guide_legend(nrow = 3)) +
  xlab("Capacity additions by firm focus (%)") + ylab("Technologies")

cairo_pdf('R-code/create_figs_main/fig/fig1_combined.pdf', width = 18) # to print unicode properly
grid.arrange(p1,p2, ncol = 2, widths = c(1.3,1))
grid.text("A", x = 0.02, y = .97, gp = gpar(fontsize = 25, fontface = "bold"))
grid.text("B", x = 0.58, y = .97, gp = gpar(fontsize = 25, fontface = "bold"))
dev.off()
