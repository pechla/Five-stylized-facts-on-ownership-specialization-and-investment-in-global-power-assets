rm(list=ls())
source('R-code/source_code/libraries.R')

# share of firms investing
data_summary <- function(x) {
  data.table(y=mean(x),ymin=min(x),ymax=max(x))
}


D <- fread('data_processed/R-data-output/firm_tech_year.csv')
source('R-code/create_figs_main/code_fig1/fig1_helper_count.R')

D <- fread('data_processed/R-data-output/firm_tech_year.csv')
source('R-code/create_figs_main/code_fig1/fig1_helper_weight.R')


# make figure
rownames <- c('firms owning\nonly 1 technology', 
              'firms owning\n>2 technologies',
              'firms investing\nper year', 
              'firms investing in \u22651\n year in past decade', 
              'firms investing in \u22655\n years in past decade',
              'investing firms adding\nmore than 1 technology', 
              'investments from\nnew firms',
              'investing firms investing\n in new technology'
)
rownames <- factor(rownames, levels = rev(rownames) )

d1 <- data.table(type = rownames, variable = 'Counts\n(number of firms)', sharetable.count)
d2 <- data.table(type = rownames, variable = 'Weighted\n(by capacity)', sharetable.weight)
dl <- rbind(d1,d2)#melt(d, id.vars = 'type')
setnames(dl, 'variable', 'Share based on:')


saveRDS(dl, 'data_processed/data_temp/Fig1_data.RDS')
