rm(list=ls())

# create data in data_temp/ to create figure
source('R-code/create_figs_main/code_fig1/0_build_data_panelA.R')
source('R-code/create_figs_main/code_fig1/0_build_data_panelB.R')

# create figure
source('R-code/create_figs_main/code_fig1/fig1_several_facts.R')