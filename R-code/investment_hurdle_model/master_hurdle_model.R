rm(list=ls())

# STAGE 1: INVESTMENT YES OR NO
source('R-code/investment_hurdle_model/1_decision_trees/1_tree_invest_YN.R')
source('R-code/investment_hurdle_model/2_logistic_regressions/1_mfx_invest_prob.R')

# STAGE 2 IF YES, CHOOSE WHICH TECH
source('R-code/investment_hurdle_model/1_decision_trees/2_tree_tech_choice.R')
source('R-code/investment_hurdle_model/2_logistic_regressions/2_mfx_tech_choice.R')

# STAGE 2 IF TECH, HOW MUCH
source('R-code/investment_hurdle_model/3_linear_regressions/linear_regressions.R')