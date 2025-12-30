rm(list=ls())
source('R-code/source_code/libraries.R')

set.seed(123)

dreg <- fread('data_processed/R-data-output/regtable.csv')

##### Dep Var: Probability investing in wind conditional on investing at all ####
sharecols <- paste0(tech_rankings, '_share')

# create factors
dreg$nr.5yr_invest <- factor(dreg$nr.5yr_invest)
dreg$year_cat <- factor(dreg$year)

# log cap
dreg$log_total_mw <- (dreg$total_mw) # use unlogged
dreg$type <- ifelse(dreg$type == 'Public firm', 'Public', ifelse(dreg$type == 'Private firm', 'Private', dreg$type))
dreg$nr.tech <- as.integer(dreg$nr.tech)

#######################
#### FIT THE TREES ####
#######################

#### SOLAR PV #### 
dreg_sub <- dreg[invest_TF == T & total_mw>0 & year %in% 2014:2023,] # no investment decision in 2024
techs <- c('PV')
# make tech-specific
setnames(dreg_sub, techs, 'techshare')
setnames(dreg_sub, paste0(techs,'_cap'), 'tech_capacity')
dreg_sub$tech_cap <- (dreg_sub$tech_capacity) # not use log

dreg_sub$invest_tech <- ifelse(dreg_sub[, .SD, .SDcols = paste0(techs, '_investsize')] > 0, 1, 0)

## previous 5-year investments
tech_other <- tech_rank_short[ !(tech_rank_short %in% techs) ]
check_tech <- apply( dreg_sub[, .SD, .SDcols = paste0('nr.5yr_invest_',techs)], 1, function(x) any(x>0) )
check_other <- apply( dreg_sub[, .SD, .SDcols = paste0('nr.5yr_invest_',tech_other)], 1, function(x) any(x>0) )

dreg_sub$invest_type.5yrs <- ifelse( (!check_tech) & (!check_other), 'none', 
                                     ifelse(  check_tech & check_other, 'PV & others', 
                                              ifelse(  check_tech & (!check_other), 'only PV', 'only others')))

dreg_sub$invest_type.5yrs <- factor(dreg_sub$invest_type.5yrs, levels = c("none", 
                                                                          "PV & others",
                                                                          "only PV",
                                                                          "only others"))

dreg_sub$invest_type.simple <- ifelse( (!check_tech) & (!check_other), 'none or only others', 
                                       ifelse(  check_tech & check_other, 'PV & others', 
                                                ifelse(  check_tech & (!check_other), 'PV & others', 'none or only others')))

dreg_sub$invest_type.simple <- factor(dreg_sub$invest_type.simple, levels = c("PV & others", "none or only others"))

D <- dreg_sub[, c('invest_tech','year_cat', 'total_mw', 'tech_cap', 'nr.tech', 'type', 'invest_type.5yrs', 'invest_type.simple', 'techshare'  )]
D[, invest_tech := factor(ifelse(invest_tech==1,'Yes','No'))]

setnames(D, c('invest_tech','year_cat', 'total_mw', 'tech_cap', 'nr.tech', 'type', 'invest_type.5yrs', 'invest_type.simple', 'techshare'  ),
         c('invest', 'Year', 'Total capacity (MW)', 'PV cap. (MW)', '# technologies', 'Firm type', 'Past choice 1', 'Past choice 2', 'PV share'  ))
D[,`Past choice 1` := NULL]
setnames(D, "Past choice 2", "Past choice")

tree1 <- rpart(invest  ~ ., data = D, method = "class", cp = 10^(-9), minsplit = 10, model = TRUE)


#### ONSHORE #### 
dreg_sub <- dreg[invest_TF == T & total_mw>0 & year %in% 2014:2023,]
techs <- c('Onshore')
# make tech-specific
setnames(dreg_sub, techs, 'techshare')
setnames(dreg_sub, paste0(techs,'_cap'), 'tech_capacity')
dreg_sub$tech_cap <- (dreg_sub$tech_capacity) # not use log

dreg_sub$invest_tech <- ifelse(dreg_sub[, .SD, .SDcols = paste0(techs, '_investsize')] > 0, 1, 0)

## previous 5-year investments
tech_other <- tech_rank_short[ !(tech_rank_short %in% techs) ]
check_tech <- apply( dreg_sub[, .SD, .SDcols = paste0('nr.5yr_invest_',techs)], 1, function(x) any(x>0) )
check_other <- apply( dreg_sub[, .SD, .SDcols = paste0('nr.5yr_invest_',tech_other)], 1, function(x) any(x>0) )

dreg_sub$invest_type.5yrs <- ifelse( (!check_tech) & (!check_other), 'none', 
                                     ifelse(  check_tech & check_other, 'wind & others', 
                                              ifelse(  check_tech & (!check_other), 'only wind', 'only others')))

dreg_sub$invest_type.5yrs <- factor(dreg_sub$invest_type.5yrs, levels = c("none", 
                                                                          "wind & others",
                                                                          "only wind",
                                                                          "only others"))


dreg_sub$invest_type.simple <- ifelse( (!check_tech) & (!check_other), 'none, wind & others', 
                                       ifelse(  check_tech & check_other, 'none, wind & others', 
                                                ifelse(  check_tech & (!check_other), 'none, wind & others', 'only others')))

dreg_sub$invest_type.simple <- factor(dreg_sub$invest_type.simple, levels = c("none, wind & others", "only others"))

D <- dreg_sub[, c('invest_tech','year_cat', 'total_mw', 'tech_cap', 'nr.tech', 'type', 'invest_type.5yrs', 'invest_type.simple', 'techshare'  )]
D[, invest_tech := factor(ifelse(invest_tech==1,'Yes','No'))]

setnames(D, c('invest_tech','year_cat', 'total_mw', 'tech_cap', 'nr.tech', 'type', 'invest_type.5yrs','invest_type.simple', 'techshare'  ),
         c('invest', 'Year', 'Total capacity (MW)', 'Wind  capacity (MW)', '# technologies', 'Firm type', 'Past choice 1', 'Past choice 2', 'Wind share'  ))
D[,`Past choice 1` := NULL]
setnames(D, "Past choice 2", "Past choice")

tree2 <- rpart(invest  ~ ., data = D, method = "class", cp = 10^(-9), minsplit = 10, model = TRUE)


#### Gas #### 
dreg_sub <- dreg[invest_TF == T & total_mw>0 & year %in% 2014:2023,]
techs <- c('Gas')
# make tech-specific
setnames(dreg_sub, techs, 'techshare')
setnames(dreg_sub, paste0(techs,'_cap'), 'tech_capacity')
dreg_sub$tech_cap <- (dreg_sub$tech_capacity) # not use log

dreg_sub$invest_tech <- ifelse(dreg_sub[, .SD, .SDcols = paste0(techs, '_investsize')] > 0, 1, 0)

## previous 5-year investments
tech_other <- tech_rank_short[ !(tech_rank_short %in% techs) ]
check_tech <- apply( dreg_sub[, .SD, .SDcols = paste0('nr.5yr_invest_',techs)], 1, function(x) any(x>0) )
check_other <- apply( dreg_sub[, .SD, .SDcols = paste0('nr.5yr_invest_',tech_other)], 1, function(x) any(x>0) )

dreg_sub$invest_type.5yrs <- ifelse( (!check_tech) & (!check_other), 'none', 
                                     ifelse(  check_tech & check_other, 'gas & others', 
                                              ifelse(  check_tech & (!check_other), 'only gas', 'only others')))

dreg_sub$invest_type.5yrs <- factor(dreg_sub$invest_type.5yrs, levels = c("none", 
                                                                          "gas & others",
                                                                          "only gas",
                                                                          "only others"))

dreg_sub$invest_type.simple <- ifelse( (!check_tech) & (!check_other), 'none, gas & others', 
                                       ifelse(  check_tech & check_other, 'none, gas & others', 
                                                ifelse(  check_tech & (!check_other), 'none, gas & others', 'only others')))

dreg_sub$invest_type.simple <- factor(dreg_sub$invest_type.simple, levels = c("none, gas & others", "only others"))

D <- dreg_sub[, c('invest_tech','year_cat', 'total_mw', 'tech_cap', 'nr.tech', 'type', 'invest_type.5yrs', 'invest_type.simple', 'techshare'  )]
D[, invest_tech := factor(ifelse(invest_tech==1,'Yes','No'))]
D$total_mw <- D$total_mw/1000
D$tech_cap <- D$tech_cap/1000

setnames(D, c('invest_tech','year_cat', 'total_mw', 'tech_cap', 'nr.tech', 'type', 'invest_type.5yrs', 'invest_type.simple', 'techshare'  ),
         c('invest', 'Year', 'Total capacity (GW)', 'Gas capacity (GW)', '# technologies', 'Firm type', 'Past choice 1', 'Past choice 2', 'Gas share'  ))
D[,`Past choice 1` := NULL]
setnames(D, "Past choice 2", "Past choice")

tree3 <- rpart(invest  ~ ., data = D, method = "class", cp = 10^(-9), minsplit = 10, model = TRUE)

#### Coal #### 
dreg_sub <- dreg[invest_TF == T & total_mw>0 & year %in% 2014:2023,]
techs <- c('Coal')
# make tech-specific
setnames(dreg_sub, techs, 'techshare')
setnames(dreg_sub, paste0(techs,'_cap'), 'tech_capacity')
dreg_sub$tech_cap <- (dreg_sub$tech_capacity)/1000 # IN GW

dreg_sub$invest_tech <- ifelse(dreg_sub[, .SD, .SDcols = paste0(techs, '_investsize')] > 0, 1, 0)

## previous 5-year investments
tech_other <- tech_rank_short[ !(tech_rank_short %in% techs) ]
check_tech <- apply( dreg_sub[, .SD, .SDcols = paste0('nr.5yr_invest_',techs)], 1, function(x) any(x>0) )
check_other <- apply( dreg_sub[, .SD, .SDcols = paste0('nr.5yr_invest_',tech_other)], 1, function(x) any(x>0) )

dreg_sub$invest_type.5yrs <- ifelse( (!check_tech) & (!check_other), 'none', 
                                     ifelse(  check_tech & check_other, 'coal & others', 
                                              ifelse(  check_tech & (!check_other), 'only coal', 'only others')))

dreg_sub$invest_type.5yrs <- factor(dreg_sub$invest_type.5yrs, levels = c("none", 
                                                                          "coal & others",
                                                                          "only coal",
                                                                          "only others"))

dreg_sub$invest_type.simple <- ifelse( (!check_tech) & (!check_other), 'none or only others', 
                                       ifelse(  check_tech & check_other, 'coal & others', 
                                                ifelse(  check_tech & (!check_other), 'coal & others', 'none or only others')))

dreg_sub$invest_type.simple <- factor(dreg_sub$invest_type.simple, levels = c("coal & others", "none or only others"))

D <- dreg_sub[, c('invest_tech','year_cat', 'total_mw', 'tech_cap', 'nr.tech', 'type', 'invest_type.5yrs', 'invest_type.simple', 'techshare'  )]
D[, invest_tech := factor(ifelse(invest_tech==1,'Yes','No'))]

setnames(D, c('invest_tech','year_cat', 'total_mw', 'tech_cap', 'nr.tech', 'type', 'invest_type.5yrs', 'invest_type.simple',  'techshare'  ),
         c('invest', 'Year', 'Total capacity (MW)', 'Coal capacity (GW)', '# technologies', 'Firm type', 'Past choice 1', 'Past choice 2', 'Coal share'  ))
D[,`Past choice 1` := NULL]
setnames(D, "Past choice 2", "Past choice")

tree4 <- rpart(invest  ~ ., data = D, method = "class", cp = 10^(-9), minsplit = 10, model = TRUE)



#######################
#### VISUALIZATION ####
#######################


pdf('R-code/investment_hurdle_model/fig/tech_choice_tree.pdf', width = 15)
par(mfrow=c(2,2))
cptab1 <- tree1$cptable
minrow1 <- which.min(cptab1[, "xerror"])
se_threshold1 <- cptab1[minrow1, "xerror"] + 1*cptab1[minrow1, "xstd"]
se_cp <- max(cptab1[cptab1[, "xerror"] <= se_threshold1, "CP"])
tree1_pruned <- prune(tree1, cp = se_cp)
tree1_snipped <- snip.rpart(tree1_pruned, toss = c(10,11,12, 13) )
#tree1_snipped <- tree1_pruned

rpart.plot(tree1_snipped,
           tweak = 1.2,
           type = 3,
           extra = 104, #
           under = T,
           clip.right.labs = T, branch = 0.3)
text(x = 0.83, y = 1.02, labels = "A. Choose solar PV", xpd = NA, cex = 1.5, font = 2)


cptab2 <- tree2$cptable
minrow2 <- which.min(cptab2[, "xerror"])
se_threshold2 <- cptab2[minrow2, "xerror"] + 1*cptab2[minrow2, "xstd"]
se_cp <- max(cptab2[cptab2[, "xerror"] <= se_threshold2, "CP"])
tree2_pruned <- prune(tree2, cp = se_cp)

tree2_snipped <- snip.rpart(tree2_pruned, toss = c(7,12, 26, 27) )
#tree2_snipped <- tree2_pruned


rpart.plot(tree2_snipped,
           tweak = 1.2,
           type = 3,
           extra = 104, #
           under = T,
           clip.right.labs = T, branch = 0.3)
text(x = 0.79, y = 1.02, labels = "B. Choose onshore wind", xpd = NA, cex = 1.5, font = 2)


cptab3 <- tree3$cptable
minrow3 <- which.min(cptab3[, "xerror"])
se_threshold3 <- cptab3[minrow3, "xerror"] + 1*cptab3[minrow3, "xstd"]
se_cp <- max(cptab3[cptab3[, "xerror"] <= se_threshold3, "CP"])
tree3_pruned <- prune(tree3, cp = se_cp)

tree3_snipped <- snip.rpart(tree3_pruned, toss = c(7) )
#tree3_snipped <- tree3_pruned

rpart.plot(tree3_snipped,
           tweak = 1.2,
           type = 3,
           extra = 104, #
           under = T,
           clip.right.labs = T, branch = 0.3)
text(x = 0.8, y = 1.05, labels = "C. Choose gas", xpd = NA, cex = 1.5, font = 2)

cptab4 <- tree4$cptable
minrow4 <- which.min(cptab4[, "xerror"])
se_threshold4 <- cptab4[minrow4, "xerror"] + 1*cptab4[minrow4, "xstd"]
se_cp <- max(cptab4[cptab4[, "xerror"] <= se_threshold4, "CP"])
tree4_pruned <- prune(tree4, cp = se_cp)

tree4_snipped <- snip.rpart(tree4_pruned, toss = c(7, 12, 13) )
#tree4_snipped <- tree4_pruned

rpart.plot(tree4_snipped,
           type = 3,
           tweak = 1.2,
           extra = 104, #
           under = T,
           clip.right.labs = T, 
           branch = 0.3
)
text(x = 0.8, y = 1.05, labels = "D. Choose coal", xpd = NA, cex = 1.5, font = 2)
dev.off()



pdf('R-code/investment_hurdle_model/fig/tech_choice_tree_cp.pdf', width = 9)
par(mfrow=c(2,2))
plotcp(tree1)
i1 <- as.integer( names(cptab1[cptab1[, "xerror"] <= se_threshold1, "CP"]))[1] 
y <- cptab1[i1, "xerror"]
se <- cptab1[i1, "xstd"]
points(i1, y, pch = 19, col = "firebrick", cex = 1.5)
arrows(i1, y - se, i1, y + se, angle = 90, code = 0, length = 0.05, col = "firebrick", lwd = 2)
text(x = 23, y = 1.1, labels = "A. Choose solar PV", xpd = NA, cex = 1.2, font = 2)

plotcp(tree2)
i2 <- as.integer( names(cptab2[cptab2[, "xerror"] <= se_threshold2, "CP"]))[1] 
y <- cptab2[i2, "xerror"]
se <- cptab2[i2, "xstd"]
points(i2, y, pch = 19, col = "firebrick", cex = 1.5)
arrows(i2, y - se, i2, y + se, angle = 90, code = 0, length = 0.05, col = "firebrick", lwd = 2)
text(x = 22, y = 1.1, labels = "B. Choose onshore wind", xpd = NA, cex = 1.2, font = 2)

plotcp(tree3)    # size of axis labels ("CP", "xerror"))
i3 <- as.integer( names(cptab3[cptab3[, "xerror"] <= se_threshold3, "CP"]))[1] 
y <- cptab3[i3, "xerror"]
se <- cptab3[i3, "xstd"]
points(i3, y, pch = 19, col = "firebrick", cex = 1.5)
arrows(i3, y - se, i3, y + se, angle = 90, code = 0, length = 0.05, col = "firebrick", lwd = 2)
text(x = 25, y = 1.1, labels = "C. Choose gas", xpd = NA, cex = 1.2, font = 2)

plotcp(tree4)
i4 <- as.integer( names(cptab4[cptab4[, "xerror"] <= se_threshold4, "CP"]))[1] 
y <- cptab4[i4, "xerror"]
se <- cptab4[i4, "xstd"]
points(i4, y, pch = 19, col = "firebrick", cex = 1.5)
arrows(i4, y - se, i4, y + se, angle = 90, code = 0, length = 0.05, col = "firebrick", lwd = 2)
text(x = 17, y = 1.12, labels = "D. Choose coal", xpd = NA, cex = 1.2, font = 2)
dev.off()

