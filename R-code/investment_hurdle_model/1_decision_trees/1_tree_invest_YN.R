rm(list=ls())
source('R-code/source_code/libraries.R')

dreg <- fread('data_processed/R-data-output/regtable.csv')

# create factors
dreg$type <- factor(dreg$type, levels = c('Private firm', 'Public firm', 'Other'))
#dreg$nr.tech_class <- factor(dreg$nr.tech_class, levels = c('0','1','[2,4)','[4,6)', '>=6'))
dreg$focus_class <- factor(dreg$focus_class, levels = c('Mixed', 'Solar PV', 'Onshore wind', 'Hydro', 'Gas', 'Coal', 'Mixed-FF', 'Mixed-RE', 'Other'))

dreg$state_own_class <- ifelse(dreg$state_own > 50 | is.na(dreg$state_own), 'state', 'no state')
dreg$state_own_class <- factor(dreg$state_own_class, levels = c('no state', 'state'))

dreg$year <- factor(dreg$year)
dreg$nr.5yr_invest <- factor(dreg$nr.5yr_invest)
dreg$log_total_mw <- (dreg$total_mw) # we don't use logs in the tree

dreg$geo %>% table

DREGL <- list(
  Full = dreg,
  decade = dreg[year %in% 2014:2023, ],
  quinquennium = dreg[year %in% 2019:2023, ]
)



# choose which dataset
i<-2 # most recent 10-year
dat <- copy(DREGL[[i]])
dat[, invest := factor(ifelse(invest_TF==T, 'invest', 'not invest'))]

# all predictors
dat <- dat[, c("invest", "year", "log_total_mw", "nr.tech", 'nr.5yr_invest', "type", "focus_class")]

setnames(dat, c("invest", "year", "log_total_mw", "nr.tech", 'nr.5yr_invest', "type", "focus_class"),
         c("invest", "year", "Capacity (MW)", "# tech", '# invest (past 5 yrs)', "type", "focus_class"))



# 1. Fit the full tree with a small cp to allow full growth
set.seed(123)
tree1_rpart <- rpart(invest ~ ., data = dat, method = "class", cp = 0.00001)




cptab <- tree1_rpart$cptable
minrow <- which.min(cptab[, "xerror"])
se_threshold <- cptab[minrow, "xerror"] + cptab[minrow, "xstd"]
se_cp <- max(cptab[cptab[, "xerror"] <= se_threshold, "CP"])
tree1_pruned <- prune(tree1_rpart, cp = se_cp)

tree1_snipped <- snip.rpart(tree1_pruned, toss = c(10,11) )
#tree1_snipped <- tree1_pruned
pdf('R-code/investment_hurdle_model/fig/tree_invest1.pdf', width= 5, height =8)
rpart.plot(tree1_snipped,
           type = 3, 
           extra = 104, #  
           under = T,
           clip.right.labs = T, branch = 0.3)
text(x = 0.15, y = 1.1, labels = "A.", xpd = NA, cex = 1.2, font = 2)
dev.off()




# 2. Plot the cross-validated error vs. cp
pdf('R-code/investment_hurdle_model/fig/tree_invest1_cp.pdf', width= 6, height = 5)
par(mfrow=c(1,1))
cpt <- tree1_rpart$cptable
plotcp(tree1_rpart)         # Visual: CV error vs. tree size
i1 <- as.integer( names(cpt[cpt[, "xerror"] <= se_threshold, "CP"]))[1] 
y <- cpt[i1, "xerror"]
se <- cpt[i1, "xstd"]
points(i1, y, pch = 19, col = "firebrick", cex = 1.5)
arrows(i1, y - se, i1, y + se, angle = 90, code = 0, length = 0.05, col = "firebrick", lwd = 2)
dev.off()
