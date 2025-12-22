rm(list=ls())
source('R-code/source_code/libraries.R')

dreg <- fread('data_processed/R-data-output/regtable.csv')

# create factors
dreg$type <- factor(dreg$type, levels = c('Private firm', 'Public firm', 'Other'))
dreg$nr.tech_class <- factor(dreg$nr.tech_class, levels = c('0','1','[2,4)','[4,6)', '>=6'))
dreg$focus_class <- factor(dreg$focus_class, levels = c('Mixed', 'Solar PV', 'Onshore wind', 'Hydro', 'Gas', 'Coal', 'Mixed-FF', 'Mixed-RE', 'Other'))

dreg$state_own_class <- ifelse(dreg$state_own > 50 | is.na(dreg$state_own), 'state', 'no state')
dreg$state_own_class <- factor(dreg$state_own_class, levels = c('no state', 'state'))

dreg$year <- factor(dreg$year)
dreg$nr.5yr_invest <- factor(dreg$nr.5yr_invest)
dreg$log_total_mw <- log10(dreg$total_mw)

DREGL <- list(
  Full = dreg,
  decade = dreg[year %in% 2014:2023, ],      # note that no investment decisions in 2024
  quinquennium = dreg[year %in% 2019:2023, ]
)


res <- res_mp <- list()
nrinvests <- c()
for (i in 1:length(DREGL)) {
  
  if( DREGL[[i]]$year %>% unique() %>% length() > 1){
    res[[i]] <- glm(invest_TF ~ 
                      year +
                      log_total_mw +
                      nr.tech_class +
                      type + 
                      nr.5yr_invest +
                      focus_class, 
                    family = 'binomial',
                    data = DREGL[[i]]) 
  } else{
    res[[i]] <- glm(invest_TF ~ 
                      log_total_mw +
                      nr.tech_class +
                      type + 
                      nr.5yr_invest +
                      focus_class,
                    family = 'binomial',
                    data = DREGL[[i]]) 
  }
  
  res_mp[[i]] <- logitmfx(res[[i]], data = DREGL[[i]], atmean = F, robust = TRUE)
  
  nrinvests[i] <- sum(DREGL[[i]]$invest_TF==T)
  print(i)
}


## print results
COVS <- 
  c("Log total capacity",
    "Nr of tech owned: [2,4)",
    "Nr of tech owned: [4,6)",
    "Nr of tech owned: $/ge$6",
    "Public firm",
    "Other entity",
    "Invested 1x in past 5 yrs",
    "Invested 2x in past 5 yrs",
    "Invested 3x in past 5 yrs",
    "Invested 4x in past 5 yrs",
    "Invested 5x in past 5 yrs",
    "Focus: Solar PV",
    "Focus: Wind Onshore",
    "Focus: Hydro",
    "Focus: Gas",
    "Focus: Coal",
    "Focus: Mixed-FF",
    "Focus: Mixed-RE",
    "Focus: Any other tech"
  )


pseudorsq <- 1 - sapply( res_mp, function(x) round(x$fit$deviance,1)) / sapply( res_mp, function(x) round(x$fit$null.deviance,1))

stargazer( lapply(res_mp, function(x) x$fit),
           star.cutoffs = c(5*10^(-2), 10^(-2), 10^(-16)),
           coef = lapply(res_mp, function(x) c(x$mfxest[,1]) ),
           se = lapply(res_mp, function(x) c(x$mfxest[,2]) ),
           p = lapply(res_mp, function(x) c(x$mfxest[,4]) ),
           type = 'latex',
           single.row = F,
           no.space = T,
           omit = c("year", 'Constant'),
           covariate.labels=COVS,
           omit.stat =c("LL", 'f', 'rsq', 'ser', 'aic'),
           add.lines = list(
             c("Pseudo R2", round(pseudorsq,3)),
             c("Nr of investments", nrinvests)
           ),
           label = c("t:reg_invest"
           )
)




##################################
##### MARGINAL EFFECT PLOT #######
##################################
marginal_effects <- data.table( term = rownames(res_mp[[2]]$mfxest), 
                                estimate = res_mp[[2]]$mfxest[,'dF/dx'],
                                std.error = res_mp[[2]]$mfxest[, 'Std. Err.'])

# Create a 95% confidence interval
marginal_effects[, lower := estimate - 1.96 * std.error]
marginal_effects[, upper := estimate + 1.96 * std.error]

# Clean up variable names for better display
marginal_effects <- marginal_effects %>%
  mutate(
    clean_term = case_when(
      grepl("^year", term) ~ paste0("Year ", substr(term, 5, 8)),
      term == "log_total_mw" ~ "Log capacity (MW)",
      grepl("^nr.tech_class", term) ~ gsub("nr.tech_class", "# technologies: ", term),
      grepl("^type", term) ~ gsub("type", "Ownership: ", term),
      grepl("^nr.5yr_invest1", term) ~ paste0("Invested ", 1, "x in past 5 yrs"),
      grepl("^nr.5yr_invest2", term) ~ paste0("Invested ", 2, "x in past 5 yrs"),
      grepl("^nr.5yr_invest3", term) ~ paste0("Invested ", 3, "x in past 5 yrs"),
      grepl("^nr.5yr_invest4", term) ~ paste0("Invested ", 4, "x in past 5 yrs"),
      grepl("^nr.5yr_invest5", term) ~ paste0("Invested ", 5, "x in past 5 yrs"),
      grepl("^focus_class", term) ~ gsub("focus_class", "Focus: ", term),
      TRUE ~ term
    )
  )

# Add category grouping for coloring
marginal_effects <- marginal_effects %>%
  mutate(
    category = case_when(
      grepl("^Year", clean_term) ~ "Year",
      grepl("^Log", clean_term) ~ "Size",
      grepl("^#", clean_term) ~ "# technologies",
      grepl("^Ownership", clean_term) ~ "Ownership type",
      grepl("^Invested", clean_term) ~ "Investment history",
      grepl("^Focus", clean_term) ~ "Technology focus",
      TRUE ~ "Other"
    )
  )

marginal_effects_for_plot <- marginal_effects %>%
  filter(!grepl("^Year", clean_term))

# Order by category and effect size within categories
marginal_effects_for_plot <- marginal_effects_for_plot %>%
  mutate(abs_estimate = abs(estimate)) %>%
  arrange(category, desc(abs_estimate)) 

# Create a factor to order the variables in a meaningful way
category_order <- c("Size", "Investment history", "# technologies", "Technology focus", "Ownership type")
marginal_effects_for_plot$category <- factor(marginal_effects_for_plot$category, levels = category_order)

# Sort by category and then by absolute effect size within category
marginal_effects_for_plot <- marginal_effects_for_plot %>%
  arrange(category, desc(abs_estimate)) %>%
  mutate(clean_term = factor(clean_term, levels = rev(clean_term)))

# Define baseline text to add to the plot
baseline_text <- c(
  "Baselines for categorical variables:\n# technologies: Owns 1 technology\nOwnership type: Private firm\nInvestment history: No investments in past 5 years\nTechnology focus: Mixed (Multi-tech firm without dominant focus)"
)


pdf('R-code/investment_hurdle_model/fig/mfx_fig.pdf', height = 7, width = 6)
ggplot(marginal_effects_for_plot, aes(x = estimate, y = clean_term, color = category)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = lower, xend = upper, yend = clean_term), 
               linewidth = 0.8) +
  geom_point(size = 3) +
  scale_color_manual(values = c('black', 'dodgerblue', 'firebrick', 'darkgreen', 'burlywood2')) +
  labs(
    x = "Marginal effects on investment probability",
    y = NULL,
    title = 'B.',
    color = "Variable Category"
  ) +
  theme(
    legend.position = c(0.99,0.02),
    legend.justification = c(1,0.05),
    legend.background = element_rect(color = 'black', size = 0.1),
    legend.key.width = unit(1,'cm'),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_markdown(face = "bold", hjust = 0, size = 16,
                                  margin = margin(l = -150, unit = "pt")),
    legend.title = element_blank(),
    axis.text.y = element_text(hjust = 0, size = 14),
    axis.title.x = element_text(size=14),
    axis.text.x = element_text(size = 14),
    legend.text = element_text(size = 14)
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), position = 'top') +
  scale_y_discrete(
    labels = function(x) {
      ifelse(
        x == "Log capacity (MW)",
        expression(Log[10]~"capacity (MW)"),
        x
      )
    }
  )


dev.off()

