rm(list=ls())
source('R-code/source_code/libraries.R')

dx <- fread('data_processed/R-data-output/firm_tech_year.csv')
dx <- dx[total_mw > 0 & year == 2024,]
total_mw <- sort(dx$total_mw)


### fit log normal mixture
log_capacities <- log(total_mw)
k <- 2
mixture_fit <- normalmixEM(log_capacities, k = k)


cat("Mixture of", k, "lognormal distributions:\n")
for(i in 1:k) {
  cat("Component", i, ":\n")
  cat("  Proportion:", round(mixture_fit$lambda[i], 4), "\n")
  cat("  Meanlog:", round(mixture_fit$mu[i], 4), "\n")
  cat("  SDlog:", round(mixture_fit$sigma[i], 4), "\n")
}


log_capacities_df <- data.table(log_capacity = log_capacities)

# Create a sequence for density plotting
component_densities <- data.table( x= seq(min(log_capacities), max(log_capacities), length.out = length(log_capacities)),
                                   capacity = log_capacities )

component_densities[, comp1 := ( mixture_fit$lambda[1] * dnorm(x, mean = mixture_fit$mu[1], sd = mixture_fit$sigma[1]) ) ]
component_densities[, comp2 := ( mixture_fit$lambda[2] * dnorm(x, mean = mixture_fit$mu[2], sd = mixture_fit$sigma[2]) ) ]
component_densities[, mixed := comp1 + comp2 ]


p1 <- ggplot(component_densities) + 
  geom_histogram(aes(x = capacity, y = ..density..), fill = "antiquewhite", color = "grey70", alpha = 0.7) +
  geom_line(aes(x = x, y = comp1, color = "Comp. 1"), linewidth = 1.2) +
  geom_line(aes(x = x, y = comp2, color = "Comp. 2"), linewidth = 1.2) +
  geom_line(aes(x = x, y = mixed, color = "Mixture"), linewidth = 1.2) +
  scale_color_manual(values = c("Comp. 1" = "dodgerblue", 
                                "Comp. 2" = "firebrick", 
                                "Mixture" = "black")) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0,1),
    legend.justification = c(0,1),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_text(size=22),
    axis.text = element_text(size = 22),
    legend.text = element_text(size = 22),
    legend.key.width = unit(1.2, 'cm'),
  ) +
  xlab('Natural Log of firm capacity (MW)') + ylab('Density')



#### SIMULATE FROM MIXTURE MODEL
# Function to generate random samples from mixture model
rmix_lnorm <- function(n, lambda, meanlog, sdlog) {
  
  # draw TRUE & FALSE based on lambda (component share) (avoids rounding)
  z <- runif(n) <= lambda[1]  # TRUE ~ comp1 times, FALSE ~ comp2 times
  
  # Generate from respective components
  result <- numeric(n)
  result[z] <- rlnorm(sum(z), meanlog = meanlog[1], sdlog = sdlog[1])
  result[!z] <- rlnorm(sum(!z), meanlog = meanlog[2], sdlog = sdlog[2])
  
  return(result)
}

# ### Simulate data
# set.seed(1000)
# n_sim <- length(total_mw)
# simulated_data <- rmix_lnorm(n_sim, 
#                              lambda = mixture_fit$lambda,
#                              meanlog = mixture_fit$mu,
#                              sdlog = mixture_fit$sigma)

#### make CCDF plot through comparing simulated data with actual data
ccdf_with_confidence <- function(
    actual_data,          
    n_simulations = 100,  
    fit,
    conf_level = 0.95,    # Confidence level (e.g., 0.95 for 95% confidence)
    seed = 123             
) {
  # Set random seed
  set.seed(seed)
  
  # Create a common x-grid for evaluating all CCDFs
  x_grid <- exp(seq(log(min(actual_data)*0.5), log(max(actual_data)*2), length.out = 1000))
  
  # Calculate actual CCDF on this grid
  ecdf_actual <- ecdf(actual_data)
  ccdf_actual <- 1 - ecdf_actual(x_grid)
  
  # Initialize matrix to store all simulation CCDFs
  sim_ccdfs <- matrix(0, nrow = n_simulations, ncol = length(x_grid))
  
  # Generate simulations and calculate CCDFs
  for (i in 1:n_simulations) {
    sim_data <-rmix_lnorm(length(actual_data), 
                          lambda = mixture_fit$lambda,
                          meanlog = mixture_fit$mu,
                          sdlog = mixture_fit$sigma)
    
    ecdf_sim <- ecdf(sim_data)
    sim_ccdfs[i,] <- 1 - ecdf_sim(x_grid)
  }
  
  # Calculate quantiles for each x position
  lower_quantile <- (1 - conf_level) / 2
  upper_quantile <- 1 - lower_quantile
  
  lower_bound <- apply(sim_ccdfs, 2, quantile, probs = lower_quantile)
  upper_bound <- apply(sim_ccdfs, 2, quantile, probs = upper_quantile)
  median_line <- apply(sim_ccdfs, 2, median)
  
  # Create data frames for plotting
  actual_df <- data.frame(x = x_grid, ccdf = ccdf_actual, type = "Empirical")
  median_df <- data.frame(x = x_grid, ccdf = median_line, type = "Theoretical mixture")
  conf_df <- data.frame(x = x_grid, lower = lower_bound, upper = upper_bound)
  
  
  return(list(
    actual_df = actual_df,
    median_df = median_df,
    conf_df = conf_df
  ))
}

conf_data <- ccdf_with_confidence(  
  seed = 1,
  actual_data = total_mw,
  n_simulations = 1000, fit = mixture_fit,
  conf_level = 1
)



p2 <- ggplot() +
  # Add actual data line
  geom_line(data = conf_data$actual_df,
            aes(x = x, y = ccdf, color = type, linetype = type),
            linewidth = 1.2) +
  # Add confidence region
  geom_ribbon(data = conf_data$conf_df, 
              aes(x = x, ymin = lower, ymax = upper),
              fill = "red", alpha = 0.2) +
  # Add median line
  geom_line(data = conf_data$median_df,
            aes(x = x, y = ccdf, color = type, linetype = type),
            linewidth = 1.2) +
  # Scale axes
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(base = 10, sides = 'bl') +
  scale_color_manual(values = c('dodgerblue', 'firebrick')) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  # Theming
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0, 0.03),
    legend.justification = c(0, 0),
    legend.background = element_blank(),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22),
    legend.text = element_text(size = 22),
    legend.key.width = unit(1.2, 'cm')
  ) +
  xlab("Firm capacity in MW") + 
  ylab('CCDF: Prob(Firm capacity > x)')







# Create QQ plots with multiple simulation lines
multiple_qqplots <- function(
    actual_data,                  # Your actual data vector
    n_simulations = 10,           # Number of simulations to show
    fit,
    seed = 123,                   # Random seed for reproducibility
    line_colors = NULL            # Optional custom colors
) {
  # Set seed for reproducibility
  set.seed(seed)
  
  # If colors not provided, create a color palette
  if (is.null(line_colors)) {
    # Create a palette with actual data in red and simulations in different blues
    line_colors <- c("red", colorRampPalette(c("#0570b0", "#74a9cf", "#bdc9e1"))(n_simulations))
  }
  
  # Sort actual data
  sorted_actual <- sort(actual_data)
  n <- length(sorted_actual)
  
  # Quantile probabilities
  probs <- (1:n - 0.5) / n
  
  # Create data frame for actual data quantiles
  actual_quantiles <- data.frame(
    p = probs,
    q = sorted_actual,
    type = "Actual Data"
  )
  
  # Generate simulations and store their quantiles
  all_sim_quantiles <- list()
  
  for (i in 1:n_simulations) {
    # Generate simulated data using the provided rmix_lnorm function
    sim_data <- rmix_lnorm(n, lambda = fit$lambda, meanlog = fit$mu, sdlog = fit$sigma)
    
    # Sort simulated data
    sorted_sim <- sort(sim_data)
    
    # Store in data frame
    sim_quantiles <- data.frame(
      p = probs,
      q = sorted_sim,
      type = paste("Simulation", i)
    )
    
    all_sim_quantiles[[i]] <- sim_quantiles
  }
  
  # Combine all quantiles into one data frame
  all_quantiles <- rbind(actual_quantiles, do.call(rbind, all_sim_quantiles))
  
  # Create a wide format data frame for QQ plots
  qq_data <- pivot_wider(all_quantiles, 
                         id_cols = p, 
                         names_from = type, 
                         values_from = q)
  
  # Create a long format for plotting multiple lines against actual data
  qq_long <- data.frame()
  
  for (i in 1:n_simulations) {
    sim_col <- paste("Simulation", i)
    
    temp_data <- data.frame(
      theoretical = qq_data[[sim_col]],
      empirical = qq_data$`Actual Data`,
      simulation = sim_col
    )
    
    qq_long <- rbind(qq_long, temp_data)
  }
  
  
  return(qq_long)
}


nsim <- 15
qq_long <- multiple_qqplots(actual_data = total_mw, n_simulations = nsim, fit = mixture_fit, seed = 123)

# Create the QQ plot
p3 <- ggplot(qq_long, aes(x = theoretical, y = empirical, color = simulation)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 1) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = 'bl') +
  scale_color_manual(values = colorRampPalette(c('red','yellow','blue'))(nsim)) +
  xlab("Simulated quantiles (MW)") + 
  ylab("Sample quantiles (MW)") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 22),
    axis.title = element_text(size = 22)
  ) 


pdf('R-code/create_figs_SI/fig/fig_mixture_model.pdf', width = 18, height = 6)
grid.arrange(p1,p2,p3, ncol=3)
grid.text("A", x = 0.01, y = .97, gp = gpar(fontsize = 24, fontface = "bold"))
grid.text("B", x = 0.35, y = .97, gp = gpar(fontsize = 24, fontface = "bold"))
grid.text("C", x = 0.69, y = .97, gp = gpar(fontsize = 24, fontface = "bold"))
dev.off()

