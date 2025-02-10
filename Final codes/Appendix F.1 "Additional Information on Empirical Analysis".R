###################################################
#Get GBTC Data
##################################################

install.packages("vrtest")
libraries = c("vrtest") # needed this lib for skewness and kurtosis

lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x, repos = "http://cran.uni-muenster.de/")
})

lapply(c(libraries), require, character.only = TRUE)

# Loaded required libraries
library(quantmod)
library(dplyr)
library(rugarch)

# Downloaded GBTC data from Yahoo Finance
GBTC <- getSymbols("GBTC", auto.assign = FALSE, from = "2020-04-11", to = "2024-09-13", src = "yahoo")
View(GBTC)

#####################################################################################
#Convert and clean the data
##########################################################################################
# Converted to a data frame for easier manipulation
GBTC_df <- data.frame(Date = index(GBTC), coredata(GBTC))

# Cleaned the GBTC data: Ensured Price is numeric and filter dates from 2020-04-11 onwards
GBTC_cleaned <- GBTC_df %>%
  mutate(
    Price = as.numeric(GBTC.Adjusted), # Used the Adjusted Close as the price
    Date = as.Date(Date)
  ) %>%
  select(Date, Price) %>% # Kept only relevant columns
  filter(Date >= as.Date("2020-04-11"))

# Computed log returns for GBTC
GBTC_returns <- GBTC_cleaned %>%
  mutate(Return = c(NA, diff(log(Price)))) %>% # Log returns calculation
  filter(!is.na(Return)) # Removed NA values

# View cleaned data with returns:)
View(GBTC_returns)


##########################################################################
#Chapter 2 Analysis
##########################################################################
# Loaded necessary libraries
library(dplyr)
library(moments)
library(kableExtra)
library(webshot2)
library(tidyverse)

# Installed and loaded `tseries` for Jarque-Bera test
install.packages("tseries")
library(tseries)

# Computed descriptive statistics for GBTC
compute_descriptive_stats <- function(returns) {
  stats <- c(
    mean = mean(returns),
    median = median(returns),
    sd = sd(returns),
    max = max(returns),
    min = min(returns),
    skewness = skewness(returns),
    kurtosis = kurtosis(returns),
    jarque_bera = jarque.bera.test(returns)$statistic
  )
  return(stats)
}

# Applied the function to GBTC returns
gbtc_stats <- compute_descriptive_stats(GBTC_returns$Return)

# Formatted numbers to avoid scientific notation
formatted_stats <- formatC(round(gbtc_stats, 4), format = "f", digits = 4)

# Created a summary table
summary_table_GBTC <- data.frame(
  Metric = c("Mean", "Median", "SD", "Min", "Max", "Skewness", "Kurtosis", "Jarque-Bera"),
  Value = formatted_stats
)

# Added a star (*) to Jarque-Bera to indicate significance
summary_table_GBTC$Value[8] <- paste0(formatted_stats[8], "*")

#########In the thesis, overleaf created a table, so this steps actually not really necessary
# Created and saved the vertical table
summary_table_GBTC %>%
  kable("html", align = "c", col.names = c("Metric", "Value")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  save_kable("gbtc_summary_stats.html")

# Created and saved the horizontal table
summary_table_GBTC %>%
  pivot_wider(names_from = Metric, values_from = Value) %>%
  kable("html", align = "c") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(0, extra_css = "border-top: 2px solid black; border-bottom: 2px solid black;") %>%
  row_spec(1, extra_css = "border-bottom: 2px solid black;") %>%
  save_kable("summary_stats_horizontal.html")

# Saved both tables as images
webshot::install_phantomjs(force = TRUE)
webshot("gbtc_summary_stats.html", "gbtc_summary_stats.png")
webshot("summary_stats_horizontal.html", "summary_stats_horizontal.png")


#Jumps

# Function to calculate RV, BPV, Jump Component, and Jump Percentage for log returns
calculate_rv_bpv_jump <- function(log_returns) {
  n <- length(log_returns)
  
  # Calculated Realized Volatility (RV)
  RV <- sum(log_returns^2, na.rm = TRUE)
  
  # Calculated the BiPower Variation (BPV)
  mu1 <- sqrt(2 / pi)
  
  # BPV using adjacent absolute returns
  BPV <- mu1^(-2) * sum(abs(log_returns[2:n]) * abs(log_returns[1:(n-1)]), na.rm = TRUE)
  
  # Calculated Jump Component (J)
  J <- max(RV - BPV, 0)
  
  # Calculated Jump Percentage
  Jump_Percentage <- round((J / RV) * 100, 2)
  
  return(list(RV = RV, BPV = BPV, Jump = J, Jump_Percentage = Jump_Percentage))
}

# Applied the function to GBTC log returns
gbtc_results <- calculate_rv_bpv_jump(GBTC_returns$Return)

# Displayed the results
gbtc_results


#######################
#Chapter 3 Analysis
#####################

# Step 2: Defined GARCH, EGARCH, and GJR-GARCH Specifications
garch_specs <- list(
  "GARCH(1,1)" = ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0)),
    distribution.model = "norm"
  ),
  "EGARCH(1,1)" = ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0)),
    distribution.model = "norm"
  ),
  "GJR-GARCH(1,1)" = ugarchspec(
    variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0)),
    distribution.model = "norm"
  )
)

# Step 3: Fitted GARCH Models to GBTC Data
fit_models <- function(returns, specs) {
  lapply(names(specs), function(model_name) {
    tryCatch({
      fit <- ugarchfit(spec = specs[[model_name]], data = returns)
      list(model = model_name, fit = fit)
    }, error = function(e) {
      message(paste("Error fitting", model_name, ":", e$message))
      return(NULL)
    })
  })
}

# Fitted models for GBTC
garch_results_gbtc <- fit_models(GBTC_returns$Return, garch_specs)

# Step 4: Displayed Results for GBTC
cat("\n\n=== Results for GBTC ===\n")
lapply(garch_results_gbtc, function(result) {
  if (!is.null(result)) {
    cat("\n", result$model, "\n")
    print(result$fit)
  }
})

# Step 5: ARJI-GARCH

# ARJI-GARCH Log-Likelihood Function
fit_arji <- function(params, returns) {
  omega <- params["omega"]
  alpha <- params["alpha"]
  beta <- params["beta"]
  lambda0 <- params["lambda0"]
  rho <- params["rho"]
  gamma <- params["gamma"]
  
  h <- numeric(length(returns))
  lambda <- numeric(length(returns))
  u <- returns
  
  h[1] <- var(returns)  # Initialized conditional variance
  lambda[1] <- lambda0  # Initialized jump intensity
  
  for (t in 2:length(returns)) {
    h[t] <- pmax(omega + alpha * u[t - 1]^2 + beta * h[t - 1], 1e-6)
    lambda[t] <- pmax(lambda0 + rho * lambda[t - 1] + gamma * u[t - 1]^2, 1e-6)
  }
  
  # Computed log-likelihood
  log_likelihood <- sum(dnorm(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), log = TRUE))
  return(-log_likelihood)  # Negative log-likelihood for optimization
}

################################
#To be careful, load data again and than fit ARJI
################################

# Step 1: Loaded and Cleaned GBTC Data 
GBTC <- getSymbols("GBTC", auto.assign = FALSE, from = "2020-04-11", to = "2024-09-13", src = "yahoo")
GBTC_df <- data.frame(Date = index(GBTC), coredata(GBTC))

GBTC_cleaned <- GBTC_df %>%
  mutate(
    Price = as.numeric(GBTC.Adjusted), # Used the Adjusted Close as the price
    Date = as.Date(Date)
  ) %>%
  select(Date, Price) %>%
  filter(Date >= as.Date("2020-04-11"))

GBTC_returns <- GBTC_cleaned %>%
  mutate(Return = c(NA, diff(log(Price)))) %>%
  filter(!is.na(Return))

# Step 2: Set Initial Parameters and Bounds
params_start <- c(omega = 0.1, alpha = 0.05, beta = 0.8, lambda0 = 0.05, rho = 0.5, gamma = 0.5)
lower_bounds <- c(omega = 1e-6, alpha = 1e-6, beta = 1e-6, lambda0 = 1e-6, rho = 0, gamma = 1e-6)
upper_bounds <- c(omega = 10, alpha = 1, beta = 1, lambda0 = 1, rho = 1, gamma = 10)

# Step 3: Fitted ARJI-GARCH Model to GBTC Returns
returns <- GBTC_returns$Return

fit <- optim(
  par = params_start,
  fn = fit_arji,
  returns = returns,
  method = "L-BFGS-B",
  lower = lower_bounds,
  upper = upper_bounds,
  control = list(maxit = 1000)
)

# Step 4: Calculated Log-Likelihood, AIC, and BIC
log_likelihood <- -fit_arji(fit$par, returns)
num_obs <- length(returns)
num_params <- length(fit$par)
AIC <- (-2 * log_likelihood / num_obs) + (2 * num_params / num_obs)
BIC <- (-2 * log_likelihood / num_obs) + (log(num_obs) * num_params / num_obs)

# Step 5: Displayed Results
cat("\n=== Results for GBTC ===\n")
cat("Parameters:\n")
print(fit$par)
cat("\nLog-Likelihood:", log_likelihood)
cat("\nAIC:", AIC)
cat("\nBIC:", BIC)

#####################
#With distributions
#####################

# Step 2: Defined Models and Distributions
distributions <- c("norm", "std", "ged", "sstd")  # Normal, Student's t, GED, and Skewed Student's t
models <- list(
  "GARCH(1,1)" = list(model = "sGARCH", order = c(1, 1)),
  "EGARCH(1,1)" = list(model = "eGARCH", order = c(1, 1)),
  "GJR-GARCH(1,1)" = list(model = "gjrGARCH", order = c(1, 1))
)

# Initialized results list
dist_results <- list()

# Step 3: Applied Models with Different Distributions to GBTC
for (model_name in names(models)) {
  model_spec <- models[[model_name]]
  
  for (dist in distributions) {
    cat("\n### Applying", model_name, "with", dist, "distribution ###\n")
    
    # Defined the model specification with the chosen distribution
    garch_spec <- ugarchspec(
      variance.model = list(model = model_spec$model, garchOrder = model_spec$order),
      mean.model = list(armaOrder = c(0, 0)),
      distribution.model = dist
    )
    
    # Fitted the model
    garch_fit <- tryCatch({
      fit <- ugarchfit(spec = garch_spec, data = returns)
      list(
        log_likelihood = likelihood(fit),
        AIC = infocriteria(fit)[1],
        BIC = infocriteria(fit)[2],
        parameters = coef(fit)
      )
    }, error = function(e) {
      cat("Error fitting", model_name, "with", dist, "distribution:", e$message, "\n")
      NULL
    })
    
    dist_results[[model_name]][[dist]] <- garch_fit
  }
}

# Step 4: Adding Jump-GARCH(1,1) with Different Distributions

#First try (Spoiler alert: It did not converge properly, so you can skip this try)

jump_garch_results <- list()
for (dist in distributions) {
  cat("\n### Applying Jump-GARCH(1,1) with", dist, "distribution ###\n")
  
  # Defined Jump-GARCH specifications
  params_start <- c(omega = 0.1, alpha = 0.05, beta = 0.8, lambda0 = 0.05, rho = 0.5, gamma = 0.5)
  lower_bounds <- c(omega = 1e-6, alpha = 1e-6, beta = 1e-6, lambda0 = 1e-6, rho = 0, gamma = 1e-6)
  upper_bounds <- c(omega = 10, alpha = 1, beta = 1, lambda0 = 1, rho = 1, gamma = 10)
  
  # Fitted Jump-GARCH using optimization
  jump_garch_fit <- tryCatch({
    fit <- optim(
      par = params_start,
      fn = fit_arji,
      returns = returns,
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds,
      control = list(maxit = 1000)
    )
    
    # Calculated log-likelihood, AIC, and BIC
    log_likelihood <- -fit_arji(fit$par, returns)
    num_obs <- length(returns)
    num_params <- length(fit$par)
    AIC <- (-2 * log_likelihood / num_obs) + (2 * num_params / num_obs)
    BIC <- (-2 * log_likelihood / num_obs) + (log(num_obs) * num_params / num_obs)
    
    list(parameters = fit$par, log_likelihood = log_likelihood, AIC = AIC, BIC = BIC)
  }, error = function(e) {
    cat("Error fitting Jump-GARCH with", dist, "distribution:", e$message, "\n")
    NULL
  })
  
  jump_garch_results[[dist]] <- jump_garch_fit
}
#######################################
# Jump-GARCH Model Fitting Corrected:)
#######################################

# Loaded required libraries
library(rugarch)
library(sn)

# Defined custom GED density function
dged_custom <- function(x, mean = 0, sd = 1, nu = 1.5, log = FALSE) {
  beta <- sqrt(gamma(1 / nu) / gamma(3 / nu))  # Scaling factor for GED
  z <- (x - mean) / (sd * beta)
  density <- nu / (2 * gamma(1 / nu) * beta) * exp(-abs(z)^nu)
  if (log) return(log(density))
  return(density)
}

# Defined custom skewed Student-t density function
dsstd_custom <- function(x, mean = 0, sd = 1, shape = 0, nu = 5, log = FALSE) {
  delta <- shape / sqrt(1 + shape^2)  # Skewness adjustment
  lambda <- sqrt(nu) * gamma((nu + 1) / 2) / (sqrt(pi * nu) * gamma(nu / 2))
  z <- (x - mean) / sd
  z_adj <- z / (1 + delta * sign(z))
  density <- 2 / (sd * lambda) * (1 + (z_adj^2 / nu))^(-(nu + 1) / 2)
  if (log) return(log(density))
  return(density)
}

# Defined distributions to test
distributions <- c("norm", "std", "ged", "sstd")

# Placeholder for results
jump_garch_results <- list()

# ARJI-GARCH Log-Likelihood Function
fit_arji <- function(params, returns, dist) {
  omega <- params["omega"]
  alpha <- params["alpha"]
  beta <- params["beta"]
  lambda0 <- params["lambda0"]
  rho <- params["rho"]
  gamma <- params["gamma"]
  
  h <- numeric(length(returns))
  lambda <- numeric(length(returns))
  u <- returns
  
  h[1] <- var(returns)
  lambda[1] <- lambda0
  
  for (t in 2:length(returns)) {
    h[t] <- pmax(omega + alpha * u[t - 1]^2 + beta * h[t - 1], 1e-6)
    lambda[t] <- pmax(lambda0 + rho * lambda[t - 1] + gamma * u[t - 1]^2, 1e-6)
  }
  
  log_density <- switch(
    dist,
    "norm" = dnorm(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), log = TRUE),
    "std" = dt(u / sqrt(h + lambda * (u^2 + var(u)) + 1e-6), df = 5, log = TRUE) - log(sqrt(h + lambda * (u^2 + var(u)) + 1e-6)),
    "ged" = dged_custom(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), nu = 1.5, log = TRUE),
    "sstd" = dsstd_custom(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), shape = 0.5, nu = 5.5, log = TRUE),
    stop("Invalid distribution")
  )
  
  log_likelihood <- sum(log_density)
  return(-log_likelihood)
}

# Fitted Jump-GARCH(1,1) models for GBTC with different distributions
for (dist in distributions) {
  cat("\n### Applying Jump-GARCH(1,1) with", dist, "distribution ###\n")
  
  params_start <- c(omega = 0.1, alpha = 0.05, beta = 0.8, lambda0 = 0.05, rho = 0.5, gamma = 0.5)
  lower_bounds <- c(omega = 1e-6, alpha = 1e-6, beta = 1e-6, lambda0 = 1e-6, rho = 0, gamma = 1e-6)
  upper_bounds <- c(omega = 10, alpha = 1, beta = 1, lambda0 = 1, rho = 1, gamma = 10)
  
  jump_garch_fit <- tryCatch({
    fit <- optim(
      par = params_start,
      fn = function(params) fit_arji(params, GBTC_returns$Return, dist),
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds,
      control = list(maxit = 1000)
    )
    
    log_likelihood <- -fit_arji(fit$par, GBTC_returns$Return, dist)
    num_obs <- length(GBTC_returns$Return)
    num_params <- length(fit$par)
    AIC <- (-2 * log_likelihood + 2 * num_params) / num_obs
    BIC <- (-2 * log_likelihood + log(num_obs) * num_params) / num_obs
    
    list(parameters = fit$par, log_likelihood = log_likelihood, AIC = AIC, BIC = BIC)
  }, error = function(e) {
    cat("Error fitting Jump-GARCH with", dist, "distribution:", e$message, "\n")
    NULL
  })
  
  jump_garch_results[[dist]] <- jump_garch_fit
}

# Printed the results for Jump-GARCH
cat("\n=== Results for Jump-GARCH(1,1) Model for GBTC ===\n")
for (dist in names(jump_garch_results)) {
  cat("\n--- Distribution:", dist, "---\n")
  print(jump_garch_results[[dist]])
}

cat("\n\n=== Results for GARCH Models on GBTC ===\n")
for (model_name in names(dist_results)) {
  cat("\n\n---", model_name, "---\n")
  for (dist in names(dist_results[[model_name]])) {
    cat("\nDistribution:", dist, "\n")
    print(dist_results[[model_name]][[dist]])
  }
}

#############
#heatmap
###############
# Loaded necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(viridis)

# Initialized a list to store AIC values for GBTC
aic_data_gbtc <- data.frame()

# Extracted AIC values from GARCH models
for (model_name in names(dist_results)) {
  for (dist in names(dist_results[[model_name]])) {
    if (!is.null(dist_results[[model_name]][[dist]])) {
      aic_value <- dist_results[[model_name]][[dist]]$AIC
      aic_data_gbtc <- rbind(aic_data_gbtc, data.frame(Model = model_name, Distribution = dist, AIC = aic_value))
    }
  }
}

# Extracted AIC values from Jump-GARCH models
for (dist in names(jump_garch_results)) {
  if (!is.null(jump_garch_results[[dist]])) {
    aic_value <- jump_garch_results[[dist]]$AIC
    aic_data_gbtc <- rbind(aic_data_gbtc, data.frame(Model = "Jump-GARCH(1,1)", Distribution = dist, AIC = aic_value))
  }
}


# Calculated AIC ranks (1 = best, highest rank = worst)
aic_data_gbtc <- aic_data_gbtc %>%
  mutate(AIC_Rank = rank(-AIC, ties.method = "first"))

# Combined Model and Distribution for ease of plotting
aic_data_gbtc$Model_Distribution <- paste(aic_data_gbtc$Model, aic_data_gbtc$Distribution, sep = "_")

# Plotted the heatmap using viridis color palette
ggplot(aic_data_gbtc, aes(x = Model_Distribution, y = "", fill = factor(AIC_Rank))) +
  geom_tile(color = "white") +
  scale_fill_viridis_d(option = "viridis", direction = -1, labels = function(x) 17 - as.numeric(x)) +  # Use "viridis", can also use "plasma", "magma", etc.
  labs(
    title = "Relative AIC Ranking Heatmap for GBTC Models and Distributions",
    x = "Model + Distribution",
    y = "GBTC
    
    ",
    fill = "AIC Rank"
  ) +
  scale_x_discrete(labels = function(labels) {
    # Replace "_" with " + " and capitalize distribution names
    labels <- gsub("_", " + ", labels)
    labels <- gsub("ged", "GED", labels)
    labels <- gsub("norm", "NORM", labels)
    labels <- gsub("std", "STD", labels)
    labels <- gsub("sstd", "SSTD", labels)
    labels <- gsub("Jump", "ARJI", labels)
    return(labels)
  }) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5)
  )

##########
#Heatmap BIC and log
#########

# Loaded necessary libraries
library(ggplot2)
library(dplyr)
library(viridis)

# Initialized lists to store BIC and Log-Likelihood data for GBTC
bic_data_gbtc <- data.frame()
loglikelihood_data_gbtc <- data.frame()

# Extracted BIC and Log-Likelihood values from GARCH models
for (model_name in names(dist_results)) {
  for (dist in names(dist_results[[model_name]])) {
    if (!is.null(dist_results[[model_name]][[dist]])) {
      bic_value <- dist_results[[model_name]][[dist]]$BIC
      loglikelihood_value <- dist_results[[model_name]][[dist]]$log_likelihood
      bic_data_gbtc <- rbind(bic_data_gbtc, data.frame(Model = model_name, Distribution = dist, BIC = bic_value))
      loglikelihood_data_gbtc <- rbind(loglikelihood_data_gbtc, data.frame(Model = model_name, Distribution = dist, LogLikelihood = loglikelihood_value))
    }
  }
}

# Extracted BIC and Log-Likelihood values from Jump-GARCH models
for (dist in names(jump_garch_results)) {
  if (!is.null(jump_garch_results[[dist]])) {
    bic_value <- jump_garch_results[[dist]]$BIC
    loglikelihood_value <- jump_garch_results[[dist]]$log_likelihood
    bic_data_gbtc <- rbind(bic_data_gbtc, data.frame(Model = "Jump-GARCH(1,1)", Distribution = dist, BIC = bic_value))
    loglikelihood_data_gbtc <- rbind(loglikelihood_data_gbtc, data.frame(Model = "Jump-GARCH(1,1)", Distribution = dist, LogLikelihood = loglikelihood_value))
  }
}

# Calculated BIC and Log-Likelihood ranks (1 = best, highest rank = worst)
bic_data_gbtc <- bic_data_gbtc %>%
  mutate(BIC_Rank = rank(-BIC, ties.method = "first"))

loglikelihood_data_gbtc <- loglikelihood_data_gbtc %>%
  mutate(LogLikelihood_Rank = rank(LogLikelihood, ties.method = "first"))

# Combined Model and Distribution for ease of plotting
bic_data_gbtc$Model_Distribution <- paste(bic_data_gbtc$Model, bic_data_gbtc$Distribution, sep = "_")
loglikelihood_data_gbtc$Model_Distribution <- paste(loglikelihood_data_gbtc$Model, loglikelihood_data_gbtc$Distribution, sep = "_")

# Plotted BIC heatmap for GBTC
ggplot(bic_data_gbtc, aes(x = Model_Distribution, y = "", fill = factor(BIC_Rank))) +
  geom_tile(color = "white") +
  scale_fill_viridis_d(option = "viridis", direction = -1, labels = function(x) 17 - as.numeric(x)) +
  labs(
    title = "Relative BIC Ranking Heatmap for GBTC Models and Distributions",
    x = "Model + Distribution",
    y = "GBTC
    
    ",
    fill = "BIC Rank"
  ) +
  scale_x_discrete(labels = function(labels) {
    # Replace "_" with " + " and capitalize distribution names
    labels <- gsub("_", " + ", labels)
    labels <- gsub("ged", "GED", labels)
    labels <- gsub("norm", "NORM", labels)
    labels <- gsub("std", "STD", labels)
    labels <- gsub("sstd", "SSTD", labels)
    labels <- gsub("Jump", "ARJI", labels)
    return(labels)
  }) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5)
  )

# Plot for Log-Likelihood heatmap for GBTC
ggplot(loglikelihood_data_gbtc, aes(x = Model_Distribution, y = "", fill = factor(LogLikelihood_Rank))) +
  geom_tile(color = "white") +
  scale_fill_viridis_d(option = "viridis", direction = -1, labels = function(x) 17 - as.numeric(x)) +
  labs(
    title = "Relative Log-Likelihood Ranking Heatmap for GBTC Models and Distributions",
    x = "Model + Distribution",
    y = "GBTC
    
    ",
    fill = "LL Rank"
  ) +
  scale_x_discrete(labels = function(labels) {
    # Replace "_" with " + " and capitalize distribution names
    labels <- gsub("_", " + ", labels)
    labels <- gsub("ged", "GED", labels)
    labels <- gsub("norm", "NORM", labels)
    labels <- gsub("std", "STD", labels)
    labels <- gsub("sstd", "SSTD", labels)
    labels <- gsub("Jump", "ARJI", labels)
    return(labels)
  }) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5)
  )
#####################

# Loaded necessary libraries
library(dplyr)
library(ggplot2)

# Defined distributions and models
distributions <- c("norm", "std", "ged", "sstd")
models <- c("GARCH(1,1)", "EGARCH(1,1)", "GJR-GARCH(1,1)", "Jump-GARCH(1,1)")

# Initialized dataframes to store pairwise outperformance
aic_outperformance_comparison <- expand.grid(Model1 = models, Model2 = models)
bic_outperformance_comparison <- expand.grid(Model1 = models, Model2 = models)
loglikelihood_outperformance_comparison <- expand.grid(Model1 = models, Model2 = models)

# Added columns for percentage
aic_outperformance_comparison$Percentage <- 0
bic_outperformance_comparison$Percentage <- 0
loglikelihood_outperformance_comparison$Percentage <- 0

# Total comparisons (GBTC-specific)
total_comparisons <- length(distributions)

# Function to calculate pairwise outperformance for a metric
calculate_pairwise_outperformance <- function(metric_data, metric_name, comparison_data) {
  for (dist in distributions) {
    # Filtered metric data for the current distribution
    dist_data <- metric_data %>% filter(Distribution == dist)
    
    # Compared models pairwise
    for (i in 1:nrow(comparison_data)) {
      model1 <- comparison_data$Model1[i]
      model2 <- comparison_data$Model2[i]
      
      if (model1 != model2) {
        # Get metric values for both models
        metric1 <- dist_data %>% filter(Model == model1) %>% pull(!!sym(metric_name))
        metric2 <- dist_data %>% filter(Model == model2) %>% pull(!!sym(metric_name))
        
        # Increment count based on metric criterion
        if (length(metric1) > 0 && length(metric2) > 0) {
          if (metric_name == "LogLikelihood") {
            # Higher Log-Likelihood is better
            if (metric1 > metric2) {
              comparison_data$Percentage[i] <- comparison_data$Percentage[i] + 1
            }
          } else {
            # Lower AIC/BIC is better
            if (metric1 < metric2) {
              comparison_data$Percentage[i] <- comparison_data$Percentage[i] + 1
            }
          }
        }
      }
    }
  }
  # Converted counts to percentages
  comparison_data$Percentage <- (comparison_data$Percentage / total_comparisons) * 100
  return(comparison_data)
}

# Calculated pairwise outperformance for each metric
aic_outperformance_comparison <- calculate_pairwise_outperformance(aic_data_gbtc, "AIC", aic_outperformance_comparison)
bic_outperformance_comparison <- calculate_pairwise_outperformance(bic_data_gbtc, "BIC", bic_outperformance_comparison)
loglikelihood_outperformance_comparison <- calculate_pairwise_outperformance(loglikelihood_data_gbtc, "LogLikelihood", loglikelihood_outperformance_comparison)

# Function to create improved heatmaps for pairwise comparisons
create_improved_heatmap <- function(data, metric) {
  data <- data %>% mutate(Percentage = ifelse(Model1 == Model2, NA, Percentage))
  
  ggplot(data, aes(x = Model2, y = Model1, fill = Percentage)) +
    geom_tile(color = "white") +
    geom_text(aes(label = ifelse(is.na(Percentage), "Same Model", round(Percentage, 1))), 
              color = "black", size = 4) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
    labs(
      title = paste("Pairwise Model Outperformance based on", metric),
      x = "Outperformed Model (Model2)",
      y = "Outperforming Model (Model1)",
      fill = "Outperformance (%)"
    ) +
    scale_x_discrete(labels = function(labels) gsub("Jump-GARCH", "ARJI-GARCH", labels)) +
    scale_y_discrete(labels = function(labels) gsub("Jump-GARCH", "ARJI-GARCH", labels)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(size = 10)
    ) +
    coord_fixed() + 
    annotate("rect", xmin = 0.5, xmax = nlevels(data$Model2) + 0.5, ymin = 0.5, ymax = nlevels(data$Model1) + 0.5, 
             color = "black", fill = NA, size = 0.1)
}

# Created and displayed heatmaps for each metric
print(create_improved_heatmap(aic_outperformance_comparison, "AIC"))
print(create_improved_heatmap(bic_outperformance_comparison, "BIC"))
print(create_improved_heatmap(loglikelihood_outperformance_comparison, "Log-Likelihood"))


  
#########################################################################################################
# Comparison AIC, BIC, log by distribution GARCH
#########################################################################################################

prepare_metric_data <- function(results, metric_name) {
  data <- data.frame()
  for (model_name in names(results)) {
    for (dist in names(results[[model_name]])) {
      if (!is.null(results[[model_name]][[dist]])) {
        metric_value <- results[[model_name]][[dist]][[metric_name]]
        data <- rbind(data, data.frame(Model = model_name, Distribution = dist, Metric = metric_value))
      }
    }
  }
  return(data)
}

calculate_outperformance <- function(metric_data) {
  distributions <- unique(metric_data$Distribution)
  comparison_data <- expand.grid(Distribution1 = distributions, Distribution2 = distributions) %>%
    mutate(Percentage = NA)
  
  for (i in 1:nrow(comparison_data)) {
    dist1 <- comparison_data$Distribution1[i]
    dist2 <- comparison_data$Distribution2[i]
    
    if (dist1 != dist2) {
      metric1 <- metric_data %>% filter(Distribution == dist1) %>% pull(Metric)
      metric2 <- metric_data %>% filter(Distribution == dist2) %>% pull(Metric)
      total <- length(metric1)
      
      if (total > 0) {
        count_dist1 <- sum(metric1 < metric2, na.rm = TRUE)
        count_dist2 <- sum(metric2 < metric1, na.rm = TRUE)
        total_comparisons <- count_dist1 + count_dist2
        
        if (total_comparisons > 0) {
          comparison_data$Percentage[i] <- (count_dist1 / total_comparisons) * 100
          reverse_index <- which(comparison_data$Distribution1 == dist2 & 
                                   comparison_data$Distribution2 == dist1)
          comparison_data$Percentage[reverse_index] <- (count_dist2 / total_comparisons) * 100
        }
      }
    }
  }
  
  # Replaced diagonal (same distribution) with NA
  comparison_data <- comparison_data %>%
    mutate(Percentage = ifelse(Distribution1 == Distribution2, NA, Percentage))
  
  return(comparison_data)
}

create_heatmap <- function(data, metric_name, model_name) {
  ggplot(data, aes(x = Distribution2, y = Distribution1, fill = Percentage)) +
    geom_tile(color = "white") +
    geom_text(aes(label = ifelse(is.na(Percentage), "Same Dist.", round(Percentage, 1))), size = 4) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
    labs(
      title = paste("Pairwise Distribution Outperformance for", metric_name, "(", model_name, ")"),
      x = "Outperformed Distribution",
      y = "Outperforming Distribution",
      fill = paste(metric_name, "(%)")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(size = 10),
      panel.background = element_blank(),  
      panel.grid = element_blank(),  
      plot.background = element_blank()  
    ) +
    coord_fixed()+
  annotate(
    "rect", 
    xmin = 0.5, 
    xmax = nlevels(data$Distribution2) + 0.5,
    ymin = 0.5, 
    ymax = nlevels(data$Distribution1) + 0.5, 
    color = "black", 
    fill = NA, 
    size = 0.1  
  )
}

# Prepared metric data for AIC, BIC, and Log-Likelihood
aic_data <- prepare_metric_data(dist_results, "AIC")
bic_data <- prepare_metric_data(dist_results, "BIC")
loglikelihood_data <- prepare_metric_data(dist_results, "log_likelihood")

# Calculated pairwise outperformance
aic_outperformance <- calculate_outperformance(aic_data)
bic_outperformance <- calculate_outperformance(bic_data)
loglikelihood_outperformance <- calculate_outperformance(loglikelihood_data)

# Created heatmaps
aic_heatmap <- create_heatmap(aic_outperformance, "AIC", "GARCH Models")
bic_heatmap <- create_heatmap(bic_outperformance, "BIC", "GARCH Models")
loglikelihood_heatmap <- create_heatmap(loglikelihood_outperformance, "Log-Likelihood", "GARCH Models")

# Displayed heatmaps
print(aic_heatmap)
print(bic_heatmap)
print(loglikelihood_heatmap)

