#Do everything again with more data. Uhu

#########################################################################################################
# Cryptocurrency Volatility Modeling using GARCH, EGARCH, GJRGARCH, and Jump-GARCH Variants
#########################################################################################################

# Installed necessary packages
install.packages("moments")
install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("rugarch")

# Loaded necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(rugarch)

#########################################################################################################
# Load and Prepare Data
#########################################################################################################

# Loaded data sets for selected cryptocurrencies
crypto_datasets <- list(
  BTC = read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/BTC.csv"),
  ETH = read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/ETH.csv"),
  Tether = read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/Tether.csv"),
  BNB = read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/BNB.csv"),
  SOL = read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/SOL.csv"),
  USDC = read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/USDC.csv"),
  Ripple = read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/Ripple.csv"),
  DOGE = read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/DOGE.csv"),
  TRON = read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/TRON.csv")
)

# Ensured Price is numeric and filtered dates from *2018-12-18* onwards
crypto_datasets <- lapply(crypto_datasets, function(df) {
  df %>%
    mutate(
      Price = as.numeric(Price),
      Date = as.Date(sub(" .*", "", Date))
    ) %>%
    filter(Date >= as.Date("2018-12-18"))
})

# Computed log returns for each cryptocurrency
compute_log_returns <- function(df) {
  df %>%
    mutate(Return = c(NA, diff(log(Price)))) %>%
    filter(!is.na(Return))
}
crypto_returns <- lapply(crypto_datasets, compute_log_returns)

# Restricted data to March 15, 2024
end_date <- as.Date("2024-03-15")

# Filtered each cryptocurrency dataset to include data only up to the end date
crypto_returns <- lapply(crypto_returns, function(df) {
  df %>%
    filter(Date <= end_date)
})


#########################################################################################################
# GARCH, EGARCH, GJR-GARCH Model Fitting
#########################################################################################################

# Defined GARCH, EGARCH, and GJR-GARCH specifications
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

# Function to fit GARCH models and print summaries
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

# Fitted GARCH models for each cryptocurrency
garch_results <- lapply(crypto_returns, function(returns_df) {
  fit_models(returns_df$Return, garch_specs)
})

# Displayed results for each model and cryptocurrency
for (crypto in names(garch_results)) {
  cat("\n\n=== Results for", crypto, "===\n")
  lapply(garch_results[[crypto]], function(result) {
    if (!is.null(result)) {
      cat("\n", result$model, "\n")
      print(result$fit)
    }
  })
}

#########################################################################################################
# Jump-GARCH Model Fitting
#########################################################################################################

# Defined ARJI-GARCH log-likelihood function
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
  
  h[1] <- var(returns)
  lambda[1] <- lambda0
  
  for (t in 2:length(returns)) {
    h[t] <- pmax(omega + alpha * u[t - 1]^2 + beta * h[t - 1], 1e-6)
    lambda[t] <- pmax(lambda0 + rho * lambda[t - 1] + gamma * u[t - 1]^2, 1e-6)
  }
  
  log_likelihood <- sum(dnorm(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), log = TRUE))
  return(-log_likelihood)
}

# Set initial parameters and bounds for optimization
params_start <- c(omega = 0.1, alpha = 0.05, beta = 0.8, lambda0 = 0.05, rho = 0.5, gamma = 0.5)
lower_bounds <- c(omega = 1e-6, alpha = 1e-6, beta = 1e-6, lambda0 = 1e-6, rho = 0, gamma = 1e-6)
upper_bounds <- c(omega = 10, alpha = 1, beta = 1, lambda0 = 1, rho = 1, gamma = 10)

# Fitted ARJI-GARCH model for each cryptocurrency
jump_garch_results <- lapply(crypto_returns, function(df) {
  returns <- df$Return
  
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
})

# Printed summary of ARJI-GARCH results
for (crypto in names(jump_garch_results)) {
  cat("\n", crypto, ":\n")
  print(jump_garch_results[[crypto]])
}

#########################################################################################################
# GARCH(1,1), EGARCH(1,1), GJRGARCH(1,1), and Jump-GARCH(1,1) with Different Distributions: "norm", "std", "ged", "sstd"
#########################################################################################################

# Defined the models and distributions to apply
distributions <- c("norm", "std", "ged", "sstd")  # Normal, Student's t, GED, and Skewed Student's t
models <- list(
  "GARCH(1,1)" = list(model = "sGARCH", order = c(1, 1)),
  "EGARCH(1,1)" = list(model = "eGARCH", order = c(1, 1)),
  "GJR-GARCH(1,1)" = list(model = "gjrGARCH", order = c(1, 1))
)

# Initialized results list
dist_results <- list()

# Applied each model with different distributions to each cryptocurrency
for (model_name in names(models)) {
  model_spec <- models[[model_name]]
  
  for (dist in distributions) {
    cat("\n### Applying", model_name, "with", dist, "distribution ###\n")
    
    dist_results[[model_name]][[dist]] <- lapply(crypto_returns, function(df) {
      returns <- df$Return
      
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
      
      garch_fit
    })
  }
}

# Adding Jump-GARCH(1,1) with Different Distributions: Attention, takes really long to load!

#First try (Spoiler alert: It did not converge properly, so you can skip this try)
jump_garch_results <- list()
for (dist in distributions) {
  cat("\n### Applying Jump-GARCH(1,1) with", dist, "distribution ###\n")
  
  jump_garch_results[[dist]] <- lapply(crypto_returns, function(df) {
    returns <- df$Return
    
    # Defined the Jump-GARCH model specification with the chosen distribution
    jump_garch_spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(0, 0)),
      distribution.model = dist
    )
    
    # Defined initial parameters and bounds for optimization specific to Jump-GARCH
    params_start <- c(omega = 0.1, alpha = 0.05, beta = 0.8, lambda0 = 0.05, rho = 0.5, gamma = 0.5)
    lower_bounds <- c(omega = 1e-6, alpha = 1e-6, beta = 1e-6, lambda0 = 1e-6, rho = 0, gamma = 1e-6)
    upper_bounds <- c(omega = 10, alpha = 1, beta = 1, lambda0 = 1, rho = 1, gamma = 10)
    
    # Fitted the Jump-GARCH model using optimization
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
    
    jump_garch_fit
  })
}

#Upper code did not work. Problem was probably that it could not converge. Across all dist, model fit parameters were always the same.
#Thus, code a new version:

# Loaded required libraries 
install.packages("sn") #For Student-t...

library(rugarch)
library(sn)  # ...Provides the `dst` function for Student-t

# Defined distributions to test
distributions <- c("norm", "std", "ged", "sstd")

# Placeholder for results
jump_garch_results <- list()

# Custom GED density function (some paramters, such as beta, were labelled differently in the thesis)
dged_custom <- function(x, mean = 0, sd = 1, nu = 1.5, log = FALSE) {
  beta <- sqrt(gamma(1 / nu) / gamma(3 / nu))  # Scaling factor for GED
  z <- (x - mean) / (sd * beta)
  density <- nu / (2 * gamma(1 / nu) * beta) * exp(-abs(z)^nu)
  if (log) return(log(density))
  return(density)
}

# Custom skewed Student-t density function (simplified)
dsstd_custom <- function(x, mean = 0, sd = 1, shape = 0, nu = 5, log = FALSE) {
  # Placeholder for skewed Student-t implementation
  stop("Skewed Student-t distribution requires a specific implementation.")
}

# Upper custom did not work when later runnig the ARJI-GARCH, so here an alternative:

dsstd_custom <- function(x, mean = 0, sd = 1, shape = 0, nu = 5, log = FALSE) {
  # Calculated delta (skewness adjustment)
  delta <- shape / sqrt(1 + shape^2)
  
  # Normalization constant lambda
  lambda <- sqrt(nu) * gamma((nu + 1) / 2) / (sqrt(pi) * gamma(nu / 2))
  
  # Standardized x
  z <- (x - mean) / sd
  
  # Adjusted z based on skewness
  z_adj <- z / (1 + delta * sign(z))
  
  # Density calculation
  density <- (2 / (sd * lambda * sqrt(nu) * (1 + delta^2))) *
    (1 + (z_adj^2 / nu))^(-(nu + 1) / 2)
  
  if (log) {
    return(log(density))
  }
  
  return(density)
}


# Looped over distributions
for (dist in distributions) {
  cat("\n### Applying Jump-GARCH(1,1) with", dist, "distribution ###\n")
  
  jump_garch_results[[dist]] <- lapply(crypto_returns, function(df) {
    returns <- df$Return
    
    # Defineed initial parameters and bounds
    params_start <- c(omega = 0.1, alpha = 0.05, beta = 0.8, lambda0 = 0.05, rho = 0.5, gamma = 0.5)
    lower_bounds <- c(omega = 1e-6, alpha = 1e-6, beta = 1e-6, lambda0 = 1e-6, rho = 0, gamma = 1e-6)
    upper_bounds <- c(omega = 10, alpha = 1, beta = 1, lambda0 = 1, rho = 1, gamma = 10)
    
    # Defined the ARJI log-likelihood function
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
      
      # Log-likelihood calculation based on distribution
      log_density <- switch(
        dist,
        "norm" = dnorm(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), log = TRUE),
        "std" = dt(u / sqrt(h + lambda * (u^2 + var(u)) + 1e-6), df = 5, log = TRUE) - log(sqrt(h + lambda * (u^2 + var(u)) + 1e-6)),
        "ged" = dged_custom(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), nu = 1.5, log = TRUE),
        "sstd" = dsstd_custom(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), shape = 0, nu = 5.5, log = TRUE),
        stop("Invalid distribution") #Remark: The values of the shape parameters impact the values for the model fit parameters. Try to maximize (or minimize in case of AIC and BIC) the fit parameters manually.
      )
      
      log_likelihood <- sum(log_density)
      return(-log_likelihood)
    }
    
    # Optimized the Jump-GARCH model
    jump_garch_fit <- tryCatch({
      fit <- optim(
        par = params_start,
        fn = function(params) fit_arji(params, returns, dist),
        method = "L-BFGS-B",
        lower = lower_bounds,
        upper = upper_bounds,
        control = list(maxit = 1000)
      )
      
      # Calculated log-likelihood, AIC, and BIC
      log_likelihood <- -fit_arji(fit$par, returns, dist)
      num_obs <- length(returns)
      num_params <- length(fit$par)
      AIC <-  AIC <- (-2 * log_likelihood + 2 * num_params) / num_obs
      BIC <- (-2 * log_likelihood + log(num_obs) * num_params) / num_obs
      
      list(parameters = fit$par, log_likelihood = log_likelihood, AIC = AIC, BIC = BIC)
    }, error = function(e) {
      cat("Error fitting Jump-GARCH with", dist, "distribution:", e$message, "\n")
      NULL
    })
    
    jump_garch_fit
  })
}

# Printed the summary of results for each cryptocurrency, model, and distribution
for (model_name in names(dist_results)) {
  cat("\n\n=== Results for", model_name, "Model ===\n")
  
  for (dist in names(dist_results[[model_name]])) {
    cat("\n--- Distribution:", dist, "---\n")
    
    for (crypto in names(dist_results[[model_name]][[dist]])) {
      cat("\n", crypto, ":\n")
      print(dist_results[[model_name]][[dist]][[crypto]])
    }
  }
}

# Printed the summary of results for each cryptocurrency, model, and distribution
for (model_name in names(dist_results)) {
  cat("\n\n=== Results for", model_name, "Model ===\n")
  
  for (dist in names(dist_results[[model_name]])) {
    cat("\n--- Distribution:", dist, "---\n")
    
    for (crypto in names(dist_results[[model_name]][[dist]])) {
      cat("\n", crypto, ":\n")
      print(dist_results[[model_name]][[dist]][[crypto]])
    }
  }
}

# Printed the summary of Jump-GARCH results
cat("\n\n=== Results for Jump-GARCH(1,1) Model ===\n")
for (dist in names(jump_garch_results)) {
  cat("\n--- Distribution:", dist, "---\n")
  
  for (crypto in names(jump_garch_results[[dist]])) {
    cat("\n", crypto, ":\n")
    print(jump_garch_results[[dist]][[crypto]])
  }
}

#########################################################################################################
# Heatmap for Relative AIC Rankings with Custom Color Palette
#########################################################################################################
# Loaded necessary libraries for plotting
library(ggplot2)
library(reshape2)
library(dplyr)
library(viridis)  # Ensure viridis is loaded

# Initialized a list to store AIC values for each model-distribution combination
aic_data <- list()

# Extracted AIC values from each model and distribution, and organized them into a data frame
for (model_name in names(dist_results)) {
  for (dist in names(dist_results[[model_name]])) {
    for (crypto in names(dist_results[[model_name]][[dist]])) {
      # Check if the model fit was successful and AIC is available
      if (!is.null(dist_results[[model_name]][[dist]][[crypto]])) {
        aic_value <- dist_results[[model_name]][[dist]][[crypto]]$AIC
        aic_data <- rbind(aic_data, data.frame(Model = model_name, Distribution = dist, Crypto = crypto, AIC = aic_value))
      }
    }
  }
}

# Added Jump-GARCH results to the AIC data frame
for (dist in names(jump_garch_results)) {
  for (crypto in names(jump_garch_results[[dist]])) {
    # Check if the model fit was successful and AIC is available
    if (!is.null(jump_garch_results[[dist]][[crypto]])) {
      aic_value <- jump_garch_results[[dist]][[crypto]]$AIC
      aic_data <- rbind(aic_data, data.frame(Model = "Jump-GARCH(1,1)", Distribution = dist, Crypto = crypto, AIC = aic_value))
    }
  }
}

# Calculated AIC ranks for each cryptocurrency (1 = best, highest rank = worst)
aic_data <- aic_data %>%
  group_by(Crypto) %>%
  mutate(AIC_Rank = rank(-AIC, ties.method = "first")) %>%
  ungroup()

# Combined Model and Distribution for ease of plotting
aic_data$Model_Distribution <- paste(aic_data$Model, aic_data$Distribution, sep = "_")

crypto_order <- c("TRON", "DOGE", "Ripple", "USDC", "SOL", "BNB", "Tether", "ETH", "BTC")
# Converted the Crypto column to a factor with the specified levels
aic_data$Crypto <- factor(aic_data$Crypto, levels = crypto_order)

# Plotted the heatmap using the viridis color palette:)
ggplot(aic_data, aes(x = Model_Distribution, y = Crypto, fill = factor(AIC_Rank))) +
  geom_tile(color = "white") +
  scale_fill_viridis_d(option = "viridis", direction = -1, labels = function(x) 17 - as.numeric(x)) +  # "viridis" is so cool.
  labs(
    title = "Relative AIC Ranking Heatmap for Different Models and Distributions",
    x = "Model + Distribution",
    y = "Cryptocurrency",
    fill = "AIC Rank"
  ) +
  scale_x_discrete(labels = function(labels) {
    # Replaced "_" with " + " and capitalized distribution names
    labels <- gsub("_", " + ", labels)
    labels <- gsub("ged", "GED", labels)
    labels <- gsub("norm", "NORM", labels)
    labels <- gsub("std", "STD", labels)
    labels <- gsub("sstd", "SSTD", labels)
    labels <- gsub("Jump", "ARJI", labels)
    return(labels)
  }) +
  scale_y_discrete(labels = c(
    "Tether" = "USDT",
    "TRON" = "TRX",
    "Ripple" = "XRP"
  )) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5)
  )
#########################################################################################################
# Prepare Data for BIC and Log-Likelihood Rankings
#########################################################################################################
# Initialized lists to store BIC and Log-likelihood data for each model-distribution combination
bic_data <- list()
loglikelihood_data <- list()

# Extracted BIC and Log-likelihood values from each model and distribution, and organized them into data frames
for (model_name in names(dist_results)) {
  for (dist in names(dist_results[[model_name]])) {
    for (crypto in names(dist_results[[model_name]][[dist]])) {
      # Check if the model fit was successful and metrics are available
      if (!is.null(dist_results[[model_name]][[dist]][[crypto]])) {
        bic_value <- dist_results[[model_name]][[dist]][[crypto]]$BIC
        loglikelihood_value <- dist_results[[model_name]][[dist]][[crypto]]$log_likelihood
        bic_data <- rbind(bic_data, data.frame(Model = model_name, Distribution = dist, Crypto = crypto, BIC = bic_value))
        loglikelihood_data <- rbind(loglikelihood_data, data.frame(Model = model_name, Distribution = dist, Crypto = crypto, LogLikelihood = loglikelihood_value))
      }
    }
  }
}

# Added Jump-GARCH results to BIC and Log-likelihood data frames
for (dist in names(jump_garch_results)) {
  for (crypto in names(jump_garch_results[[dist]])) {
    if (!is.null(jump_garch_results[[dist]][[crypto]])) {
      bic_value <- jump_garch_results[[dist]][[crypto]]$BIC
      loglikelihood_value <- jump_garch_results[[dist]][[crypto]]$log_likelihood
      bic_data <- rbind(bic_data, data.frame(Model = "Jump-GARCH(1,1)", Distribution = dist, Crypto = crypto, BIC = bic_value))
      loglikelihood_data <- rbind(loglikelihood_data, data.frame(Model = "Jump-GARCH(1,1)", Distribution = dist, Crypto = crypto, LogLikelihood = loglikelihood_value))
    }
  }
}

# Calculated BIC and Log-likelihood ranks for each cryptocurrency (1 = best, highest rank = worst)
bic_data <- bic_data %>%
  group_by(Crypto) %>%
  mutate(BIC_Rank = rank(-BIC, ties.method = "first")) %>%
  ungroup()

loglikelihood_data <- loglikelihood_data %>%
  group_by(Crypto) %>%
  mutate(LogLikelihood_Rank = rank(LogLikelihood, ties.method = "first")) %>%
  ungroup()

# Combined Model and Distribution for ease of plotting
bic_data$Model_Distribution <- paste(bic_data$Model, bic_data$Distribution, sep = "_")
loglikelihood_data$Model_Distribution <- paste(loglikelihood_data$Model, loglikelihood_data$Distribution, sep = "_")

# Converted the Crypto column to a factor with the specified levels
crypto_order <- c("TRON", "DOGE", "Ripple", "USDC", "SOL", "BNB", "Tether", "ETH", "BTC")
bic_data$Crypto <- factor(bic_data$Crypto, levels = crypto_order)
loglikelihood_data$Crypto <- factor(loglikelihood_data$Crypto, levels = crypto_order)

# Plotted BIC heatmap
ggplot(bic_data, aes(x = Model_Distribution, y = Crypto, fill = factor(BIC_Rank))) +
  geom_tile(color = "white") +
  scale_fill_viridis_d(option = "viridis", direction = -1, labels = function(x) 17 - as.numeric(x)) +
  labs(
    title = "Relative BIC Ranking Heatmap for Different Models and Distributions",
    x = "Model + Distribution",
    y = "Cryptocurrency",
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
  scale_y_discrete(labels = c(
    "Tether" = "USDT",
    "TRON" = "TRX",
    "Ripple" = "XRP"
  )) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5)
  )

# Plotted Log-likelihood heatmap
ggplot(loglikelihood_data, aes(x = Model_Distribution, y = Crypto, fill = factor(LogLikelihood_Rank))) +
  geom_tile(color = "white") +
  scale_fill_viridis_d(option = "viridis", direction = -1, labels = function(x) 17 - as.numeric(x)) +
  labs(
    title = "Relative Log-Likelihood Ranking Heatmap for Different Models and Distributions",
    x = "Model + Distribution",
    y = "Cryptocurrency",
    fill = "LL Rank"
  ) +
  scale_x_discrete(labels = function(labels) {
    # Replaced "_" with " + " and capitalized distribution names
    labels <- gsub("_", " + ", labels)
    labels <- gsub("ged", "GED", labels)
    labels <- gsub("norm", "NORM", labels)
    labels <- gsub("std", "STD", labels)
    labels <- gsub("sst", "SSTD", labels)
    labels <- gsub("Jump", "ARJI", labels)
    return(labels)
  }) +
  scale_y_discrete(labels = c(
    "Tether" = "USDT",
    "TRON" = "TRX",
    "Ripple" = "XRP"
  )) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5)
  )


#########################################################################################################
# Comparison AIC, BIC, log-likelihood by model
#########################################################################################################

# Initialized a data frame to store counts of pairwise outperformance for each model
model_names <- c("GARCH(1,1)", "EGARCH(1,1)", "GJR-GARCH(1,1)", "Jump-GARCH(1,1)")
outperformance_comparison <- expand.grid(Model1 = model_names, Model2 = model_names)
outperformance_comparison$Percentage <- 0

#Looped through each distribution
for (dist in distributions) {
  # Filter AIC data to the current distribution
  dist_aic_data <- aic_data %>% filter(Distribution == dist)
  
  #Grouped by cryptocurrency and find AIC for each model
  for (crypto in unique(dist_aic_data$Crypto)) {
    crypto_data <- dist_aic_data %>% filter(Crypto == crypto)
    
    #Pairwise comparison of models based on AIC
    for (i in 1:nrow(outperformance_comparison)) {
      model1 <- outperformance_comparison$Model1[i]
      model2 <- outperformance_comparison$Model2[i]
      
      if (model1 != model2) {
        aic1 <- crypto_data %>% filter(Model == model1) %>% pull(AIC)
        aic2 <- crypto_data %>% filter(Model == model2) %>% pull(AIC)
        
        #Increment count if model1 has a lower (better) AIC than model2
        if (length(aic1) > 0 && length(aic2) > 0 && aic1 < aic2) {
          outperformance_comparison$Percentage[i] <- outperformance_comparison$Percentage[i] + 1
        }
      }
    }
  }
}

#Calculated the total number of comparisons for each Model1-Model2 pair
total_comparisons <- length(distributions) * length(unique(aic_data$Crypto))

#Converted counts to percentages
outperformance_comparison$Percentage <- (outperformance_comparison$Percentage / total_comparisons) * 100

#Pairwise outperformance comparison table in percentages
print(outperformance_comparison)

#Everything for BIC and log-likelihood
#Initialized a data frame to store counts of pairwise outperformance for each model based on BIC and Log-Likelihood
model_names <- c("GARCH(1,1)", "EGARCH(1,1)", "GJR-GARCH(1,1)", "Jump-GARCH(1,1)")

#BIC Outperformance Comparison
bic_outperformance_comparison <- expand.grid(Model1 = model_names, Model2 = model_names)
bic_outperformance_comparison$Percentage <- 0

#Log-Likelihood Outperformance Comparison
loglikelihood_outperformance_comparison <- expand.grid(Model1 = model_names, Model2 = model_names)
loglikelihood_outperformance_comparison$Percentage <- 0

#Looped through each distribution
for (dist in distributions) {
  ### BIC Comparison ###
  #Filter BIC data for the current distribution
  dist_bic_data <- bic_data %>% filter(Distribution == dist)
  
  #Grouped by cryptocurrency and find BIC for each model
  for (crypto in unique(dist_bic_data$Crypto)) {
    crypto_data <- dist_bic_data %>% filter(Crypto == crypto)
    
    #Pairwise comparison of models based on BIC
    for (i in 1:nrow(bic_outperformance_comparison)) {
      model1 <- bic_outperformance_comparison$Model1[i]
      model2 <- bic_outperformance_comparison$Model2[i]
      
      if (model1 != model2) {
        bic1 <- crypto_data %>% filter(Model == model1) %>% pull(BIC)
        bic2 <- crypto_data %>% filter(Model == model2) %>% pull(BIC)
        
        #Increment count if model1 has a lower (better) BIC than model2
        if (length(bic1) > 0 && length(bic2) > 0 && bic1 < bic2) {
          bic_outperformance_comparison$Percentage[i] <- bic_outperformance_comparison$Percentage[i] + 1
        }
      }
    }
  }
  
  ### Log-Likelihood Comparison ###
  # Filter Log-Likelihood data for the current distribution
  dist_loglikelihood_data <- loglikelihood_data %>% filter(Distribution == dist)
  
  #Grouped by cryptocurrency and find Log-Likelihood for each model
  for (crypto in unique(dist_loglikelihood_data$Crypto)) {
    crypto_data <- dist_loglikelihood_data %>% filter(Crypto == crypto)
    
    #Pairwise comparison of models based on Log-Likelihood
    for (i in 1:nrow(loglikelihood_outperformance_comparison)) {
      model1 <- loglikelihood_outperformance_comparison$Model1[i]
      model2 <- loglikelihood_outperformance_comparison$Model2[i]
      
      if (model1 != model2) {
        loglikelihood1 <- crypto_data %>% filter(Model == model1) %>% pull(LogLikelihood)
        loglikelihood2 <- crypto_data %>% filter(Model == model2) %>% pull(LogLikelihood)
        
        #Increment count if model1 has a higher (better) Log-Likelihood than model2
        if (length(loglikelihood1) > 0 && length(loglikelihood2) > 0 && loglikelihood1 > loglikelihood2) {
          loglikelihood_outperformance_comparison$Percentage[i] <- loglikelihood_outperformance_comparison$Percentage[i] + 1
        }
      }
    }
  }
}

# Calculated the total number of comparisons for each Model1-Model2 pair
total_comparisons <- length(distributions) * length(unique(bic_data$Crypto))

# Converted counts to percentages for BIC
bic_outperformance_comparison$Percentage <- (bic_outperformance_comparison$Percentage / total_comparisons) * 100

# Displayed the pairwise outperformance comparison table in percentages for BIC
print("BIC Pairwise Outperformance Comparison in Percentages")
print(bic_outperformance_comparison)

# Converted counts to percentages for Log-Likelihood
loglikelihood_outperformance_comparison$Percentage <- (loglikelihood_outperformance_comparison$Percentage / total_comparisons) * 100

# Displayed the pairwise outperformance comparison table in percentages for Log-Likelihood
print("Log-Likelihood Pairwise Outperformance Comparison in Percentages")
print(loglikelihood_outperformance_comparison)


# Loaded necessary library
library(ggplot2)

# Function to create a heatmap for pairwise outperformance comparisons with the improvements
create_improved_heatmap <- function(data, metric) {
  # Replaced diagonal entries with a blank space or "Same Model"
  data <- data %>%
    mutate(Percentage = ifelse(Model1 == Model2, NA, Percentage))
  
  # Created the heatmap
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

# Created the heatmaps for each metric: AIC, BIC, and Log-Likelihood

# Heatmap for AIC pairwise outperformance
aic_heatmap <- create_improved_heatmap(outperformance_comparison, "AIC")
print(aic_heatmap)

# Heatmap for BIC pairwise outperformance
bic_heatmap <- create_improved_heatmap(bic_outperformance_comparison, "BIC")
print(bic_heatmap)

# Heatmap for Log-Likelihood pairwise outperformance
loglikelihood_heatmap <- create_improved_heatmap(loglikelihood_outperformance_comparison, "Log-Likelihood")
print(loglikelihood_heatmap)

#########################################################################################################
# Comparison AIC, BIC, log by distribution EGARCH
#########################################################################################################

#Defined distributions and filter only EGARCH model
distributions <- c("norm", "std", "ged", "sstd")
model <- "EGARCH(1,1)"

#Initialized a data frame to store counts of pairwise outperformance for each distribution
outperformance_distribution_comparison <- expand.grid(Distribution1 = distributions, Distribution2 = distributions)
outperformance_distribution_comparison$Percentage <- 0

#Filtered AIC data to include only EGARCH model
filtered_aic_data <- aic_data %>% filter(Model == model)

#Grouped by cryptocurrency and find AIC for each distribution within the EGARCH model
for (crypto in unique(filtered_aic_data$Crypto)) {
  crypto_data <- filtered_aic_data %>% filter(Crypto == crypto)
  
  # Rounded AIC values to the 5th decimal place for precise comparison
  crypto_data <- crypto_data %>% mutate(AIC = round(AIC, 5))
  
  # Pairwise comparison of distributions based on AIC for the current cryptocurrency
  for (i in 1:nrow(outperformance_distribution_comparison)) {
    dist1 <- outperformance_distribution_comparison$Distribution1[i]
    dist2 <- outperformance_distribution_comparison$Distribution2[i]
    
    if (dist1 != dist2) {
      aic1 <- crypto_data %>% filter(Distribution == dist1) %>% pull(AIC)
      aic2 <- crypto_data %>% filter(Distribution == dist2) %>% pull(AIC)
      
      # Increment count if dist1 has a lower (better) AIC than dist2
      if (length(aic1) > 0 && length(aic2) > 0 && aic1 < aic2) {
        outperformance_distribution_comparison$Percentage[i] <- outperformance_distribution_comparison$Percentage[i] + 1
      }
    }
  }
}

#Calculated the total number of comparisons for EGARCH model
total_comparisons <- length(unique(filtered_aic_data$Crypto))

#Converted counts to percentages
outperformance_distribution_comparison$Percentage <- (outperformance_distribution_comparison$Percentage / total_comparisons) * 100

#Replaced diagonal entries with "Same Distribution"
outperformance_distribution_comparison <- outperformance_distribution_comparison %>%
  mutate(Percentage = ifelse(Distribution1 == Distribution2, NA, Percentage))

# Created the heatmap for pairwise distribution comparison for EGARCH only
library(ggplot2)
ggplot(outperformance_distribution_comparison, aes(x = Distribution2, y = Distribution1, fill = Percentage)) +
  geom_tile(color = "white") +
  # Display percentage values in the cells, but leave diagonal entries blank
  geom_text(aes(label = ifelse(is.na(Percentage), "Same Dist.", round(Percentage, 1))), 
            color = "black", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
  labs(
    title = "Pairwise Distribution Outperformance based on AIC (EGARCH Only)",
    x = "Outperformed Distribution (Distribution2)",
    y = "Outperforming Distribution (Distribution1)",
    fill = "Outperformance (%)"
  ) +
  scale_x_discrete(labels = function(labels) toupper(labels)) +  # Uppercase!!!
  scale_y_discrete(labels = function(labels) toupper(labels)) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10),
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    plot.background = element_blank()  
  ) +
  coord_fixed() +  
  annotate(
    "rect", 
    xmin = 0.5, 
    xmax = nlevels(outperformance_distribution_comparison$Distribution2) + 0.5,
    ymin = 0.5, 
    ymax = nlevels(outperformance_distribution_comparison$Distribution1) + 0.5, 
    color = "black", 
    fill = NA, 
    size = 0.1  
  )

# Defined distributions and filter only EGARCH model
distributions <- c("norm", "std", "ged", "sstd")
model <- "EGARCH(1,1)"

# Initialized a dataframe to store counts of pairwise outperformance for each distribution
outperformance_distribution_comparison <- expand.grid(Distribution1 = distributions, Distribution2 = distributions)
outperformance_distribution_comparison$Percentage <- 0

# Filtered BIC data to include only EGARCH model
filtered_bic_data <- bic_data %>% filter(Model == model)

# Grouped by cryptocurrency and find BIC for each distribution within the EGARCH model
for (crypto in unique(filtered_bic_data$Crypto)) {
  crypto_data <- filtered_bic_data %>% filter(Crypto == crypto)
  
  # Rounded BIC values to the 5th decimal place for precise comparison
  crypto_data <- crypto_data %>% mutate(BIC = round(BIC, 5))
  
  # Pairwise comparison of distributions based on BIC for the current cryptocurrency
  for (i in 1:nrow(outperformance_distribution_comparison)) {
    dist1 <- outperformance_distribution_comparison$Distribution1[i]
    dist2 <- outperformance_distribution_comparison$Distribution2[i]
    
    if (dist1 != dist2) {
      bic1 <- crypto_data %>% filter(Distribution == dist1) %>% pull(BIC)
      bic2 <- crypto_data %>% filter(Distribution == dist2) %>% pull(BIC)
      
      # Increment count if dist1 has a lower (better) BIC than dist2
      if (length(bic1) > 0 && length(bic2) > 0 && bic1 < bic2) {
        outperformance_distribution_comparison$Percentage[i] <- outperformance_distribution_comparison$Percentage[i] + 1
      }
    }
  }
}

# Calculated the total number of comparisons for EGARCH model
total_comparisons <- length(unique(filtered_bic_data$Crypto))

# Converted counts to percentages
outperformance_distribution_comparison$Percentage <- (outperformance_distribution_comparison$Percentage / total_comparisons) * 100

# Replaced diagonal entries with "Same Distribution" or leave blank
outperformance_distribution_comparison <- outperformance_distribution_comparison %>%
  mutate(Percentage = ifelse(Distribution1 == Distribution2, NA, Percentage))

# Created the heatmap for pairwise distribution comparison for EGARCH based on BIC
library(ggplot2)
ggplot(outperformance_distribution_comparison, aes(x = Distribution2, y = Distribution1, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(is.na(Percentage), "Same Dist.", round(Percentage, 1))), 
            color = "black", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
  labs(
    title = "Pairwise Distribution Outperformance based on BIC (EGARCH Only)",
    x = "Outperformed Distribution (Distribution2)",
    y = "Outperforming Distribution (Distribution1)",
    fill = "Outperformance (%)"
  ) +
  scale_x_discrete(labels = function(labels) toupper(labels)) +  
  scale_y_discrete(labels = function(labels) toupper(labels)) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10),
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    plot.background = element_blank()  
  ) +
  coord_fixed() + 
  annotate(
    "rect", 
    xmin = 0.5, 
    xmax = nlevels(outperformance_distribution_comparison$Distribution2) + 0.5,
    ymin = 0.5, 
    ymax = nlevels(outperformance_distribution_comparison$Distribution1) + 0.5, 
    color = "black", 
    fill = NA, 
    size = 0.1
  )


# Defined distributions and filter only EGARCH model
distributions <- c("norm", "std", "ged", "sstd")
model <- "EGARCH(1,1)"

# Initialized a dataframe to store counts of pairwise outperformance for each distribution
outperformance_distribution_comparison <- expand.grid(Distribution1 = distributions, Distribution2 = distributions)
outperformance_distribution_comparison$Percentage <- 0

# Filtered Log-Likelihood data to include only EGARCH model
filtered_loglikelihood_data <- loglikelihood_data %>% filter(Model == model)

# Grouped by cryptocurrency and find Log-Likelihood for each distribution within the EGARCH model
for (crypto in unique(filtered_loglikelihood_data$Crypto)) {
  crypto_data <- filtered_loglikelihood_data %>% filter(Crypto == crypto)
  
  # Rounded Log-Likelihood values to the 5th decimal place for precise comparison
  crypto_data <- crypto_data %>% mutate(LogLikelihood = round(LogLikelihood, 5))
  
  # Pairwise comparison of distributions based on Log-Likelihood for the current cryptocurrency
  for (i in 1:nrow(outperformance_distribution_comparison)) {
    dist1 <- outperformance_distribution_comparison$Distribution1[i]
    dist2 <- outperformance_distribution_comparison$Distribution2[i]
    
    if (dist1 != dist2) {
      loglik1 <- crypto_data %>% filter(Distribution == dist1) %>% pull(LogLikelihood)
      loglik2 <- crypto_data %>% filter(Distribution == dist2) %>% pull(LogLikelihood)
      
      # Increment count if dist1 has a higher (better) Log-Likelihood than dist2
      if (length(loglik1) > 0 && length(loglik2) > 0 && loglik1 > loglik2) {
        outperformance_distribution_comparison$Percentage[i] <- outperformance_distribution_comparison$Percentage[i] + 1
      }
    }
  }
}

# Calculated the total number of comparisons for EGARCH model
total_comparisons <- length(unique(filtered_loglikelihood_data$Crypto))

# Converted counts to percentages
outperformance_distribution_comparison$Percentage <- (outperformance_distribution_comparison$Percentage / total_comparisons) * 100

# Replaced diagonal entries with "Same Distribution"
outperformance_distribution_comparison <- outperformance_distribution_comparison %>%
  mutate(Percentage = ifelse(Distribution1 == Distribution2, NA, Percentage))

# Created the heatmap for pairwise distribution comparison for EGARCH based on Log-Likelihood
library(ggplot2)
ggplot(outperformance_distribution_comparison, aes(x = Distribution2, y = Distribution1, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(is.na(Percentage), "Same Dist.", round(Percentage, 1))), 
            color = "black", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
  labs(
    title = "Pairwise Distribution Outperformance based on Log-Likelihood (EGARCH Only)",
    x = "Outperformed Distribution (Distribution2)",
    y = "Outperforming Distribution (Distribution1)",
    fill = "Outperformance (%)"
  ) +
  scale_x_discrete(labels = function(labels) toupper(labels)) +  
  scale_y_discrete(labels = function(labels) toupper(labels)) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10),
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    plot.background = element_blank()  
  ) +
  coord_fixed() +  
  annotate(
    "rect", 
    xmin = 0.5, 
    xmax = nlevels(outperformance_distribution_comparison$Distribution2) + 0.5,
    ymin = 0.5, 
    ymax = nlevels(outperformance_distribution_comparison$Distribution1) + 0.5, 
    color = "black", 
    fill = NA, 
    size = 0.1  
  )

#########################################################################################################
# Comparison AIC, BIC, log by distribution (all models) (Just out of interest)
#########################################################################################################
# Defined distributions and model names
distributions <- c("norm", "std", "ged", "sstd")
models <- c("GARCH(1,1)", "EGARCH(1,1)", "GJR-GARCH(1,1)", "Jump-GARCH(1,1)")

# Initialized a dataframe to store counts of pairwise outperformance for each distribution within each model
outperformance_distribution_comparison <- expand.grid(Distribution1 = distributions, Distribution2 = distributions, Model = models)
outperformance_distribution_comparison$Percentage <- 0

# Looped through each model to compare distributions
for (model in models) {
  # Filtered AIC data to the current model
  model_aic_data <- aic_data %>% filter(Model == model)
  
  # Grouped by cryptocurrency and find AIC for each distribution within the current model
  for (crypto in unique(model_aic_data$Crypto)) {
    crypto_data <- model_aic_data %>% filter(Crypto == crypto)
    
    # Pairwise comparison of distributions based on AIC for the current model and cryptocurrency
    for (i in 1:nrow(outperformance_distribution_comparison)) {
      dist1 <- outperformance_distribution_comparison$Distribution1[i]
      dist2 <- outperformance_distribution_comparison$Distribution2[i]
      model_in_row <- outperformance_distribution_comparison$Model[i]
      
      # Ensured we are comparing within the same model and excluding self-comparison
      if (dist1 != dist2 && model_in_row == model) {
        aic1 <- crypto_data %>% filter(Distribution == dist1) %>% pull(AIC)
        aic2 <- crypto_data %>% filter(Distribution == dist2) %>% pull(AIC)
        
        # Increment count if dist1 has a lower (better) AIC than dist2
        if (length(aic1) > 0 && length(aic2) > 0 && aic1 < aic2) {
          outperformance_distribution_comparison$Percentage[i] <- outperformance_distribution_comparison$Percentage[i] + 1
        }
      }
    }
  }
}

# Calculated the total number of comparisons for each distribution pair within each model
total_comparisons <- length(unique(aic_data$Crypto))

# Converted counts to percentages by dividing by the total comparisons within each model
outperformance_distribution_comparison <- outperformance_distribution_comparison %>%
  group_by(Model) %>%
  mutate(Percentage = (Percentage / total_comparisons) * 100) %>%
  ungroup()

# Replaced diagonal entries with "Same Distribution"
outperformance_distribution_comparison <- outperformance_distribution_comparison %>%
  mutate(Percentage = ifelse(Distribution1 == Distribution2, NA, Percentage))

# ggplot2

# Loaded necessary library
library(ggplot2)

# Created the heatmap for pairwise distribution comparison within each model
ggplot(outperformance_distribution_comparison, aes(x = Distribution2, y = Distribution1, fill = Percentage)) +
  geom_tile(color = "white") +
  # Displayed percentage values in the cells, but left diagonal entries blank
  geom_text(aes(label = ifelse(is.na(Percentage), "Same Dist.", round(Percentage, 1))), 
            color = "black", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
  labs(
    title = "Pairwise Distribution Outperformance by Model based on AIC",
    x = "Outperformed Distribution",
    y = "Outperforming Distribution",
    fill = "Outperformance (%)"
  ) +
  facet_wrap(~ Model) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10)
  )

#########################################################################################################
# Where are the best best models (just out of interest)
#########################################################################################################

# Initialized a data frame to store counts of each model's outperformance for each distribution metric
outperformance_counts <- data.frame(
  Model = c("GARCH(1,1)", "EGARCH(1,1)", "GJR-GARCH(1,1)", "Jump-GARCH(1,1)"),
  norm = 0,
  std = 0,
  ged = 0,
  sstd = 0
)

# 1. Calculated model outperformance by AIC for each distribution
for (dist in distributions) {
  # Filter AIC data for the current distribution
  dist_aic_data <- aic_data %>% filter(Distribution == dist)
  
  # Identified the model with the lowest AIC for each cryptocurrency in this distribution
  best_models <- dist_aic_data %>%
    group_by(Crypto) %>%
    filter(AIC == min(AIC)) %>%
    ungroup()
  
  # Counted occurrences of each model having the lowest AIC for this distribution
  model_counts <- best_models %>%
    group_by(Model) %>%
    summarize(count = n()) %>%
    ungroup()
  
  # Updated the main `outperformance_counts` dataframe with the counts for this distribution
  for (model in model_counts$Model) {
    outperformance_counts[outperformance_counts$Model == model, dist] <- model_counts$count[model_counts$Model == model]
  }
}

# Displayed the AIC outperformance counts
print(outperformance_counts)

# Loaded necessary library for plotting
library(ggplot2)

# Reshaped data for heatmap plotting
outperformance_counts_long <- outperformance_counts %>%
  pivot_longer(cols = -Model, names_to = "Distribution", values_to = "Count")

# Created a heatmap for AIC outperformance counts
ggplot(outperformance_counts_long, aes(x = Distribution, y = Model, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), color = "black", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Model Outperformance by Distribution (AIC Heatmap)",
    x = "Distribution",
    y = "Model",
    fill = "Outperformance Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# 2. Repeated similar steps to calculate outperformance by BIC for each distribution
for (dist in distributions) {
  # Filter BIC data for the current distribution
  dist_bic_data <- bic_data %>% filter(Distribution == dist)
  
  # Identified the model with the lowest BIC for each cryptocurrency in this distribution
  best_models <- dist_bic_data %>%
    group_by(Crypto) %>%
    filter(BIC == min(BIC)) %>%
    ungroup()
  
  # Counted occurrences of each model having the lowest BIC for this distribution
  model_counts <- best_models %>%
    group_by(Model) %>%
    summarize(count = n()) %>%
    ungroup()
  
  # Updated the main `outperformance_counts` dataframe with the counts for this distribution
  for (model in model_counts$Model) {
    outperformance_counts[outperformance_counts$Model == model, dist] <- model_counts$count[model_counts$Model == model]
  }
}

# Displayed the BIC outperformance counts
print(outperformance_counts)

# Reshaped data for BIC heatmap
outperformance_counts_long <- outperformance_counts %>%
  pivot_longer(cols = -Model, names_to = "Distribution", values_to = "Count")

# Created a heatmap for BIC outperformance counts
ggplot(outperformance_counts_long, aes(x = Distribution, y = Model, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), color = "black", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Model Outperformance by Distribution (BIC Heatmap)",
    x = "Distribution",
    y = "Model",
    fill = "Outperformance Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# 3. Calculated model outperformance by Log-Likelihood for each distribution
for (dist in distributions) {
  # Filter Log-Likelihood data for the current distribution
  dist_loglikelihood_data <- loglikelihood_data %>% filter(Distribution == dist)
  
  # Identified the model with the highest Log-Likelihood for each cryptocurrency in this distribution
  best_models <- dist_loglikelihood_data %>%
    group_by(Crypto) %>%
    filter(LogLikelihood == max(LogLikelihood)) %>%
    ungroup()
  
  # Counted occurrences of each model having the highest Log-Likelihood for this distribution
  model_counts <- best_models %>%
    group_by(Model) %>%
    summarize(count = n()) %>%
    ungroup()
  
  # Updated the main `outperformance_counts` dataframe with the counts for this distribution
  for (model in model_counts$Model) {
    outperformance_counts[outperformance_counts$Model == model, dist] <- model_counts$count[model_counts$Model == model]
  }
}

# Displayed the Log-Likelihood outperformance counts
print(outperformance_counts)

# Reshaped data for Log-Likelihood heatmap
outperformance_counts_long <- outperformance_counts %>%
  pivot_longer(cols = -Model, names_to = "Distribution", values_to = "Count")

# Created a heatmap for Log-Likelihood outperformance counts
ggplot(outperformance_counts_long, aes(x = Distribution, y = Model, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), color = "black", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Model Outperformance by Distribution (Log-Likelihood Heatmap)",
    x = "Distribution",
    y = "Model",
    fill = "Outperformance Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

