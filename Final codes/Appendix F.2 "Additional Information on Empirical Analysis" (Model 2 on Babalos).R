#########################################################################################################
# Cryptocurrency Volatility Modeling EGARCH
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
options(scipen = 999)  # Prevent scientific notation globally:)

#########################################################################################################
# Load and Prepare Data
#########################################################################################################

# Loaded datasets for selected cryptocurrencies
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

# Ensured price is numeric and filtered dates from 2017-12-18 onwards
crypto_datasets <- lapply(crypto_datasets, function(df) {
  df %>%
    mutate(
      Price = as.numeric(Price),
      Date = as.Date(sub(" .*", "", Date))
    ) %>%
    filter(Date >= as.Date("2017-12-18"))
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


# Loaded required libraries
library(quantmod)
library(dplyr)
library(rugarch)

# Downloaded GBTC data from Yahoo Finance
GBTC <- getSymbols("GBTC", auto.assign = FALSE, from = "2017-12-18", to = "2024-03-15", src = "yahoo")

# Converted to a data frame for easier manipulation
GBTC_df <- data.frame(Date = index(GBTC), coredata(GBTC))

# Cleaned the GBTC data: Ensured price is numeric and filtered dates from 2018-12-18 onwards
GBTC_cleaned <- GBTC_df %>%
  mutate(
    Price = as.numeric(GBTC.Adjusted), # Used the Adjusted Close as the price
    Date = as.Date(Date)
  ) %>%
  select(Date, Price) %>% # Keep only relevant columns
  filter(Date >= as.Date("2018-12-18"))

# Computed log returns for GBTC
GBTC_returns <- GBTC_cleaned %>%
  mutate(Return = c(NA, diff(log(Price)))) %>% # Log returns calculation
  filter(!is.na(Return)) # Removed NA values

# View cleaned data with returns:))
View(GBTC_returns)

################################################################################################
# Step 1: Compute ETFVOL_t
##############################

# Loaded libraries
library(rugarch)
library(dplyr)
library(lubridate)
library(zoo)  # For na.approx and na.locf functions

# Created Dummy_t for the event date
GBTC_returns <- GBTC_returns %>%
  mutate(
    Dummy_t = ifelse(Date >= as.Date("2024-01-11"), 1, 0)
  )

# Fitted GARCH(1,1) to GBTC returns
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "ged"
)
garch_fit <- ugarchfit(spec = garch_spec, data = GBTC_returns$Return)

# Added ETFVOL_t to GBTC_returns
GBTC_returns <- GBTC_returns %>%
  mutate(
    ETFVOL_t = as.numeric(sigma(garch_fit)) #GARCH-Based Volatility
  )

View(GBTC_returns)

#Bc. interesting to look at (but not in paper):
plot(GBTC_returns$Date, GBTC_returns$ETFVOL_t, type = "l", main = "GBTC Volatility (ETFVOL_t)", xlab = "Date", ylab = "Volatility")
##################################################################
################################################################################################
# Step 2: Add ETFVOL_t to Cryptocurrencies
##############################

library(dplyr)
library(zoo) # For na.approx and na.locf

# Added ETFVOL_t to each cryptocurrency dataset
crypto_returns <- lapply(crypto_returns, function(df) {
  df <- df %>%
    left_join(GBTC_returns %>% select(Date, ETFVOL_t), by = "Date") %>%
    mutate(
      # Handled missing ETFVOL_t values with interpolation and forward-fill
      ETFVOL_t = na.approx(ETFVOL_t, na.rm = FALSE), # Linear interpolation
      ETFVOL_t = ifelse(is.na(ETFVOL_t), na.locf(ETFVOL_t), ETFVOL_t) # Forward-fill fallback
    )
  return(df)
})

# Checked the structure of the first cryptocurrency dataset
str(crypto_returns[[1]]) #This is BTC. If I put in a two, I will see it for ETH etc.
View(crypto_returns[[1]])

# Verified that ETFVOL_t has been added and there are no missing values
summary(crypto_returns[[1]]$ETFVOL_t)

##################################################################
#################################################################################################
#Add the Dummy
#################################################################################################

# Defined the event date
event_date <- as.Date("2024-01-11")

# Added Dummy_t to each cryptocurrency dataset
crypto_returns <- lapply(crypto_returns, function(df) {
  df %>%
    mutate(Dummy_t = ifelse(Date >= event_date, 1, 0))
})

str(crypto_returns[[1]]) 
View(crypto_returns[[1]])

###############################################################################################
#Define the Model
###############################################################################################

# Specified the EGARCH(1,1)-X model
egarch_spec <- ugarchspec(
  variance.model = list(
    model = "eGARCH",            # EGARCH model
    garchOrder = c(1, 1),        # (p, q) = (1, 1)
    external.regressors = NULL  # Placeholder for external regressors
  ),
  mean.model = list(
    armaOrder = c(0, 0),         # ARMA(0,0) for the mean
    include.mean = TRUE,         # Included the mean term
    external.regressors = NULL  # Placeholder for external regressors
  ),
  distribution.model = "ged"    # GED distribution (or any other, if wished)
)

crypto_returns <- lapply(crypto_returns, function(df) {
  df %>%
    mutate(
      ETFVOL_Dummy = ETFVOL_t * Dummy_t # Interaction term
    )
})

str(crypto_returns[[1]]) 
View(crypto_returns[[1]])

# Calculated summary statistics
summary(crypto_returns[[1]]$Return)
summary(crypto_returns[[1]]$ETFVOL_t)
summary(crypto_returns[[1]]$ETFVOL_Dummy)

# Calculated standard deviations
sd(crypto_returns[[1]]$Return)
sd(crypto_returns[[1]]$ETFVOL_t)
sd(crypto_returns[[1]]$ETFVOL_Dummy)
#These differences show that the scales of the variables are not aligned.-> Scaling

####################################################################################################
#Apply EGARCH
###################################################################################################

crypto_returns <- lapply(crypto_returns, function(df) {
  df %>%
    mutate(
      ETFVOL_t = scale(ETFVOL_t),         # Standardized ETFVOL_t
      ETFVOL_Dummy = scale(ETFVOL_Dummy) # Standardized ETFVOL_Dummy
    )
})
crypto_returns <- lapply(crypto_returns, function(df) {
  df %>%
    mutate(
      ETFVOL_t = scale(ETFVOL_t),         # Standardized ETFVOL_t
      ETFVOL_Dummy = df$ETFVOL_t * df$Dummy_t  # Recomputed interaction term in original scale
    )
})


crypto_returns <- lapply(crypto_returns, function(df) {
  df %>%
    mutate(
      ETFVOL_t = scale(ETFVOL_t),         # Standardized ETFVOL_t
      ETFVOL_Dummy = ETFVOL_Dummy         # Kept ETFVOL_Dummy unscaled
    )
})


crypto_returns <- lapply(crypto_returns, function(df) {
  df %>%
    mutate(
      ETFVOL_Dummy = ETFVOL_t * Dummy_t # Interaction term
    )
})


str(crypto_returns[[1]]) 
View(crypto_returns[[1]])

library(rugarch)

# Looped through each dataset and fitted the EGARCH model
egarch_results <- lapply(crypto_returns, function(df) {
  # Defined the EGARCH specification with external regressors
  spec <- ugarchspec(
    variance.model = list(
      model = "eGARCH",                  # Exponential GARCH model
      garchOrder = c(1, 1),             # GARCH(1,1)
      external.regressors = as.matrix(df[, c("Dummy_t", "ETFVOL_t", "ETFVOL_Dummy")]) # Variance regressors
    ),
    mean.model = list(
      armaOrder = c(0, 0),              # No ARMA terms in the mean
      include.mean = TRUE,              # Included intercept
      external.regressors = as.matrix(df[, "Dummy_t", drop = FALSE]) # Dummy in the mean equation
    ),
    distribution.model = "ged"          # GED for residuals. One can play with distributions.
  )
  
  # Fitted the EGARCH model
  fit <- ugarchfit(spec = spec, data = df$Return)
  
  return(fit)
})


##############################################################################################################
#View results
###################

# Extracted coefficients for all models
egarch_coefficients <- lapply(egarch_results, function(fit) coef(fit))

# Example: Printed coefficients for the first cryptocurrency
print(egarch_coefficients[[1]])


lapply(egarch_results, function(fit) fit@fit$convergence) #If the convergence status is 0, the model converged properly.
lapply(crypto_returns, function(df) any(is.na(df$Return) | is.infinite(df$Return))) #If any datasets return TRUE, investigate further. Not the case here
lapply(egarch_results, function(fit) coef(fit)) #Extreme values in parameters?
lapply(egarch_results, function(fit) fit@fit$LLH)  # Extract log-likelihood values


# Computed normalized AIC
normalized_aic <- lapply(egarch_results, function(fit) {
  log_likelihood <- fit@fit$LLH       # Extracted log-likelihood
  num_params <- length(coef(fit))    # Number of estimated parameters
  num_obs <- length(fit@model$modeldata$data)  # Number of observations
  (-2 * log_likelihood / num_obs) + (2 * num_params / num_obs)  # Normalized AIC formula
})

# Assigned names and printed normalized AICs
names(normalized_aic) <- names(crypto_returns)
print("normalized_aic")
print(normalized_aic)

# Computed normalized BIC
normalized_bic <- lapply(egarch_results, function(fit) {
  log_likelihood <- fit@fit$LLH       # Extract log-likelihood
  num_params <- length(coef(fit))    # Number of estimated parameters
  num_obs <- length(fit@model$modeldata$data)  # Number of observations
  (-2 * log_likelihood / num_obs) + (log(num_obs) * num_params / num_obs)  # Normalized BIC formula
})


# Assigned names and printed normalized BICs
names(normalized_bic) <- names(crypto_returns)
print("Normalized BIC:")
print(normalized_bic)
##########################################################################################
################################################################################################
#Portmanteau
#############################################################################################

######################## 1. Box-Pierce statistic #########################

# Extracted standardized residuals
standardized_residuals_list <- lapply(egarch_results, function(fit) {
  residuals(fit, standardize = TRUE)
})

# Computed autocorrelations for |Z_t| and Z_t^2
autocorrelations <- lapply(standardized_residuals_list, function(residuals) {
  abs_residuals <- abs(residuals)         # Absolute residuals
  squared_residuals <- residuals^2       # Squared residuals
  
  # Computed autocorrelations for |Z_t| and Z_t^2
  autocorr_abs <- acf(abs_residuals, lag.max = 10, plot = FALSE)$acf[-1]  # Exclude lag 0
  autocorr_sq <- acf(squared_residuals, lag.max = 10, plot = FALSE)$acf[-1]
  
  list(autocorr_abs = autocorr_abs, autocorr_sq = autocorr_sq)
})

# Computed Box-Pierce statistics for each dataset
box_pierce_results <- mapply(function(residuals, acorrs) {
  n <- length(residuals)  # Sample size
  s <- 10  # Number of lags
  
  # Computed Box-Pierce statistics
  Q_abs <- n * sum(acorrs$autocorr_abs^2)
  Q_sq <- n * sum(acorrs$autocorr_sq^2)
  
  list(Q_abs = Q_abs, Q_sq = Q_sq)
}, residuals = standardized_residuals_list, acorrs = autocorrelations, SIMPLIFY = FALSE)

# Critical value for chi-squared distribution with 10 degrees of freedom
critical_value <- qchisq(0.95, df = 10)  # 95% confidence level

# Evaluated results
evaluated_results <- lapply(box_pierce_results, function(stats) {
  list(
    Q_abs = ifelse(stats$Q_abs > critical_value, "Reject H0", "Fail to reject H0"),
    Q_sq = ifelse(stats$Q_sq > critical_value, "Reject H0", "Fail to reject H0")
  )
})

# Assigned dataset names to results
names(evaluated_results) <- names(egarch_results)

# Printed results
print(evaluated_results)

# Computed Box-Pierce statistics and p-values
box_pierce_results_with_pvalues <- mapply(function(residuals, acorrs) {
  n <- length(residuals)  # Sample size
  s <- 10  # Number of lags
  
  # Computed Box-Pierce statistics
  Q_abs <- n * sum(acorrs$autocorr_abs^2)
  Q_sq <- n * sum(acorrs$autocorr_sq^2)
  
  # Computed p-values
  p_value_abs <- 1 - pchisq(Q_abs, df = s)  # For Q_abs
  p_value_sq <- 1 - pchisq(Q_sq, df = s)    # For Q_sq
  
  list(
    Q_abs = Q_abs,
    Q_sq = Q_sq,
    p_value_abs = p_value_abs,
    p_value_sq = p_value_sq
  )
}, residuals = standardized_residuals_list, acorrs = autocorrelations, SIMPLIFY = FALSE)

# Evaluated test results based on p-values
evaluated_results_with_pvalues <- lapply(box_pierce_results_with_pvalues, function(stats) {
  list(
    Q_abs = ifelse(stats$p_value_abs < 0.05, "Reject H0", "Fail to reject H0"),
    Q_sq = ifelse(stats$p_value_sq < 0.05, "Reject H0", "Fail to reject H0"),
    p_value_abs = stats$p_value_abs,
    p_value_sq = stats$p_value_sq
  )
})

# Assigned dataset names to results
names(evaluated_results_with_pvalues) <- names(egarch_results)

# Printed results
print(evaluated_results_with_pvalues)
################################################
######################## 2. Ljung-Box Test #########################

# Extracted standardized residuals
standardized_residuals_list <- lapply(egarch_results, function(fit) {
  residuals(fit, standardize = TRUE)  # Standardized residuals
})

# Computed autocorrelations for |Z_t| and Z_t^2
autocorrelations <- lapply(standardized_residuals_list, function(residuals) {
  abs_residuals <- abs(residuals)         # Absolute residuals
  squared_residuals <- residuals^2       # Squared residuals
  
  # Computed autocorrelations for |Z_t| and Z_t^2
  autocorr_abs <- acf(abs_residuals, lag.max = 10, plot = FALSE)$acf[-1]  # Exclude lag 0
  autocorr_sq <- acf(squared_residuals, lag.max = 10, plot = FALSE)$acf[-1]
  
  list(autocorr_abs = autocorr_abs, autocorr_sq = autocorr_sq)
})

# Computed Ljung-Box statistics for each dataset
ljung_box_results <- mapply(function(residuals, acorrs) {
  n <- length(residuals)  # Sample size
  s <- 10  # Number of lags
  
  # Computed Ljung-Box statistics
  Q_abs <- n * (n + 2) * sum(acorrs$autocorr_abs^2 / (n - seq_len(s)))
  Q_sq <- n * (n + 2) * sum(acorrs$autocorr_sq^2 / (n - seq_len(s)))
  
  # Computed p-values
  p_value_abs <- 1 - pchisq(Q_abs, df = s)  # For Q_abs
  p_value_sq <- 1 - pchisq(Q_sq, df = s)    # For Q_sq
  
  list(
    Q_abs = Q_abs,
    Q_sq = Q_sq,
    p_value_abs = p_value_abs,
    p_value_sq = p_value_sq
  )
}, residuals = standardized_residuals_list, acorrs = autocorrelations, SIMPLIFY = FALSE)

# Evaluated test results based on p-values
evaluated_ljung_box_results <- lapply(ljung_box_results, function(stats) {
  list(
    Q_abs = ifelse(stats$p_value_abs < 0.05, "Reject H0", "Fail to reject H0"),
    Q_sq = ifelse(stats$p_value_sq < 0.05, "Reject H0", "Fail to reject H0"),
    p_value_abs = stats$p_value_abs,
    p_value_sq = stats$p_value_sq
  )
})

# Assigned dataset names to results
names(evaluated_ljung_box_results) <- names(egarch_results)

# Printed results
cat("\n######### Ljung-Box Test Results #########\n")
print(evaluated_ljung_box_results)
################################################
######################## 3. Adjusted Box-Pierce Statistics standardized residuals##############

# Extracted standardized residuals
standardized_residuals_list <- lapply(egarch_results, function(fit) {
  residuals(fit, standardize = TRUE)
})

adjusted_box_pierce <- function(residuals, m) {
  n <- length(residuals)  # Sample size
  
  # Computed autocorrelations for lags up to m
  autocorr <- acf(residuals, lag.max = m, plot = FALSE)$acf[-1]  # Exclude lag 0
  
  # Original Box-Pierce statistic
  Q_BP <- n * sum(autocorr^2)
  
  # Adjusted Box-Pierce statistic
  E_QBP <- m                  # Mean of Q_BP
  Var_QBP <- 2 * m            # Variance of Q_BP
  Q_BP_adj <- m + sqrt(2 * m / Var_QBP) * (Q_BP - E_QBP)
  
  # Computed p-value
  p_value <- 1 - pchisq(Q_BP_adj, df = m)
  
  return(list(Q_BP = Q_BP, Q_BP_adj = Q_BP_adj, p_value = p_value))
}


# Applied Adjusted Box-Pierce test to squared and absolute residuals
adjusted_results <- lapply(standardized_residuals_list, function(residuals) {
  abs_residuals <- abs(residuals)         # Absolute residuals
  squared_residuals <- residuals^2       # Squared residuals
  
  # Computed Adjusted Box-Pierce statistics for both cases
  list(
    abs_residuals = adjusted_box_pierce(abs_residuals, m = 10),  # Example: 10 lags
    squared_residuals = adjusted_box_pierce(squared_residuals, m = 10)
  )
})

# Assigned dataset names to results
names(adjusted_results) <- names(egarch_results)

# Printed results
print(adjusted_results)


# Added decisions to results for convenience:)
adjusted_results <- lapply(adjusted_results, function(dataset_results) {
  abs_residuals <- dataset_results$abs_residuals
  squared_residuals <- dataset_results$squared_residuals
  
  # Added decision based on p-value
  abs_residuals$decision <- ifelse(
    abs_residuals$p_value < 0.05,
    "Reject H0: Significant autocorrelation",
    "Fail to reject H0: No significant autocorrelation"
  )
  
  squared_residuals$decision <- ifelse(
    squared_residuals$p_value < 0.05,
    "Reject H0: Significant autocorrelation",
    "Fail to reject H0: No significant autocorrelation"
  )
  
  # Updated results
  list(abs_residuals = abs_residuals, squared_residuals = squared_residuals)
})

# Printed results with decisions
cat("\n######### Adjusted Box-Pierce Test: STANDARDIZED Residuals with Decisions #########\n")
for (dataset in names(adjusted_results)) {
  cat("\nDataset:", dataset, "\n")
  
  abs_results <- adjusted_results[[dataset]]$abs_residuals
  squared_results <- adjusted_results[[dataset]]$squared_residuals
  
  cat("Absolute Residuals:\n")
  cat("  Q_BP:", abs_results$Q_BP, "\n")
  cat("  Q_BP_adj:", abs_results$Q_BP_adj, "\n")
  cat("  p-value:", abs_results$p_value, "\n")
  cat("  Decision:", abs_results$decision, "\n\n")
  
  cat("Squared Residuals:\n")
  cat("  Q_BP:", squared_results$Q_BP, "\n")
  cat("  Q_BP_adj:", squared_results$Q_BP_adj, "\n")
  cat("  p-value:", squared_results$p_value, "\n")
  cat("  Decision:", squared_results$decision, "\n")
}


library(kableExtra)

# Prepared the data for the table
adjusted_box_pierce_table <- data.frame(
  Cryptocurrency = names(adjusted_results),
  `Q^2(10)` = sapply(adjusted_results, function(dataset) {
    squared_results <- dataset$squared_residuals
    paste0(round(squared_results$Q_BP, 3), " (", format(round(squared_results$p_value, 3), nsmall = 3), ")")
  }),
  `|Q|(10)` = sapply(adjusted_results, function(dataset) {
    abs_results <- dataset$abs_residuals
    paste0(round(abs_results$Q_BP, 3), " (", format(round(abs_results$p_value, 3), nsmall = 3), ")")
  }),
  `Decision (Q^2)` = sapply(adjusted_results, function(dataset) {
    dataset$squared_residuals$decision
  }),
  `Decision (|Q|)` = sapply(adjusted_results, function(dataset) {
    dataset$abs_residuals$decision
  }),
  check.names = FALSE  # Prevented R from transforming special characters in column names
)

# Rendered the table using kable
kable(adjusted_box_pierce_table, "html", caption = "Adjusted Box-Pierce Test Results for Standardized Residuals", row.names = FALSE) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = TRUE) %>%  # Highlighted cryptocurrency names
  add_header_above(c(" " = 1, "Statistics" = 2, "Decisions" = 2))  # Adjusted header

library(kableExtra)
library(webshot2)  # For converting HTML to PNG

# Prepared the table (assuming `adjusted_box_pierce_table` is already defined)
kable_output <- kable(adjusted_box_pierce_table, "html", 
                      caption = "Adjusted Box-Pierce Test Results for Standardized Residuals", 
                      row.names = FALSE) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = TRUE) %>%  # Highlighted cryptocurrency names
  add_header_above(c(" " = 1, "Statistics" = 2, "Decisions" = 2))  # Adjusted header

# Saved table as an HTML file
html_file <- "adjusted_box_pierce_table.html"
save_kable(kable_output, file = html_file)

# Converted HTML file to PNG
png_file <- "adjusted_box_pierce_table.png"
webshot2::webshot(url = html_file, file = png_file, vwidth = 1200, vheight = 800)

cat("HTML saved as:", html_file, "\nPNG saved as:", png_file, "\n")

########################
####################################################################################################
#Check
###################################################################################################
print(egarch_coefficients[[1]])  # Looked for all expected coefficients.

df <- crypto_returns[[1]]  # Extracted the first data frame
str(df)

summary(df$Dummy_t)
summary(df$ETFVOL_t)
summary(df$ETFVOL_Dummy)

sd(df$Dummy_t)
sd(df$ETFVOL_t)
sd(df$ETFVOL_Dummy)

############################################################################################################################################
# Great, now we now that the model mostly suits (stablecoins not 100%). Now create a table for the results of the EGARCH Model on the data.
###########################################################################################################################################

# Computed normalized AIC
normalized_aic <- lapply(egarch_results, function(fit) {
  log_likelihood <- fit@fit$LLH       # Extracted log-likelihood
  num_params <- length(coef(fit))    # Number of estimated parameters
  num_obs <- length(fit@model$modeldata$data)  # Number of observations
  (-2 * log_likelihood / num_obs) + (2 * num_params / num_obs)  # Normalized AIC formula
})
normalized_aic <- unlist(normalized_aic)

# Computed normalized BIC
normalized_bic <- lapply(egarch_results, function(fit) {
  log_likelihood <- fit@fit$LLH       # Extracted log-likelihood
  num_params <- length(coef(fit))    # Number of estimated parameters
  num_obs <- length(fit@model$modeldata$data)  # Number of observations
  (-2 * log_likelihood / num_obs) + (log(num_obs) * num_params / num_obs)  # Normalized BIC formula
})
normalized_bic <- unlist(normalized_bic)

# Extracted coefficients for all models
egarch_coefficients <- lapply(egarch_results, function(fit) coef(fit))

# Extracted log-likelihood for all models
log_likelihoods <- sapply(egarch_results, function(fit) fit@fit$LLH)

# Helper function to add stars
add_stars <- function(coef, pval) {
  if (pval <= 0.01) {
    return(paste0(round(coef, 4), "***"))
  } else if (pval <= 0.05) {
    return(paste0(round(coef, 4), "**"))
  } else if (pval <= 0.1) {
    return(paste0(round(coef, 4), "*"))
  } else {
    return(round(coef, 4))
  }
}

# Extracted p-values and add stars to coefficients
egarch_coefficients_with_stars <- lapply(egarch_results, function(fit) {
  coef <- coef(fit)
  pvals <- fit@fit$matcoef[, 4] # 4th column contains p-values
  mapply(add_stars, coef, pvals)
})

# Combined coefficients into a data frame
crypto_names <- names(crypto_datasets)  # Names of cryptocurrencies
table_data <- do.call(cbind, egarch_coefficients_with_stars)  # Combined coefficients
rownames(table_data) <- names(egarch_coefficients[[1]])       # Row names for coefficients
table_data <- as.data.frame(table_data)                       # Converted to data frame
colnames(table_data) <- crypto_names                          # Set cryptocurrency names

# Added log-likelihood, AIC, and BIC rows
table_data["Log-Likelihood", ] <- round(log_likelihoods, 2)
table_data["AIC", ] <- round(normalized_aic, 4)
table_data["BIC", ] <- round(normalized_bic, 4)

# Created a styled HTML table
library(kableExtra)

kable(table_data, "html", caption = "EGARCH Model Results for Cryptocurrencies") %>%
  kable_styling(full_width = F) %>%
  add_header_above(c(" " = 1, "Cryptocurrencies" = length(crypto_names))) %>%
  pack_rows("Mean Equation", 1, 2, bold = T, italic = T) %>%
  pack_rows("Variance Equation", 3, 9, bold = T, italic = T) %>%
  pack_rows("Residual Distribution", 10, 10, bold = T, italic = T) %>%
  pack_rows("Model Fit Metrics", 11, 13, bold = T, italic = T)

##################################################################################################################
#More beautiful: Haha, again, actually Overleaf was used in the end
###################################################################################################

install.packages("kableExtra")
install.packages("webshot2") # For converting HTML to PNG
webshot::install_phantomjs() # Required for rendering HTML to PNG

# Renamed specific row names
rownames(table_data)[rownames(table_data) == "mu"] <- "µ"
rownames(table_data)[rownames(table_data) == "mxreg1"] <- "φ1"
rownames(table_data)[rownames(table_data) == "omega"] <- "α0"
rownames(table_data)[rownames(table_data) == "alpha1"] <- "α1"
rownames(table_data)[rownames(table_data) == "beta1"] <- "β1"
rownames(table_data)[rownames(table_data) == "gamma1"] <- "θ"
rownames(table_data)[rownames(table_data) == "vxreg1"] <- "τ1"
rownames(table_data)[rownames(table_data) == "vxreg2"] <- "τ2"
rownames(table_data)[rownames(table_data) == "vxreg3"] <- "τ3"
rownames(table_data)[rownames(table_data) == "shape"] <- "shape (ν)"

# Adjusted row groupings to separate "shape" and fit metrics
kable(table_data, "html", caption = "EGARCH Model Results for Cryptocurrencies") %>%
  kable_styling(full_width = F) %>%
  add_header_above(c(" " = 1, "Cryptocurrencies" = length(crypto_names))) %>%
  pack_rows("Mean Equation", 1, 2, bold = T, italic = T) %>%
  pack_rows("Variance Equation", 3, 9, bold = T, italic = T) %>% # Variance Equation ends at row 7
  pack_rows("Residual Distribution", 10, 10, bold = T, italic = T) %>% # "shape" row
  pack_rows("Model Fit Metrics", 11, 13, bold = T, italic = T) # Log-Likelihood, AIC, and BIC

# Created the table with proper grouping
html_table <- kable(
  table_data,
  "html",
  caption = "EGARCH Model Results for Cryptocurrencies with ETFVOL"
) %>%
  kable_styling(full_width = F) %>%
  add_header_above(c(" " = 1, "Cryptocurrencies" = length(crypto_names))) %>%
  pack_rows("Mean Equation", 1, 2, bold = T, italic = T) %>%
  pack_rows("Variance Equation", 3, 9, bold = T, italic = T) %>%
  pack_rows("Residual Distribution", 10, 10, bold = T, italic = T) %>%
  pack_rows("Model Fit Metrics", 11, 13, bold = T, italic = T)

# Saved as HTML
html_file <- "egarch_table_ETFVOL.html"
save_kable(html_table, file = html_file)

# Converted HTML to PNG
png_file <- "egarch_table_ETFVOL.png"
webshot2::webshot(html_file, file = png_file, vwidth = 1200, vheight = 800)

cat("HTML saved as:", html_file, "\nPNG saved as:", png_file, "\n")

####################################################################################################
#Unconditional variance
####################################################################################################

#########Remember: Specify GED!!

# Extracted parameters
parameters <- coef(egarch_results[[8]])  # Replaced with the index of the model you want
alpha_0 <- parameters["omega"]
cat("alpha_0:", alpha_0, "\n")
beta <- parameters["beta1"]
cat("beta:", beta, "\n")
alpha_1 <- parameters["alpha1"]
cat("alpha_1:", alpha_1, "\n")
theta <- parameters["gamma1"]  # Assuming gamma1 represents theta
cat("theta:", theta, "\n")
zeta <- 1  # Replaced with the actual coefficient for zeta if applicable
cat("zeta:", zeta, "\n")
tau_2 <- parameters["vxreg2"]
cat("tau_2:", tau_2, "\n")
tau_1 <- parameters["vxreg1"]
cat("tau_1:", tau_1, "\n")
tau_3 <- parameters["vxreg3"]
cat("tau_3:", tau_3, "\n")

# Generated Z_t following a standardized GED
library(rugarch)
install.packages("fGarch")
library(fGarch)


# Specified GED parameters
shape_param <- parameters["shape"]  # Shape parameter (nu > 1 for GED)
cat(shape_param)
n_samples <- length(residuals(egarch_results[[1]], standardize = TRUE))  # Use the same length as residuals

# Generated Z_t samples from GED
Z_t <- rged(n = n_samples, nu = shape_param)  # Use `nu` instead of `shape`

# Standardized Z_t to mean 0 and variance 1
Z_t <- scale(Z_t)
cat("Z_t (standardized GED):", head(Z_t), "\n")


# Defined g(Z_t)
expected_abs_Z_t <- function(shape_param) {
  2^(1 / shape_param) * gamma(1 / shape_param) / sqrt(gamma(3 / shape_param))
}
E_abs_Z_t <- expected_abs_Z_t(shape_param)

g_function <- function(Z_t, theta, zeta, E_abs_Z_t) {
  theta * Z_t + zeta * (abs(Z_t) - E_abs_Z_t)
}
g_Z_t <- g_function(Z_t, theta, zeta, E_abs_Z_t)
cat("g_Z_t:", head(g_Z_t), "\n")

# Computed the expectation for each term
compute_expectation <- function(m, g_Z_t) {
  mean(exp(m * g_Z_t))  # Calculate the expectation as the sample mean
}

# Computed the infinite product
compute_infinite_product <- function(alpha_1, beta, g_Z_t, tol = 1e-6, max_iter = 1000) {
  product <- 1
  prev_term <- 0
  for (i in 1:max_iter) {
    m <- beta^(i - 1) * alpha_1
    term <- compute_expectation(m, g_Z_t)
    product <- product * term^2
    if (abs(term - prev_term) < tol) break  # Check for convergence
    prev_term <- term
  }
  return(product)
}

infinite_product <- compute_infinite_product(alpha_1, beta, g_Z_t)
cat("Infinite Product:", infinite_product, "\n")

# Computed the conditional variances
unconditional_variance <- exp((alpha_0) / (1 - beta)) * infinite_product
cat("Unonditional Variance:", unconditional_variance, "\n")


unconditional_variance_ETF <- exp((alpha_0+tau_1)/ (1 - beta)) * infinite_product
cat("Unonditional Variance after ETF:", unconditional_variance_ETF, "\n")


#WITH ETF::

# Extracted parameters
parameters <- coef(egarch_results[[5]])  # Replace with the index of the currency you want
alpha_0 <- parameters["omega"]
cat("alpha_0:", alpha_0, "\n")
beta <- parameters["beta1"]
cat("beta:", beta, "\n")
alpha_1 <- parameters["alpha1"]
cat("alpha_1:", alpha_1, "\n")
theta <- parameters["gamma1"]  # Assuming gamma1 represents theta
cat("theta:", theta, "\n")
zeta <- 1  # Replaced with the actual coefficient for zeta if applicable
cat("zeta:", zeta, "\n")
tau_2 <- parameters["vxreg2"]
cat("tau_2:", tau_2, "\n")
tau_1 <- parameters["vxreg1"]
cat("tau_1:", tau_1, "\n")
tau_3 <- parameters["vxreg3"]
cat("tau_3:", tau_3, "\n")

summary(df$ETFVOL_t)  # Checked the basic statistics of ETFVOL_t

E_ETFVOL <- mean(abs(df$ETFVOL_t), na.rm = TRUE)
cat(E_ETFVOL)

# Generated Z_t following a standardized GED
library(rugarch)
install.packages("fGarch")
library(fGarch)


# Specified GED parameters
shape_param <- parameters["shape"]  # Shape parameter (nu > 1 for GED)
cat(shape_param)
n_samples <- length(residuals(egarch_results[[1]], standardize = TRUE))  # Used the same length as residuals

# Generated Z_t samples from GED
Z_t <- rged(n = n_samples, nu = shape_param)  # Use `nu` instead of `shape`

# Standardized Z_t to mean 0 and variance 1
Z_t <- scale(Z_t)
cat("Z_t (standardized GED):", head(Z_t), "\n")


# Defined g(Z_t)
expected_abs_Z_t <- function(shape_param) {
  2^(1 / shape_param) * gamma(1 / shape_param) / sqrt(gamma(3 / shape_param))
}
E_abs_Z_t <- expected_abs_Z_t(shape_param)

g_function <- function(Z_t, theta, zeta, E_abs_Z_t) {
  theta * Z_t + zeta * (abs(Z_t) - E_abs_Z_t)
}
g_Z_t <- g_function(Z_t, theta, zeta, E_abs_Z_t)
cat("g_Z_t:", head(g_Z_t), "\n")

# Computed the expectation for each term
compute_expectation <- function(m, g_Z_t) {
  mean(exp(m * g_Z_t))  # Calculate the expectation as the sample mean
}

# Computed the infinite product
compute_infinite_product <- function(alpha_1, beta, g_Z_t, tol = 1e-6, max_iter = 1000) {
  product <- 1
  prev_term <- 0
  for (i in 1:max_iter) {
    m <- beta^(i - 1) * alpha_1
    term <- compute_expectation(m, g_Z_t)
    product <- product * term^2
    if (abs(term - prev_term) < tol) break  # Check for convergence
    prev_term <- term
  }
  return(product)
}

infinite_product <- compute_infinite_product(alpha_1, beta, g_Z_t)
cat("Infinite Product:", infinite_product, "\n")

# Computed the unconditional variances
unconditional_variance <- exp((alpha_0 + tau_2*E_ETFVOL) / (1 - beta)) * infinite_product
cat("Conditional Variance:", unconditional_variance, "\n")

unconditional_variance_ETF <- exp((alpha_0+tau_2*E_ETFVOL+tau_1)/ (1 - beta)) * infinite_product
cat("Unonditional Variance ETF:", unconditional_variance_ETF, "\n")

#Second regression now interaction
library(car)
vif(lm(Return ~ Dummy_t + ETFVOL_t + ETFVOL_t:Dummy_t, data = df))
library(car)
vif(lm(Return ~ Dummy_t + ETFVOL_t, data = df))

###########Loop#################

# Loaded necessary libraries
library(rugarch)
library(fGarch)
library(car)

# Defined required functions
expected_abs_Z_t <- function(shape_param) {
  2^(1 / shape_param) * gamma(1 / shape_param) / sqrt(gamma(3 / shape_param))
}

g_function <- function(Z_t, theta, zeta, E_abs_Z_t) {
  theta * Z_t + zeta * (abs(Z_t) - E_abs_Z_t)
}

compute_expectation <- function(m, g_Z_t) {
  mean(exp(m * g_Z_t))  # Calculated the expectation as the sample mean
}

compute_infinite_product <- function(alpha_1, beta, g_Z_t, tol = 1e-6, max_iter = 1000) {
  product <- 1
  prev_term <- 0
  for (i in 1:max_iter) {
    m <- beta^(i - 1) * alpha_1
    term <- compute_expectation(m, g_Z_t)
    product <- product * term^2
    if (abs(term - prev_term) < tol) break  # Checked for convergence
    prev_term <- term
  }
  return(product)
}

# Initialized a list to store results
results <- list()

# Looped through currencies 1–9
for (i in 1:9) {
  cat("\n--- Processing Currency", i, "---\n")
  
  # Extracted model and data for the current currency
  parameters <- coef(egarch_results[[i]])
  df <- crypto_returns[[i]]
  
  # Extracted parameters
  alpha_0 <- parameters["omega"]
  beta <- parameters["beta1"]
  alpha_1 <- parameters["alpha1"]
  theta <- parameters["gamma1"]  # Assuming gamma1 represents theta
  zeta <- 1  # Replaced with actual coefficient for zeta if applicable
  tau_2 <- parameters["vxreg2"]
  tau_1 <- parameters["vxreg1"]
  tau_3 <- parameters["vxreg3"]
  shape_param <- parameters["shape"]
  
  # Checked ETFVOL_t statistics and compute its mean
  summary(df$ETFVOL_t)  # Basic stats of ETFVOL_t
  E_ETFVOL <- mean(abs(df$ETFVOL_t), na.rm = TRUE)  # Expected value of ETFVOL_t
  cat("E_ETFVOL:", E_ETFVOL, "\n")
  
  # Generated Z_t from standardized GED
  n_samples <- length(residuals(egarch_results[[i]], standardize = TRUE))
  Z_t <- rged(n = n_samples, nu = shape_param)  # Generate GED samples
  Z_t <- scale(Z_t)  # Standardize to mean 0 and variance 1
  
  # Computed g(Z_t)
  E_abs_Z_t <- expected_abs_Z_t(shape_param)
  g_Z_t <- g_function(Z_t, theta, zeta, E_abs_Z_t)
  
  # Computed the infinite product
  infinite_product <- compute_infinite_product(alpha_1, beta, g_Z_t)
  
  # Computed unconditional variances
  unconditional_variance <- exp((alpha_0 + tau_2 * E_ETFVOL) / (1 - beta)) * infinite_product
  unconditional_variance_ETF <- exp((alpha_0 + tau_2 * E_ETFVOL + tau_1) / (1 - beta)) * infinite_product
  
  # Stored results for the current currency
  results[[i]] <- list(
    currency_index = i,
    unconditional_variance = unconditional_variance,
    unconditional_variance_ETF = unconditional_variance_ETF
  )
  
  # Printed results for the current currency
  cat("Unconditional Variance (Before ETF):", unconditional_variance, "\n")
  cat("Unconditional Variance (After ETF):", unconditional_variance_ETF, "\n")
}



# Looped through currencies 1–9
for (i in 1:9) {
  cat("\n--- Processing Currency", i, "---\n")
  
  # Extracted model and data for the current currency
  parameters <- coef(egarch_results[[i]])
  df <- crypto_returns[[i]]
  
  # Extracted parameters
  alpha_0 <- parameters["omega"]
  beta <- parameters["beta1"]
  alpha_1 <- parameters["alpha1"]
  theta <- parameters["gamma1"]  # Assuming gamma1 represents theta
  zeta <- 1  # Replace with actual coefficient for zeta if applicable
  tau_2 <- parameters["vxreg2"]
  tau_1 <- parameters["vxreg1"]
  tau_3 <- parameters["vxreg3"]
  shape_param <- parameters["shape"]
  
  # Checked ETFVOL_t statistics and compute its mean
  summary(df$ETFVOL_t)  # Basic stats of ETFVOL_t
  E_ETFVOL <- mean(abs(df$ETFVOL_t), na.rm = TRUE)  # Expected value of ETFVOL_t
  cat("E_ETFVOL:", E_ETFVOL, "\n")
  
  # Generated Z_t from standardized GED
  n_samples <- length(residuals(egarch_results[[i]], standardize = TRUE))
  Z_t <- rged(n = n_samples, nu = shape_param)  # Generate GED samples
  Z_t <- scale(Z_t)  # Standardize to mean 0 and variance 1
  
  # Computed g(Z_t)
  E_abs_Z_t <- expected_abs_Z_t(shape_param)
  g_Z_t <- g_function(Z_t, theta, zeta, E_abs_Z_t)
  
  # Computed the infinite product
  infinite_product <- compute_infinite_product(alpha_1, beta, g_Z_t)
  
  # Computed unconditional variances
  unconditional_variance <- exp((alpha_0 + tau_2 * E_ETFVOL) / (1 - beta)) * infinite_product
  unconditional_variance_ETF <- exp((alpha_0 + tau_2 * E_ETFVOL + tau_1) / (1 - beta)) * infinite_product
  
  # Computed standard deviations
  unconditional_sd <- sqrt(unconditional_variance)
  unconditional_sd_ETF <- sqrt(unconditional_variance_ETF)
  
  # Stored results for the current currency
  results[[i]] <- list(
    currency_index = i,
    unconditional_variance = unconditional_variance,
    unconditional_variance_ETF = unconditional_variance_ETF,
    unconditional_sd = unconditional_sd,
    unconditional_sd_ETF = unconditional_sd_ETF
  )
  
  # Printed results for the current currency
  cat("Unconditional Variance (Before ETF):", unconditional_variance, "\n")
  cat("Unconditional SD (Before ETF):", unconditional_sd, "\n")
  cat("Unconditional Variance (After ETF):", unconditional_variance_ETF, "\n")
  cat("Unconditional SD (After ETF):", unconditional_sd_ETF, "\n")
}


################################################End################################################
