#######################Examples of ChatGPT-Coding-Prompts################
#During the writing of the thesis, ChatGPT (OpneAI, 2024/25) was used several times as a helper tool for coding.
#In the following, examples of such prompts will be provided
#ChatGPT was especially used for the ARJI-GARCH Model. Thus, more detailed prompt on this will be provided

#########################################################################################################
#Examples of Prompts on the Basis of Chapter 2
#########################################################################################################

############################Prompt-Category 1: Applying existing code on other data sources############
#Prompt: Apply this code to all cryptos. I listed them in this chat above:

BTC <- BTC %>%
  mutate(Date = as.Date(sub(" .*", "", Date))) %>%
  filter(Date >= as.Date("2020-04-11"))

View(BTC)

#Whereby ChatGPT (OpenAI, 2024) gave the answer:

#To apply this logic to all the cryptocurrencies you listed before (BTC, ETH, Tether, BNB, SOL, USDC, Ripple, DOGE, and TRON),
#you can modify the code for each dataset in the same manner.
#Here's the full code for all the datasets:

library(dplyr)

# BTC dataset
BTC <- BTC %>%
  mutate(Date = as.Date(sub(" .*", "", Date))) %>%
  filter(Date >= as.Date("2020-04-11"))
View(BTC)

# ETH dataset
ETH <- ETH %>%
  mutate(Date = as.Date(sub(" .*", "", Date))) %>%
  filter(Date >= as.Date("2020-04-11"))
View(ETH)

# Tether dataset
Tether <- Tether %>%
  mutate(Date = as.Date(sub(" .*", "", Date))) %>%
  filter(Date >= as.Date("2020-04-11"))
View(Tether)

# BNB dataset
BNB <- BNB %>%
  mutate(Date = as.Date(sub(" .*", "", Date))) %>%
  filter(Date >= as.Date("2020-04-11"))
View(BNB)

# SOL dataset
SOL <- SOL %>%
  mutate(Date = as.Date(sub(" .*", "", Date))) %>%
  filter(Date >= as.Date("2020-04-11"))
View(SOL)

# USDC dataset
USDC <- USDC %>%
  mutate(Date = as.Date(sub(" .*", "", Date))) %>%
  filter(Date >= as.Date("2020-04-11"))
View(USDC)

# Ripple dataset
Ripple <- Ripple %>%
  mutate(Date = as.Date(sub(" .*", "", Date))) %>%
  filter(Date >= as.Date("2020-04-11"))
View(Ripple)

# DOGE dataset
DOGE <- DOGE %>%
  mutate(Date = as.Date(sub(" .*", "", Date))) %>%
  filter(Date >= as.Date("2020-04-11"))
View(DOGE)

# TRON dataset
TRON <- TRON %>%
  mutate(Date = as.Date(sub(" .*", "", Date))) %>%
  filter(Date >= as.Date("2020-04-11"))
View(TRON)

############################Prompt-Category 2: Clarification Questions on how I should proceed/integrate something####################

#Prompt: I used this code to calculate descriptive stats:

library(moments)

# Function to calculate descriptive statistics

compute_descriptive_stats <- function(returns) {
  sumStat <- c(
    length(returns),            
    min(returns),               
    max(returns),               
    mean(returns),              
    median(returns),            
    sqrt(var(returns)),         
    skewness(returns),          
    kurtosis(returns)           
  )
  return(sumStat)
}

# Apply to BTC log returns (replace BTC_clean$Return with your log returns vector)
btc_stats <- compute_descriptive_stats(BTC_clean$Return)

#Is it possible that it will exclude observations, but include the Jarque–Bera? Or should I first calculate the Jarque–Bera seperately?"

#Answer: "To include the Jarque–Bera test statistic in your summary table, you’ll need to calculate it separately because it is a statistical test that checks whether the distribution of the returns is normally distributed. The test uses skewness and kurtosis to calculate the statistic.
#The jarque.test() function from the tseries package in R can be used for this. You can calculate the Jarque–Bera test for each cryptocurrency separately and then append its result to the summary table.

#Steps:
#1. Install and load the tseries package to calculate the Jarque–Bera test.
#2. Modify the compute_descriptive_stats function to include the Jarque–Bera test.
#3. Update the summary_table to include the Jarque–Bera statistic.

############################Prompt-Category 3: Correcting the original code####################

#What is wrong with the code?
ggplot(acf_data, aes(x = Lag)) +
  geom_point(aes(y = Returns, color = "Returns",shape = 16, size = 1.5), size = 2, alpha = 0.7) +  
  geom_point(aes(y = Squared_Returns, color = "Squared Returns", shape = 17, size = 1.5), size = 2, alpha = 0.4) +  
  geom_point(aes(y = Absolute_Returns, color = "Absolute Returns", shape = 18, size = 1.5), size = 2, alpha = 0.4) +  
  geom_line(aes(y = Upper_CI), linetype = "dotted", color = "gray") +  
  geom_line(aes(y = Lower_CI), linetype = "dotted", color = "gray") +  
  scale_x_log10(
    limits = c(1, 10000),  
    breaks = c(1, 60, 60*24, 60*24*7),  
    labels = c("1 min", "1 hour", "1 day", "1 week")  
  ) +
  labs(
    x = "Time [min]", 
    y = "Autocorrelation", 
    title = "Autocorrelation of Minute Returns, Squared Returns, and Absolute Returns",
    subtitle = "Minute-level data over 7 days"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right"
  ) +
  scale_color_manual(values = c("Returns" = "blue", "Squared Returns" = "orange", "Absolute Returns" = "green")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add a horizontal line at y = 0
  ylim(-0.1, 0.25) +  # Set the y-axis limits from -0.1 to 0.25
  annotate("text", x = 10, y = conf_level + 0.02, label = "95% Confidence Interval", color = "gray", size = 4, hjust = 0) +
  annotate("text", x = 10, y = -conf_level - 0.02, label = "95% Confidence Interval", color = "gray", size = 4, hjust = 0)""


#Answer:
#The issue in your code lies in the way you're specifying the shape and size within the aes() mapping.
#In aes(), you should map variables to aesthetics, but here, you're passing constant values (shape = 16 and size = 1.5).
#Instead, these should be passed outside of aes() because they are not variables but fixed properties.
#Here’s the corrected version of your code:

# Create the plot using ggplot2 with a logarithmic scale for the x-axis
ggplot(acf_data, aes(x = Lag)) +
  geom_point(aes(y = Returns, color = "Returns"), shape = 16, size = 2, alpha = 0.7) +  # Returns autocorrelation (r)
  geom_point(aes(y = Squared_Returns, color = "Squared Returns"), shape = 17, size = 2, alpha = 0.4) +  # Squared returns autocorrelation (r^2)
  geom_point(aes(y = Absolute_Returns, color = "Absolute Returns"), shape = 18, size = 2, alpha = 0.4) +  # Absolute returns autocorrelation (|r|)
  geom_line(aes(y = Upper_CI), linetype = "dotted", color = "gray") +  # Upper confidence interval
  geom_line(aes(y = Lower_CI), linetype = "dotted", color = "gray") +  # Lower confidence interval
  scale_x_log10(
    limits = c(1, 10000),  # Set x-axis limits for 1 minute to 10,000 minutes
    breaks = c(1, 60, 60*24, 60*24*7),  # Custom breaks for 1 minute, 1 hour, 1 day, and 1 week
    labels = c("1 min", "1 hour", "1 day", "1 week")  # Custom labels for time intervals
  ) +
  labs(
    x = "Time [min]", 
    y = "Autocorrelation", 
    title = "Autocorrelation of Minute Returns, Squared Returns, and Absolute Returns",
    subtitle = "Minute-level data over 7 days"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right"
  ) +
  scale_color_manual(values = c("Returns" = "blue", "Squared Returns" = "orange", "Absolute Returns" = "green")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add a horizontal line at y = 0
  ylim(-0.1, 0.25) +  # Set the y-axis limits from -0.1 to 0.25
  annotate("text", x = 10, y = conf_level + 0.02, label = "95% Confidence Interval", color = "gray", size = 4, hjust = 0) +
  annotate("text", x = 10, y = -conf_level - 0.02, label = "95% Confidence Interval", color = "gray", size = 4, hjust = 0)

#Explanation of Changes:
  #1. Shape and size:
  #The shape and size values should be outside the aes() function because they are constants, not mappings to variables. This is the main correction. For example, shape = 16 and size = 2 should be outside of aes().
  #aes(y = Returns, color = "Returns") handles mappings, while shape = 16 and size = 2 handle fixed aesthetics.
  #2. Transparency (alpha):
  #alpha = 0.7 is set outside the aes() to make the points slightly transparent for better visual clarity.

#Outcome:
  #This code will now correctly generate your plot with different point shapes for returns, squared returns, and absolute returns, with slight transparency and proper formatting.

#########################################################################################################
#ChatGPT provided a lot of assistance for the ARJI-GARCH Model. It helped to draft several versions of it.
#Thus, the main prompts used for the final version are provided below
#########################################################################################################

#The first last version, which was tediously coded, did not work in the end. Thus, ChatGPT coded the new last version of the model:

#Prompt: Somehow all the model fit values for ARJI-GARCH (code provided below) are the same for the distributions. I mean, AIC is the same for norm and ged. Did I do something wrong?
# Adding Jump-GARCH(1,1) with Different Distributions: Attention, takes really long to load!
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


# Summary of Jump-GARCH results
cat("\n\n=== Results for Jump-GARCH(1,1) Model ===\n")
for (dist in names(jump_garch_results)) {
  cat("\n--- Distribution:", dist, "---\n")
  
  for (crypto in names(jump_garch_results[[dist]])) {
    cat("\n", crypto, ":\n")
    print(jump_garch_results[[dist]][[crypto]])
  }
}?

  
#Answer: If the AIC and other metrics (e.g., BIC) are identical across distributions in your Jump-GARCH results, it suggests that the distributional specification in the model fitting is not actually being applied correctly. Here's a breakdown of potential issues and how to diagnose and fix them:
  
  #Possible Issues
#Distribution Not Applied in fit_arji:
  #In the fit_arji function, the variance and jump intensity are computed, but there is no explicit consideration of the distribution in the log-likelihood calculation.
#The function assumes a normal distribution (dnorm is used), so the specified distribution in the ugarchspec is not being reflected in the likelihood computation.
#Optimization Overwriting Parameters:
  #If the same initial parameters are used for all distributions, and the optimizer converges to the same solution, the metrics will be identical regardless of the specified distribution.
#Model Specification (ugarchspec) Ignored:
  #If the code for defining the distribution in ugarchspec does not affect the log-likelihood calculation (because you're not directly using rugarch's fitting and likelihood computation functions), the results won't differ across distributions.
#Error Handling Overwrites Results:
#If tryCatch or some other error-handling mechanism fails and silently returns a fallback result, this might cause identical output across all iterations.
  
  #Steps to Diagnose
#Verify fit_arji Considers the Distribution:
  #Check if fit_arji incorporates the specified distribution when calculating the likelihood. For example:
  #If you are using dnorm in fit_arji, replace it with the corresponding density function for the specified distribution (e.g., dged for Generalized Error Distribution).
#Print Intermediate Results:
  #Print the optimized parameters and intermediate values (e.g., h, lambda, likelihood) for each distribution to ensure the calculations differ.
#Compare Final Parameters Across Distributions:
  #After optimization, inspect the fitted parameter values (fit$par). If they are identical for different distributions, the issue is likely in the log-likelihood calculation or optimization process.
#Test ugarchfit Directly:
  #If you're using rugarch, ensure the fitting process (ugarchfit) respects the specified distribution. The ugarchfit function handles likelihood computation for the specified model and distribution.

  #Potential Fixes
#Update fit_arji to Incorporate Distribution:
  #Modify the log-likelihood calculation in fit_arji to use the specified distribution. For example:
  
  log_likelihood <- sum(
    switch(
      dist,
      "norm" = dnorm(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), log = TRUE),
      "ged" = dged(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), log = TRUE),
      "std" = dstd(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), log = TRUE)
    )
  )

#Ensure that dist is passed as a parameter to fit_arji.
#Use rugarch for Fitting:
  #If you're already defining the model using ugarchspec, rely on ugarchfit for fitting instead of implementing your own likelihood function. For example: 
  
jump_garch_fit <- ugarchfit(spec = jump_garch_spec, data = returns)


#Isolate the Problem:
  #Fit a single return series with different distributions and compare intermediate calculations manually to confirm differences in likelihood contributions.

#Key Adjustment in Your Workflow
#If you rely on rugarch for fitting:
  
  #Replace manual calls to optim with ugarchfit:

jump_garch_fit <- tryCatch({
  ugarchfit(spec = jump_garch_spec, data = returns)
}, error = function(e) {
  cat("Error fitting Jump-GARCH with", dist, "distribution:", e$message, "\n")
  NULL
})

#If you prefer custom likelihood fitting:
  
  #Pass dist to fit_arji and use appropriate density functions for log-likelihood.

#Outcome
#After making these changes:
  
  #You should observe distinct AIC, BIC, and log-likelihood values for different distributions.
#The optimized parameters should vary based on the chosen distribution.
#Let me know if you need help implementing these fixes!



###############New version still had issues. Conversation went on.#################


#Prompt: There were different loglikelihoods per distribution with  this code, but AIC and BIC always resulted in NA. Can we improve this?
  
  # Adding Jump-GARCH(1,1) with Different Distributions: Attention, takes really long to load!
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
    
    # Fitted the Jump-GARCH model using optimization
    jump_garch_fit <- tryCatch({
      ugarchfit(spec = jump_garch_spec, data = returns)
    }, error = function(e) {
      cat("Error fitting Jump-GARCH with", dist, "distribution:", e$message, "\n")
      NULL
    })
    
    if (!is.null(jump_garch_fit)) {
      # Extracted model fit metrics if fitting is successful
      log_likelihood <- likelihood(jump_garch_fit)
      num_obs <- length(returns)
      num_params <- length(coef(jump_garch_fit))
      AIC <- infocriteria(jump_garch_fit)["Akaike"]
      BIC <- infocriteria(jump_garch_fit)["Bayes"]
      
      list(parameters = coef(jump_garch_fit), log_likelihood = log_likelihood, AIC = AIC, BIC = BIC)
    } else {
      NULL
    }
  })
}

#Answer: The issue of AIC and BIC resulting in NA likely stems from the way the infocriteria function interacts with the fitted model object returned by ugarchfit. This could happen if:

#The fitting process didn't converge properly, leading to undefined information criteria.
#The infocriteria function encounters unexpected values in the model (e.g., missing log-likelihood or parameter count).
#Here are steps to address and improve your code:

# Adding Jump-GARCH(1,1) with Different Distributions
jump_garch_results <- list()
for (dist in distributions) {
  cat("\n### Applying Jump-GARCH(1,1) with", dist, "distribution ###\n")
  
  jump_garch_results[[dist]] <- lapply(crypto_returns, function(df) {
    returns <- df$Return
    
    # Define the Jump-GARCH model specification with the chosen distribution
    jump_garch_spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(0, 0)),
      distribution.model = dist
    )
    
    # Fit the Jump-GARCH model using optimization
    jump_garch_fit <- tryCatch({
      ugarchfit(spec = jump_garch_spec, data = returns)
    }, error = function(e) {
      cat("Error fitting Jump-GARCH with", dist, "distribution:", e$message, "\n")
      NULL
    })
    
    if (!is.null(jump_garch_fit)) {
      # Extract model fit metrics if fitting is successful
      log_likelihood <- likelihood(jump_garch_fit)
      num_obs <- length(returns)
      num_params <- length(coef(jump_garch_fit))
      
      # Manually calculate AIC and BIC if infocriteria fails
      AIC <- if (!is.na(log_likelihood) && num_params > 0) {
        -2 * log_likelihood + 2 * num_params
      } else {
        NA
      }
      
      BIC <- if (!is.na(log_likelihood) && num_params > 0) {
        -2 * log_likelihood + log(num_obs) * num_params
      } else {
        NA
      }
      
      list(parameters = coef(jump_garch_fit), log_likelihood = log_likelihood, AIC = AIC, BIC = BIC)
    } else {
      NULL
    }
  })
}

#Key Improvements
#Manually Compute AIC and BIC:
  #AIC and BIC are recalculated explicitly to avoid dependency on infocriteria. The formulas are:

#AIC=−2×Log-Likelihood+2×Number of Parameters
#BIC=−2×Log-Likelihood+log(Number of Observations)×Number of Parameters

#This ensures that AIC and BIC are valid as long as the log-likelihood and parameter count are non-NA.
#Added Conditional Checks:
  #Before calculating AIC and BIC, the code verifies:
  #Log-likelihood is not NA.
#The number of parameters is greater than zero.
#Improved Error Handling:
  #If fitting fails or convergence issues arise, the function gracefully handles errors and prevents invalid calculations.
#Log Progress:
  #The cat statements help track progress and debug issues for each cryptocurrency and distribution.

#Next Steps
#Verify Results:
  #After running the updated code, check:
  #Log-likelihood varies across distributions.
#AIC and BIC are now non-NA and differ between distributions.
#Inspect Non-Convergence Cases:
  #If AIC or BIC is still NA for some distributions or cryptocurrencies, inspect the corresponding models for:
  #Convergence warnings in ugarchfit.
#Data issues (e.g., outliers or missing values).
#Optimize Performance:
  #Running this for multiple distributions and cryptocurrencies is time-consuming. To optimize:
  #Use parallel processing (e.g., parallel or future.apply packages).
#Reduce the dataset size or the number of distributions to test during debugging.
#Let me know if further refinements or troubleshooting help is needed!

####################################Conversation went on, I did minor adjustments on the code, such as normalizing AIC and BIC, more prompts followed#####################################

#Prompt: We are getting closer: The Skewed and Student Skewed Dist do not seem to work, but Norm and GED do work: 
### Applying Jump-GARCH(1,1) with norm distribution ###
> # Looped over distributions
  > for (dist in distributions) {
    +   cat("\n### Applying Jump-GARCH(1,1) with", dist, "distribution ###\n")
    +   
      +   jump_garch_results[[dist]] <- lapply(crypto_returns, function(df) {
        +     returns <- df$Return
        +     
          +     # Defined initial parameters and bounds
          +     params_start <- c(omega = 0.1, alpha = 0.05, beta = 0.8, lambda0 = 0.05, rho = 0.5, gamma = 0.5)
          +     lower_bounds <- c(omega = 1e-6, alpha = 1e-6, beta = 1e-6, lambda0 = 1e-6, rho = 0, gamma = 1e-6)
          +     upper_bounds <- c(omega = 10, alpha = 1, beta = 1, lambda0 = 1, rho = 1, gamma = 10)
          +     
            +     # Defined the ARJI log-likelihood function
            +     fit_arji <- function(params, returns, dist) {
              +       omega <- params["omega"]
              +       alpha <- params["alpha"]
              +       beta <- params["beta"]
              +       lambda0 <- params["lambda0"]
              +       rho <- params["rho"]
              +       gamma <- params["gamma"]
              +       
                +       h <- numeric(length(returns))
                +       lambda <- numeric(length(returns))
                +       u <- returns
                +       
                  +       h[1] <- var(returns)
                  +       lambda[1] <- lambda0
                  +       
                    +       for (t in 2:length(returns)) {
                      +         h[t] <- pmax(omega + alpha * u[t - 1]^2 + beta * h[t - 1], 1e-6)
                      +         lambda[t] <- pmax(lambda0 + rho * lambda[t - 1] + gamma * u[t - 1]^2, 1e-6)
                      +       }
                  +       
                    +       # Log-likelihood calculation based on distribution
                    +       log_density <- switch(
                      +         dist,
                      +         "norm" = dnorm(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), log = TRUE),
                      +         "std" = sn::dst(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), nu = 5, log = TRUE),
                      +         "ged" = dged_custom(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), nu = 1.5, log = TRUE),
                      +         "sstd" = dsstd_custom(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), shape = 0, log = TRUE),
                      +         stop("Invalid distribution")
                      +       )
                    +       
                      +       log_likelihood <- sum(log_density)
                      +       return(-log_likelihood)
                      +     }
            +     
              +     # Optimized the Jump-GARCH model
              +     jump_garch_fit <- tryCatch({
                +       fit <- optim(
                  +         par = params_start,
                  +         fn = function(params) fit_arji(params, returns, dist),
                  +         method = "L-BFGS-B",
                  +         lower = lower_bounds,
                  +         upper = upper_bounds,
                  +         control = list(maxit = 1000)
                  +       )
                +       
                  +       # Calculated log-likelihood, AIC, and BIC
                  +       log_likelihood <- -fit_arji(fit$par, returns, dist)
                  +       num_obs <- length(returns)
                  +       num_params <- length(fit$par)
                  +       AIC <-  AIC <- (-2 * log_likelihood + 2 * num_params) / num_obs
                  +       BIC <- (-2 * log_likelihood + log(num_obs) * num_params) / num_obs
                  +       
                    +       list(parameters = fit$par, log_likelihood = log_likelihood, AIC = AIC, BIC = BIC)
                  +     }, error = function(e) {
                    +       cat("Error fitting Jump-GARCH with", dist, "distribution:", e$message, "\n")
                    +       NULL
                    +     })
              +     
                +     jump_garch_fit
              +   })
      + }

### Applied Jump-GARCH(1,1) with norm distribution ###

### Applied Jump-GARCH(1,1) with std distribution ###
Error fitting Jump-GARCH with std distribution: unused arguments (mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-06)) 
Error fitting Jump-GARCH with std distribution: unused arguments (mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-06)) 
Error fitting Jump-GARCH with std distribution: unused arguments (mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-06)) 
Error fitting Jump-GARCH with std distribution: unused arguments (mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-06)) 
Error fitting Jump-GARCH with std distribution: unused arguments (mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-06)) 
Error fitting Jump-GARCH with std distribution: unused arguments (mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-06)) 
Error fitting Jump-GARCH with std distribution: unused arguments (mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-06)) 
Error fitting Jump-GARCH with std distribution: unused arguments (mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-06)) 
Error fitting Jump-GARCH with std distribution: unused arguments (mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-06)) 

### Applied Jump-GARCH(1,1) with ged distribution ###

### Applied Jump-GARCH(1,1) with sstd distribution ###
Error fitting Jump-GARCH with sstd distribution: Skewed Student-t distribution requires a specific implementation. 
Error fitting Jump-GARCH with sstd distribution: Skewed Student-t distribution requires a specific implementation. 
Error fitting Jump-GARCH with sstd distribution: Skewed Student-t distribution requires a specific implementation. 
Error fitting Jump-GARCH with sstd distribution: Skewed Student-t distribution requires a specific implementation. 
Error fitting Jump-GARCH with sstd distribution: Skewed Student-t distribution requires a specific implementation. 
Error fitting Jump-GARCH with sstd distribution: Skewed Student-t distribution requires a specific implementation. 
Error fitting Jump-GARCH with sstd distribution: Skewed Student-t distribution requires a specific implementation. 
Error fitting Jump-GARCH with sstd distribution: Skewed Student-t distribution requires a specific implementation. 
Error fitting Jump-GARCH with sstd distribution: Skewed Student-t distribution requires a specific implementation.

#Answer: The issues with the Student-t (std) and Skewed Student-t (sstd) distributions arise because the corresponding density functions are not being handled correctly in your code. Let's address each distribution:

 #1. Issue with std Distribution
#Error:
unused arguments (mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-06))

#The function sn::dst (from the sn package) does not accept mean and sd as arguments. Instead, it expects:
  
  #x: The data.
#df: Degrees of freedom (equivalent to nu).
#log: Logical, whether to return the log of the density.
#Solution
#Replace the std distribution handling with the correct usage of sn::dst. Here's the updated line in the switch statement:

"std" = sn::dst(u, df = 5, log = TRUE),

#2. Issue with sstd Distribution
#Error:
  Skewed Student-t distribution requires a specific implementation.

#You currently do not have an implementation for the skewed Student-t distribution (sstd). This distribution requires a more complex density function involving skewness and degrees of freedom. The rugarch package handles this distribution internally during model fitting, but does not export a density function for external use.

#Solution

#You need a custom implementation for the skewed Student-t density. Alternatively, if you don’t have the resources to create one, you can:
  
  #Omit the sstd distribution from your tests for now.
#Implement the skewed Student-t later.
#To omit it, simply remove "sstd" from the distributions list:
distributions <- c("norm", "std", "ged")

#Updated Code
#Here’s the revised code snippet for the switch statement in the fit_arji function, addressing the issues:

log_density <- switch(
  dist,
  "norm" = dnorm(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), log = TRUE),
  "std" = sn::dst(u, df = 5, log = TRUE),
  "ged" = dged_custom(u, mean = 0, sd = sqrt(h + lambda * (u^2 + var(u)) + 1e-6), nu = 1.5, log = TRUE),
  "sstd" = stop("Skewed Student-t distribution requires a specific implementation."),
  stop("Invalid distribution")
)

#If you are ready to implement the sstd, let me know, and I can assist with that as well!

###################These kind of prompt went on, until ChatGPT coded a new ARJI-GARCH for me with some minor own adaptions on the distribution customs (see codes of Chapter 3 for final version)##########
