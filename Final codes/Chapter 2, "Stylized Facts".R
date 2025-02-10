
#########################################################################################################
#### Stylized facts: Data Selection
#########################################################################################################

install.packages("tidyverse")
install.packages("coinmarketcapr")

library(tidyverse)
library(dplyr)
library(coinmarketcapr)

coinmarketcapr::setup(api_key = "ba13896e-dcb2-4bd8-9797-3d3fa27cc4bb")
crypto_market <- get_crypto_listings(currency = "USD", latest = TRUE) %>%
  arrange(desc(USD_market_cap))
View(crypto_market)

top10_crypto <- crypto_market %>% 
  arrange(desc(USD_market_cap)) %>%  # Just sorted by USD_market_cap in descending order...
  slice(1:10) %>%  # ... to select the first 10 rows
  select(name, symbol, date_added, USD_market_cap, USD_market_cap_dominance) %>% 
  rename(market_share = USD_market_cap_dominance) %>% 
  mutate(
    rank = rank(-USD_market_cap),  # ... and computeed rank based on market cap
    cumulative_market_share = cumsum(market_share)  # Finally, calculated cumulative market share
  ) %>% 
  select(rank, everything())

#Format
top10_crypto$date_added <- as.Date(top10_crypto$date_added)
top10_crypto$USD_market_cap <- formatC(top10_crypto$USD_market_cap, format = "e", digits = 2)

# Rename columns
colnames(top10_crypto)[colnames(top10_crypto) == "rank"] <- "Rank"
colnames(top10_crypto)[colnames(top10_crypto) == "name"] <- "Name"
colnames(top10_crypto)[colnames(top10_crypto) == "symbol"] <- "Symbol"
colnames(top10_crypto)[colnames(top10_crypto) == "date_added"] <- "Date Added"
colnames(top10_crypto)[colnames(top10_crypto) == "USD_market_cap"] <- "Market Cap (USD)"
colnames(top10_crypto)[colnames(top10_crypto) == "market_share"] <- "Market Share (%)"
colnames(top10_crypto)[colnames(top10_crypto) == "cumulative_market_share"] <- "Cumulative Market Share (%)"
top10_crypto <- top10_crypto %>%
  mutate(Name = ifelse(Symbol == "XRP", "Ripple", Name))
top10_crypto <- top10_crypto %>%
  mutate(Name = ifelse(Symbol == "BNB", "Binance Coin", Name))
top10_crypto <- top10_crypto %>%
  mutate(Name = ifelse(Symbol == "USDC", "USD Coin", Name))
# View our data frame
View(top10_crypto)

#Created nice table and graph for paper (hehe, in the end, I just used overleaf for table, but whatever)

# Table

install.packages("webshot")
install.packages("htmltools")
install.packages("stargazer")
install.packages("kableExtra")

library(stargazer)
library(webshot)
webshot::install_phantomjs()
library(kableExtra)

# HTML table 
stargazer(
  top10_crypto,
  type = "html",
  out = "table1.html",
  title = "Top 10 Cryptocurrencies by Market Share",
  summary = FALSE,  # No summary statistics
  rownames = FALSE, # No row numbers
  digits = 2,       # Two decimal places for numeric values
  style = "default",  # Default white background
  column.sep.width = "2pt"  # Added more space between columns
)

webshot("table1.html", "table1.png")

# Turned the HTML table in an nicer table
top10_crypto %>%
  kable("html", align = "c", col.names = colnames(top10_crypto)) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1:7, border_left = TRUE, border_right = TRUE) %>%
  row_spec(10, extra_css = "border-bottom: 1px solid #ddd;;") %>%  # Add a bottom border to the last row (rank 10)
  save_kable("table_with_lines1.html")
webshot("table_with_lines1.html", "table_with_lines1.png")

# Crossed out 9th row, as this currency does not cover wished period
# Recalculated cumulative market share while excluding the 9th row
top10_crypto <- top10_crypto %>%
  mutate(
    `Cumulative Market Share (%)` = cumsum(ifelse(Rank == 9, 0, as.numeric(`Market Share (%)`)))  # Set market share for row 9 to 0
  )

# Manually adjusted the last cumulative market share row to reflect the sum without row 9
top10_crypto$`Cumulative Market Share (%)`[nrow(top10_crypto)] <- 
  top10_crypto$`Cumulative Market Share (%)`[nrow(top10_crypto) - 1] + top10_crypto$`Market Share (%)`[nrow(top10_crypto)]

top10_crypto <- top10_crypto %>%
  mutate_if(is.numeric, ~ round(., 2))  # Round all numeric columns to 2 decimal places


# Modified the table to cross out the ninth row and apply cumulative market share calculation
top10_crypto %>%
  kable("html", align = "c", col.names = colnames(top10_crypto)) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1:ncol(top10_crypto), border_left = TRUE, border_right = TRUE) %>%
  row_spec(9, extra_css = "text-decoration: line-through;") %>%  # Cross out the ninth row
  row_spec(nrow(top10_crypto), extra_css = "border-bottom: 1px solid #ddd;") %>%  # Lighter bottom border for last row
  save_kable("table_with_cross.html")

# Converted the table to an image using webshot
webshot("table_with_cross.html", "table_with_cross.png")

#Remark: All tables created in R were, in the end, not used in the paper. The graphs were replicated in Overleaf.

# And noooow, simsalabum, created corresponding graph, so cool:)

install.packages("ggplot2")
install.packages("scales")
library(ggplot2)
library(scales)

coinmarketcapr::setup(api_key = "ba13896e-dcb2-4bd8-9797-3d3fa27cc4bb")
crypto_market <- get_crypto_listings(currency = "USD", latest = TRUE) %>%
  arrange(desc(USD_market_cap))

top10_crypto <- crypto_market %>% 
  arrange(desc(USD_market_cap)) %>%  #  Just sorted by USD_market_cap in descending order...
  slice(1:10) %>%  # ... to select the first 10 rows
  select(name, symbol, date_added, USD_market_cap, USD_market_cap_dominance) %>% 
  rename(market_share = USD_market_cap_dominance) %>% 
  mutate(
    rank = rank(-USD_market_cap),  # ... and to compute rank based on market cap
    cumulative_market_share = cumsum(market_share)  # Finally, calculated cumulative market share
  ) %>% 
  select(rank, everything())

ggplot(top10_crypto, aes(x = reorder(symbol, -market_share))) +
  geom_bar(aes(y = market_share), stat = "identity", fill = "cornflowerblue") +  # Bar plot for market share
  geom_point(aes(y = ifelse(rank %in% c(1, 4, 7, 10),
                            cumulative_market_share * max(market_share) / 100, NA)),
             color = "navy", size = 3) +  # Line plot points for cumulative market share
  geom_line(aes(y = cumulative_market_share * max(market_share) / 100, group = 1), color = "navy", size = 1) +  # Line for cumulative market share
  scale_y_continuous(
    name = "Market Share (%)", 
    sec.axis = sec_axis(~ . * 100 / max(top10_crypto$market_share), name = "Cumulative Market Share (%)")  # Secondary y-axis
  ) +
  geom_text(aes(y = ifelse(rank %in% c(1, 4, 7, 10),
                           cumulative_market_share * max(market_share) / 100, NA),
                label = ifelse(rank %in% c(1, 4, 7, 10), 
                               scales::percent(cumulative_market_share / 100), "")),
            color = "black", vjust = -0.5, size = 2.5) +  # Labels for cumulative market share
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.title.y.left = element_text(color = "cornflowerblue"),
    axis.title.y.right = element_text(color = "navy")
  ) +
  labs(
    title = "Cryptocurrency Market Share and Cumulative Market Share",
    x = "Cryptocurrency",
    y = "Market Share (%)"
  )

#########################################################################################################
### Stylized facts:Summary Statistics
#########################################################################################################

# Install necessary packages
install.packages("moments")
install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")

library(dplyr)
library(moments)  # For skewness, kurtosis, Jarque-Bera test
library(knitr)
library(kableExtra)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
#########################################################################################################
#Downloaded historical data of https://tokeninsight.com/en/cryptocurrencies
#########################################################################################################
#Loaded all datasets
BTC <- read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/BTC.csv")
View(BTC)
ETH <- read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/ETH.csv")
View(ETH)
Tether <- read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/Tether.csv")
View(Tether)
BNB <- read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/BNB.csv")
View(BNB)
SOL <- read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/SOL.csv")
View(SOL)
USDC <- read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/USDC.csv")
View(USDC)
Ripple <- read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/Ripple.csv")
View(Ripple)
DOGE <- read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/DOGE.csv")
View(DOGE)
TRON <- read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/TRON.csv")
View(TRON)
#########################################################################################################
# Start on 11th of April, 2020
#########################################################################################################

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
#########################################################################################################
# Log returns (and corresponding graph)
#########################################################################################################
library(ggplot2)

#BTC (Did the same for all other currencies)
# Computed log returns for BTC
BTC <- BTC %>%
  mutate(Return = c(NA, diff(log(Price))))  # Computed log returns, adding NA for the first observation

# Removed NA values from the Return column
BTC_clean <- BTC %>%
  filter(!is.na(Return))

# Ploted the returns using ggplot2
ggplot(BTC_clean, aes(x = Date, y = Return)) +
  geom_line() +
  labs(title = "BTC", 
       subtitle = "Log Returns", 
       x = "Time", 
       y = "Log Returns") +
  theme(panel.grid.minor = element_blank())
View(BTC)

# ETH
ETH <- ETH %>%
  mutate(Return = c(NA, diff(log(Price)))) %>%
  filter(!is.na(Return))

ETH_clean <- ETH %>%
  filter(!is.na(Return))

ggplot(ETH_clean, aes(x = Date, y = Return)) +
  geom_line() +
  labs(title = "ETH", 
       subtitle = "Log Returns", 
       x = "Time", 
       y = "Log Returns") +
  theme(panel.grid.minor = element_blank())

# Tether
Tether <- Tether %>%
  mutate(Return = c(NA, diff(log(Price)))) %>%
  filter(!is.na(Return))

Tether_clean <- Tether %>%
  filter(!is.na(Return))

ggplot(Tether_clean, aes(x = Date, y = Return)) +
  geom_line() +
  labs(title = "Tether", 
       subtitle = "Log Returns", 
       x = "Time", 
       y = "Log Returns") +
  theme(panel.grid.minor = element_blank())

# BNB
BNB <- BNB %>%
  mutate(Return = c(NA, diff(log(Price)))) %>%
  filter(!is.na(Return))

BNB_clean <- BNB %>%
  filter(!is.na(Return))

ggplot(BNB_clean, aes(x = Date, y = Return)) +
  geom_line() +
  labs(title = "BNB", 
       subtitle = "Log Returns", 
       x = "Time", 
       y = "Log Returns") +
  theme(panel.grid.minor = element_blank())

# SOL
SOL <- SOL %>%
  mutate(Return = c(NA, diff(log(Price)))) %>%
  filter(!is.na(Return))

SOL_clean <- SOL %>%
  filter(!is.na(Return))

ggplot(SOL_clean, aes(x = Date, y = Return)) +
  geom_line() +
  labs(title = "SOL", 
       subtitle = "Log Returns", 
       x = "Time", 
       y = "Log Returns") +
  theme(panel.grid.minor = element_blank())

# USDC
USDC <- USDC %>%
  mutate(Return = c(NA, diff(log(Price)))) %>%
  filter(!is.na(Return))

USDC_clean <- USDC %>%
  filter(!is.na(Return))

ggplot(USDC_clean, aes(x = Date, y = Return)) +
  geom_line() +
  labs(title = "USDC", 
       subtitle = "Log Returns", 
       x = "Time", 
       y = "Log Returns") +
  theme(panel.grid.minor = element_blank())

# Ripple
Ripple <- Ripple %>%
  mutate(Return = c(NA, diff(log(Price)))) %>%
  filter(!is.na(Return))

Ripple_clean <- Ripple %>%
  filter(!is.na(Return))

ggplot(Ripple_clean, aes(x = Date, y = Return)) +
  geom_line() +
  labs(title = "Ripple", 
       subtitle = "Log Returns", 
       x = "Time", 
       y = "Log Returns") +
  theme(panel.grid.minor = element_blank())

# DOGE
DOGE <- DOGE %>%
  mutate(Return = c(NA, diff(log(Price)))) %>%
  filter(!is.na(Return))

DOGE_clean <- DOGE %>%
  filter(!is.na(Return))

ggplot(DOGE_clean, aes(x = Date, y = Return)) +
  geom_line() +
  labs(title = "DOGE", 
       subtitle = "Log Returns", 
       x = "Time", 
       y = "Log Returns") +
  theme(panel.grid.minor = element_blank())

# TRON
TRON <- TRON %>%
  mutate(Return = c(NA, diff(log(Price)))) %>%
  filter(!is.na(Return))

TRON_clean <- TRON %>%
  filter(!is.na(Return))

ggplot(TRON_clean, aes(x = Date, y = Return)) +
  geom_line() +
  labs(title = "TRON", 
       subtitle = "Log Returns", 
       x = "Time", 
       y = "Log Returns") +
  theme(panel.grid.minor = element_blank())

# Set up a 3x3 plotting grid for the 9 cryptocurrencies
par(mfrow = c(3, 3))  # Created a 3x3 layout for all the following plots

# Plot BTC returns
plot(BTC_clean$Date, BTC_clean$Return, type = "l", col = "cornflowerblue", 
     main = "BTC Log Returns", xlab = "Date", ylab = "Log Returns")

# Plot ETH returns
plot(ETH_clean$Date, ETH_clean$Return, type = "l", col = "cornflowerblue", 
     main = "ETH Log Returns", xlab = "Date", ylab = "Log Returns")

# Plot Tether returns
plot(Tether_clean$Date, Tether_clean$Return, type = "l", col = "cornflowerblue", 
     main = "Tether Log Returns", xlab = "Date", ylab = "Log Returns")

# Plot BNB returns
plot(BNB_clean$Date, BNB_clean$Return, type = "l", col = "cornflowerblue", 
     main = "BNB Log Returns", xlab = "Date", ylab = "Log Returns")

# Plot SOL returns
plot(SOL_clean$Date, SOL_clean$Return, type = "l", col = "cornflowerblue", 
     main = "SOL Log Returns", xlab = "Date", ylab = "Log Returns")

# Plot USDC returns
plot(USDC_clean$Date, USDC_clean$Return, type = "l", col = "cornflowerblue", 
     main = "USDC Log Returns", xlab = "Date", ylab = "Log Returns")

# Plot Ripple returns
plot(Ripple_clean$Date, Ripple_clean$Return, type = "l", col = "cornflowerblue", 
     main = "Ripple Log Returns", xlab = "Date", ylab = "Log Returns")

# Plot DOGE returns
plot(DOGE_clean$Date, DOGE_clean$Return, type = "l", col = "cornflowerblue", 
     main = "DOGE Log Returns", xlab = "Date", ylab = "Log Returns")

# Plot TRON returns
plot(TRON_clean$Date, TRON_clean$Return, type = "l", col = "cornflowerblue", 
     main = "TRON Log Returns", xlab = "Date", ylab = "Log Returns")

# Reset to default plotting parameters
par(mfrow = c(1, 1))


#########################################################################################################
#  Compute descriptive statistics on ret (Jarque Bera included):
#########################################################################################################
# Installed the required package

install.packages("tseries")
library(moments)
library(tseries)

#Jarque Bera

jarque.bera.test(BTC_clean$Return)
jarque.bera.test(ETH_clean$Return)
jarque.bera.test(Tether_clean$Return)
jarque.bera.test(BNB_clean$Return)
jarque.bera.test(SOL_clean$Return)
jarque.bera.test(USDC_clean$Return)
jarque.bera.test(Ripple_clean$Return)
jarque.bera.test(DOGE_clean$Return)
jarque.bera.test(TRON_clean$Return)

# Function to calculate descriptive statistics, including Jarque-Bera test
compute_descriptive_stats <- function(returns) {
  sumStat <- c(
    min(returns),               # Minimum
    max(returns),               # Maximum
    mean(returns),              # Mean
    median(returns),            # Median
    sqrt(var(returns)),         # Standard deviation (SD)
    skewness(returns),          # Skewness
    kurtosis(returns),          # Kurtosis (not excess)
    jarque.bera.test(returns)$statistic  # Jarque-Bera statistic
  )
  return(sumStat)
}

# Applied to BTC log returns (replace `BTC_clean$Return` with your log returns vector)
btc_stats <- compute_descriptive_stats(BTC_clean$Return)

# Extended this to other cryptocurrencies as follows:
eth_stats <- compute_descriptive_stats(ETH_clean$Return)
tether_stats <- compute_descriptive_stats(Tether_clean$Return)
bnb_stats <- compute_descriptive_stats(BNB_clean$Return)
sol_stats <- compute_descriptive_stats(SOL_clean$Return)
usdc_stats <- compute_descriptive_stats(USDC_clean$Return)
ripple_stats <- compute_descriptive_stats(Ripple_clean$Return)
doge_stats <- compute_descriptive_stats(DOGE_clean$Return)
tron_stats <- compute_descriptive_stats(TRON_clean$Return)

# Combined the statistics into a data frame for easier comparison
summary_table <- data.frame(
  Symbol = c("BTC", "ETH", "Tether", "BNB", "SOL", "USDC", "Ripple", "DOGE", "TRON"),
  Mean = c(btc_stats[3], eth_stats[3], tether_stats[3], bnb_stats[3], sol_stats[3], usdc_stats[3], ripple_stats[3], doge_stats[3], tron_stats[3]),
  Median = c(btc_stats[4], eth_stats[4], tether_stats[4], bnb_stats[4], sol_stats[4], usdc_stats[4], ripple_stats[4], doge_stats[4], tron_stats[4]),
  SD = c(btc_stats[5], eth_stats[5], tether_stats[5], bnb_stats[5], sol_stats[5], usdc_stats[5], ripple_stats[5], doge_stats[5], tron_stats[5]),
  Max = c(btc_stats[2], eth_stats[2], tether_stats[2], bnb_stats[2], sol_stats[2], usdc_stats[2], ripple_stats[2], doge_stats[2], tron_stats[2]),
  Min = c(btc_stats[1], eth_stats[1], tether_stats[1], bnb_stats[1], sol_stats[1], usdc_stats[1], ripple_stats[1], doge_stats[1], tron_stats[1]),
  Skewness = c(btc_stats[6], eth_stats[6], tether_stats[6], bnb_stats[6], sol_stats[6], usdc_stats[6], ripple_stats[6], doge_stats[6], tron_stats[6]),
  Kurtosis = c(btc_stats[7], eth_stats[7], tether_stats[7], bnb_stats[7], sol_stats[7], usdc_stats[7], ripple_stats[7], doge_stats[7], tron_stats[7]),
  Jarque_Bera = c(btc_stats[8], eth_stats[8], tether_stats[8], bnb_stats[8], sol_stats[8], usdc_stats[8], ripple_stats[8], doge_stats[8], tron_stats[8])
)

# Display the summary table
View(summary_table)

#NICEEE. Now created table
install.packages("webshot2")

library(knitr)
library(kableExtra)
library(webshot2)  # This is needed for saving the table as an image
# 1. Rounded the columns: Rounded most to 4 decimal places and Jarque-Bera to 2 decimal places
summary_table <- summary_table %>%
  mutate_at(vars(Mean, Median, SD, Max, Min, Skewness, Kurtosis), ~ round(., 4)) %>%
  mutate(Jarque_Bera = round(Jarque_Bera, 2))

# 2. Added a star (*) to the Jarque-Bera column
summary_table <- summary_table %>%
  mutate(Jarque_Bera = paste0(Jarque_Bera, "*"))

# 3. Generated and styled the table, then saved it as an HTML file:)
summary_table %>%
  kable("html", align = "c", col.names = colnames(summary_table)) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1:9, border_left = TRUE, border_right = TRUE) %>%
  row_spec(nrow(summary_table), extra_css = "border-bottom: 1px solid #ddd;") %>%
  footnote(general = "*The p-value < 2.2e-16 indicates statistical significance at a level much stricter than 1%.", general_title = "") %>%
  save_kable("summary_stats.html")

# 4. Captured the table as an image
webshot("summary_stats.html", "summary_stats.png")

#Remark: Again, table was replicated in Overleaf. The one in R was thus not used.

#########################################################################################################
#  Visualizing Non-Normality
#########################################################################################################
# List of cryptocurrency returns
crypto_returns <- list(
  BTC = BTC_clean$Return,
  ETH = ETH_clean$Return,
  Tether = Tether_clean$Return,
  BNB = BNB_clean$Return,
  SOL = SOL_clean$Return,
  USDC = USDC_clean$Return,
  Ripple = Ripple_clean$Return,
  DOGE = DOGE_clean$Return,
  TRON = TRON_clean$Return
)

# Plot of histograms with overlaid normal distribution curves for each cryptocurrency
par(mfrow = c(3, 3))  # Created a 3x3 plot grid to display all plots on one page (again)

for (crypto in names(crypto_returns)) {
  ret <- crypto_returns[[crypto]]  # Extracted the returns for the current cryptocurrency
  
  # Plot of the histogram
  hist(ret, 
       100, 
       prob = TRUE, 
       col = "cornflowerblue", 
       main = paste(crypto),  # Title showing crypto name
       xlab = paste(crypto, "Daily Returns"))           # X-axis label showing crypto name
  
  # Overlayed normal distribution curve
  lines(sort(as.numeric(ret)), 
        dnorm(sort(as.numeric(ret)), 
              mean = mean(as.numeric(ret), na.rm = TRUE), 
              sd = sqrt(var(as.numeric(ret), na.rm = TRUE))),
        lwd = 2, col = "red") 
}

#QQ-Plot (Repeated first three steps from histograms. Gives certainty)

# List of cryptocurrency returns
crypto_returns <- list(
  BTC = BTC_clean$Return,
  ETH = ETH_clean$Return,
  Tether = Tether_clean$Return,
  BNB = BNB_clean$Return,
  SOL = SOL_clean$Return,
  USDC = USDC_clean$Return,
  Ripple = Ripple_clean$Return,
  DOGE = DOGE_clean$Return,
  TRON = TRON_clean$Return
)

# Set up a 3x3 grid for plotting all Q-Q plots on one page
par(mfrow = c(3, 3))  # 3 rows, 3 columns for 9 plots

# Looped through each cryptocurrency and created Q-Q plots
for (crypto in names(crypto_returns)) {
  ret <- crypto_returns[[crypto]]  # Extracted the returns for the current cryptocurrency
  
  # Created Q-Q plot
  qqnorm(ret, main = paste(crypto), 
         xlab = "Theoretical Quantiles", ylab = paste(crypto, "Quantiles"))
  qqline(ret, col = "red")  # Added the reference line in red
}

#########################################################################################################
#  Autocorrelation
#########################################################################################################
# List of cryptocurrency returns (exactly as in steps beforehand)
crypto_returns <- list(
  BTC = BTC_clean$Return,
  ETH = ETH_clean$Return,
  Tether = Tether_clean$Return,
  BNB = BNB_clean$Return,
  SOL = SOL_clean$Return,
  USDC = USDC_clean$Return,
  Ripple = Ripple_clean$Return,
  DOGE = DOGE_clean$Return,
  TRON = TRON_clean$Return
)

# Set up a 3x3 plotting grid for the 9 cryptocurrencies (exactly as in steps beforehand)
par(mfrow = c(3, 3))

# Looped through each cryptocurrency's returns and applied the ACF function
for (crypto in names(crypto_returns)) {
  ret_data <- crypto_returns[[crypto]]  # Extracted the return data for the current cryptocurrency->Not transformed
  
  # Applied the ACF function and plot the result
  acf(ret_data, main = paste(crypto, "ACF"))
}

# Looped through each cryptocurrency's returns, squared them, and applied the ACF function
for (crypto in names(crypto_returns)) {
  ret_data <- crypto_returns[[crypto]]^2  # Squared the return data for the current cryptocurrency
  
  # Applied the ACF function and plot the result for squared returns
  acf(ret_data, main = paste(crypto, "Squared Returns ACF"))
}

# Looped through each cryptocurrency's returns, taking the absolute value, and applyig the ACF function
for (crypto in names(crypto_returns)) {
  ret_data <- abs(crypto_returns[[crypto]])  # Took the absolute value of the return data
  
  # Applied the ACF function and plotted the result for absolute returns
  acf(ret_data, main = paste(crypto, "Absolute Returns ACF"))
}

#Normalized (to check if this makes an essential difference):

# List of cryptocurrency returns
crypto_returns <- list(
  BTC = BTC_clean$Return,
  ETH = ETH_clean$Return,
  Tether = Tether_clean$Return,
  BNB = BNB_clean$Return,
  SOL = SOL_clean$Return,
  USDC = USDC_clean$Return,
  Ripple = Ripple_clean$Return,
  DOGE = DOGE_clean$Return,
  TRON = TRON_clean$Return
)

# Function to normalize returns
normalize <- function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

# Set up a 3x3 plotting grid for the 9 cryptocurrencies
par(mfrow = c(3, 3))

# Looped through each cryptocurrency's returns, normalized, and applied the ACF function
for (crypto in names(crypto_returns)) {
  ret_data <- crypto_returns[[crypto]]  # Extracted the return data for the current cryptocurrency
  ret_data_normalized <- normalize(ret_data)  # Normalized the returns
  
  # Applied the ACF function and plotted the result for normalized returns
  acf(ret_data_normalized, main = paste(crypto, "ACF (Normalized Returns)"))
}

# Looped through each cryptocurrency's squared returns, normalized, and applied the ACF function
for (crypto in names(crypto_returns)) {
  ret_data <- crypto_returns[[crypto]]^2  # Squared the return data for the current cryptocurrency
  ret_data_normalized <- normalize(ret_data)  # Normalized the squared returns
  
  # Applied the ACF function and plotted the result for squared returns
  acf(ret_data_normalized, main = paste(crypto, "ACF (Normalized Squared Returns)"))
}

# Looped through each cryptocurrency's absolute returns, normalized, and applied the ACF function
for (crypto in names(crypto_returns)) {
  ret_data <- abs(crypto_returns[[crypto]])  # Took the absolute value of the return data
  ret_data_normalized <- normalize(ret_data)  # Normalized the absolute returns
  
  # Applied the ACF function and plotted the result for absolute returns
  acf(ret_data_normalized, main = paste(crypto, "ACF (Normalized Absolute Returns)"))
}

#Finding: Does not seem to display different results, if you use normalized version
#########################################################################################################
#  BTC Autocorrelation (daily and minute)
#########################################################################################################
# Loaded necessary libraries
library(ggplot2)
library(scales)

# Calculated the autocorrelation for returns (r), squared returns (r^2), and absolute returns (|r|)
btc_returns <- BTC_clean$Return
btc_returns_squared <- btc_returns^2
btc_returns_abs <- abs(btc_returns)

# Calculated autocorrelations with a custom maximum lag (1600 lags)
acf_r <- acf(btc_returns, plot = FALSE, lag.max = 1600)  # Autocorrelation of returns up to 1600 lags
acf_r2 <- acf(btc_returns_squared, plot = FALSE, lag.max = 1600)  # Autocorrelation of squared returns
acf_abs <- acf(btc_returns_abs, plot = FALSE, lag.max = 1600)  # Autocorrelation of absolute returns

# Extracted the autocorrelations and lags
lags <- acf_r$lag
acf_r_values <- acf_r$acf
acf_r2_values <- acf_r2$acf
acf_abs_values <- acf_abs$acf

# Calculated the confidence interval bounds (for a 95% confidence interval)
conf_level <- qnorm((1 + 0.95) / 2) / sqrt(length(btc_returns))
upper_conf <- rep(conf_level, length(lags))
lower_conf <- rep(-conf_level, length(lags))

# Combined into a data frame for plotting
acf_data <- data.frame(
  Lag = lags,
  Returns = acf_r_values,
  Squared_Returns = acf_r2_values,
  Absolute_Returns = acf_abs_values,
  Upper_CI = upper_conf,
  Lower_CI = lower_conf 
)

# Created the plot with ggplot2
ggplot(acf_data, aes(x = Lag)) +
  geom_point(aes(y = Returns, color = "Returns"), shape = 16, size = 1.5, alpha=0.4) +  # Autocorrelation (r)
  geom_point(aes(y = Squared_Returns, color = "Squared Returns"), shape = 17, size = 1.5, alpha=0.4) +  # Autocorrelation (r^2)
  geom_point(aes(y = Absolute_Returns, color = "Absolute Returns"), shape = 18, size = 1.5, alpha=0.4) +  # Autocorrelation (|r|)
  geom_line(aes(y = Upper_CI), linetype = "dotted", color = "gray") + 
  geom_line(aes(y = Lower_CI), linetype = "dotted", color = "gray") +  
  scale_x_log10(limits = c(1, 1600),  # Set x-axis limits from 1 day to 1600 days
                breaks = c(1, 10, 100, 1000, 1600),  # Custom breaks for daily data
                labels = c("1 day", "10 days", "100 days", "1000 days", "1600 days")) +  
  labs(
    x = "Lag (in days)", 
    y = "Autocorrelation", 
    title = "Autocorrelation Analysis of Bitcoin Daily Returns, Squared Returns, and Absolute Returns",
    subtitle = "Daily data over 1600 days",
    color = "Legend"  # Label the legend as "Legend"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90", size = 0.2),  # Subtle gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines for a cleaner look
  ) +
  scale_color_manual(
    values = c("Returns" = "blue", "Squared Returns" = "orange", "Absolute Returns" = "green"),
    breaks = c("Returns", "Squared Returns", "Absolute Returns")  # Ensure the legend order
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add a horizontal line at y = 0
  ylim(-0.1, 0.2) +  # Y-axis limits
  annotate("text", x = 50, y = conf_level + 0.02, label = "95% Confidence Interval", color = "gray", size = 4, hjust = 0) +
  annotate("text", x = 50, y = -conf_level - 0.02, label = "95% Confidence Interval", color = "gray", size = 4, hjust = 0)

# Total number of autocorrelation points (lags)
total_points <- length(acf_r_values)

# Calculated how many data points breach the confidence interval for returns
breach_upper <- sum(acf_r_values > conf_level)  # Count how many exceed the upper confidence interval
breach_lower <- sum(acf_r_values < -conf_level)  # Count how many are below the lower confidence interval

# Total breaches
total_breaches <- breach_upper + breach_lower

# Calculated the percentage of breaches
percentage_breaches <- (total_breaches / total_points) * 100

# Printed the results
cat("Total number of data points (lags):", total_points, "\n")
cat("Number of data points breaching the confidence interval:", total_breaches, "\n")
cat("Percentage of breaches:", round(percentage_breaches, 2), "%\n")
# Total number of autocorrelation points (lags)
total_points2 <- length(acf_r2_values)

# Calculated how many data points breach the confidence interval for returns
breach_upper2 <- sum(acf_r2_values > conf_level)  
breach_lower2 <- sum(acf_r2_values < -conf_level)  

# Total breaches
total_breaches2 <- breach_upper2 + breach_lower2

# Calculated the percentage of breaches
percentage_breaches2 <- (total_breaches2 / total_points2) * 100

# Printed the results
cat("Total number of data points (lags):", total_points2, "\n")
cat("Number of data points breaching the confidence interval:", total_breaches2, "\n")
cat("Percentage of breaches:", round(percentage_breaches2, 2), "%\n")

# Total number of autocorrelation points (lags)
total_points1 <- length(acf_abs_values)

# Calculated how many data points breach the confidence interval for returns
breach_upper1 <- sum(acf_abs_values > conf_level)  
breach_lower1 <- sum(acf_abs_values < -conf_level)  

# Total breaches
total_breaches1 <- breach_upper1 + breach_lower1

# Calculated the percentage of breaches
percentage_breaches1 <- (total_breaches1 / total_points1) * 100

# Printed the results
cat("Total number of data points (lags):", total_points1, "\n")
cat("Number of data points breaching the confidence interval:", total_breaches1, "\n")
cat("Percentage of breaches:", round(percentage_breaches1, 2), "%\n")

######Now everything with minute data, as one sees the effect here more clearly:
library(readr)
BTCmin <- bitcoin_historical_minute_data_7_days_tokeninsight <- read_csv("Documents/Viola's R-Projects/BachelorArbeit/Data on Cryptos/bitcoin-historical-minute-data-7-days-tokeninsight.csv")
View(BTCmin)

# Loaded necessary libraries
library(ggplot2)
library(scales)
library(dplyr)

# BTCmin is the cleaned minute-by-minute price data
BTCmin <- BTCmin %>%
  mutate(Return = c(NA, diff(log(Price))))

# Cleaned the data by removing NA
BTCmin_clean <- BTCmin %>%
  filter(!is.na(Return))

# Calculated the autocorrelation for returns (r), squared returns (r^2), and absolute returns (|r|)
btcmin_returns <- BTCmin_clean$Return
btcmin_returns_squared <- btcmin_returns^2
btcmin_returns_abs <- abs(btcmin_returns)

# Calculated autocorrelations with a custom maximum lag (e.g., 10,000 minutes)
acf_rmin <- acf(btcmin_returns, plot = FALSE, lag.max = 10000)  # Autocorrelation of returns up to 10,000 lags
acf_r2min <- acf(btcmin_returns_squared, plot = FALSE, lag.max = 10000)  
acf_absmin <- acf(btcmin_returns_abs, plot = FALSE, lag.max = 10000)  

# Extracted the autocorrelations and lags
lagsmin <- acf_rmin$lag
acf_rmin_values <- acf_rmin$acf
acf_r2min_values <- acf_r2min$acf
acf_absmin_values <- acf_absmin$acf

# Calculated the 95% confidence interval for the autocorrelation
n_obs_min <- length(btcmin_returns)  # Number of observations
conf_level_min <- qnorm((1 + 0.95) / 2) / sqrt(n_obs_min)
upper_conf_min <- rep(conf_level, length(lagsmin))
lower_conf_min <- rep(-conf_level, length(lagsmin))

# Combined the data into a data frame for plotting
acf_data_min <- data.frame(
  Lag = lagsmin,
  Returns = acf_rmin_values,
  Squared_Returns = acf_r2min_values,
  Absolute_Returns = acf_absmin_values,
  Upper_CI_min = upper_conf_min,  
  Lower_CI_min = lower_conf_min   
)

View(acf_data_min)

# Created the plot using ggplot2 with a logarithmic scale for the x-axis
ggplot(acf_data_min, aes(x = Lag)) +
  geom_point(aes(y = Returns, color = "Returns"), shape = 16, size = 1.5, alpha = 0.4) +  
  geom_point(aes(y = Squared_Returns, color = "Squared Returns"), shape = 17, size = 1.5, alpha = 0.4) +  
  geom_point(aes(y = Absolute_Returns, color = "Absolute Returns"), shape = 18, size = 1.5, alpha = 0.4) +
  geom_line(aes(y = Upper_CI_min), linetype = "dotted", color = "gray") +  
  geom_line(aes(y = Lower_CI_min), linetype = "dotted", color = "gray") +  
  scale_x_log10(
    limits = c(1, 10000),  
    breaks = c(1, 60, 60*24, 60*24*7),  # Custom breaks for 1 minute, 1 hour, 1 day, and 1 week
    labels = c("1 min", "1 hour", "1 day", "1 week"),  # Custom labels for time intervals
    minor_breaks = c(1, 10, 100, 1000, 10000)  # Add minor breaks for 10^0, 10^1, 10^2, 10^3, 10^4
  ) +
  labs(
    x = "Lag", 
    y = "Autocorrelation", 
    title = "Autocorrelation of Minute Returns, Squared Returns, and Absolute Returns",
    subtitle = "Minute-level data over 7 days",
    colour = "Legend"  
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right",
    panel.grid.minor.x = element_line(color = "lightgrey", linetype = "dashed")  
  ) +
  scale_color_manual(values = c("Returns" = "blue", "Squared Returns" = "orange", "Absolute Returns" = "green"), breaks = c("Returns", "Squared Returns", "Absolute Returns") 
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add a horizontal line at y = 0
  ylim(-0.1, 0.25) +  # Set the y-axis limits from -0.1 to 0.25
  annotate("text", x = 10, y = conf_level_min + 0.02, label = "95% Confidence Interval", color = "gray", size = 4, hjust = 0) +
  annotate("text", x = 10, y = -conf_level_min - 0.02, label = "95% Confidence Interval", color = "gray", size = 4, hjust = 0) +
  # Custom labels for minor breaks
  annotate("text", x = 1, y = -0.1, label = expression(10^0), vjust = 1.5, size = 3, color = "grey") +
  annotate("text", x = 10, y = -0.1, label = expression(10^1), vjust = 1.5, size = 3, color = "grey") +
  annotate("text", x = 100, y = -0.1, label = expression(10^2), vjust = 1.5, size = 3, color = "grey") +
  annotate("text", x = 1000, y = -0.1, label = expression(10^3), vjust = 1.5, size = 3, color = "grey") +
  annotate("text", x = 10000, y = -0.1, label = expression(10^4), vjust = 1.5, size = 3, color = "grey")


# Total number of autocorrelation points (lags)
total_points <- length(acf_rmin_values)

# Calculated how many data points breach the confidence interval for returns
breach_upper <- sum(acf_rmin_values > conf_level_min)  # Count how many exceed the upper confidence interval
breach_lower <- sum(acf_rmin_values < -conf_level_min)  # Count how many are below the lower confidence interval

# Total breaches
total_breaches <- breach_upper + breach_lower

# Calculated the percentage of breaches
percentage_breaches <- (total_breaches / total_points) * 100

# Printed the results
cat("Total number of data points (lags):", total_points, "\n")
cat("Number of data points breaching the confidence interval:", total_breaches, "\n")
cat("Percentage of breaches:", round(percentage_breaches, 2), "%\n")

# Total number of autocorrelation points (lags)
total_points2 <- length(acf_r2min_values)

# Calculated how many data points breach the confidence interval for returns
breach_upper2 <- sum(acf_r2min_values > conf_level_min)  
breach_lower2 <- sum(acf_r2min_values < -conf_level_min)  

# Total breaches
total_breaches2 <- breach_upper2 + breach_lower2

# Calculated the percentage of breaches
percentage_breaches2 <- (total_breaches2 / total_points2) * 100

# Printed the results
cat("Total number of data points (lags):", total_points2, "\n")
cat("Number of data points breaching the confidence interval:", total_breaches2, "\n")
cat("Percentage of breaches:", round(percentage_breaches2, 2), "%\n")

# Total number of autocorrelation points (lags)
total_points1 <- length(acf_absmin_values)

# Calculated how many data points breach the confidence interval for returns
breach_upper1 <- sum(acf_absmin_values > conf_level_min)  
breach_lower1 <- sum(acf_absmin_values < -conf_level_min) 

# Total breaches
total_breaches1 <- breach_upper1 + breach_lower1

# Calculated the percentage of breaches
percentage_breaches1 <- (total_breaches1 / total_points1) * 100

# Printed the results
cat("Total number of data points (lags):", total_points1, "\n")
cat("Number of data points breaching the confidence interval:", total_breaches1, "\n")
cat("Percentage of breaches:", round(percentage_breaches1, 2), "%\n")

# Fit linear models to the autocorrelations of returns, squared returns, and absolute returns
acf_returns_slope <- lm(acf_rmin_values ~ lagsmin)
acf_squared_returns_slope <- lm(acf_r2min_values ~ lagsmin)
acf_absolute_returns_slope <- lm(acf_absmin_values ~ lagsmin)

# Summarized the models to get the !slope! coefficients
summary(acf_returns_slope)
summary(acf_squared_returns_slope)
summary(acf_absolute_returns_slope)  

# Printed summary results for each model (basically same as before, just in a different way)
cat("Summary for Returns Slope:\n")
print(summary(acf_returns_slope))

cat("\nSummary for Squared Returns Slope:\n")
print(summary(acf_squared_returns_slope))

cat("\nSummary for Absolute Returns Slope:\n")
print(summary(acf_absolute_returns_slope))

##########################
#Jumps
#########################

# Loaded necessary libraries
library(dplyr)
library(ggplot2)

# List of cryptocurrency returns (as we also did in other sections)
crypto_returns <- list(
  BTC = BTC_clean$Return,
  ETH = ETH_clean$Return,
  Tether = Tether_clean$Return,
  BNB = BNB_clean$Return,
  SOL = SOL_clean$Return,
  USDC = USDC_clean$Return,
  Ripple = Ripple_clean$Return,
  DOGE = DOGE_clean$Return,
  TRON = TRON_clean$Return
)

# Defined the function to calculate RV, BPV, and Jump Component for log returns
calculate_rv_bpv_jump <- function(log_returns) {
  n <- length(log_returns)
  
  # Calculated Realized Volatility (RV)
  RV <- sum(log_returns^2, na.rm = TRUE)
  
  # Calculated the BiPower Variation (BPV)
  mu1 <- sqrt(2 / pi)
  
  # BPV using adjacent absolute returns
  BPV <- mu1^(-2) * sum(abs(log_returns[2:n]) * abs(log_returns[1:(n-1)]), na.rm = TRUE)
  
  # Calculate Jump Component (J)
  J <- max(RV - BPV, 0)
  
  return(list(RV = RV, BPV = BPV, Jump = J))
}

# As cryptocurrency data's log returns are in crypto_returns
results <- list()

# Looped through each cryptocurrency to calculate RV, BPV, and Jump Component
for (crypto in names(crypto_returns)) {
  log_returns <- crypto_returns[[crypto]]  # Extracted log returns for the current cryptocurrency
  
  # Calculated RV, BPV, and Jump for the entire dataset of log returns
  result <- calculate_rv_bpv_jump(log_returns)
  
  # Stored the results for this cryptocurrency
  results[[crypto]] <- result
}

# Displayed results
results

library("quantmod")
# Loaded necessary libraries
library(quantmod)
library(ggplot2)
library(dplyr)  # For data manipulation

# Defined the function to calculate RV, BPV, and Jump Component for log returns
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
  
  return(list(RV = RV, BPV = BPV, Jump = J))
}

# List of assets to fetch data
assets <- list(
  "CrudeOil" = "CL=F",
  "NVDA" = "NVDA",
  "S&P500" = "^GSPC"
)

# Created a list to store the log returns for each asset
log_returns_list <- list()

# Looped through each asset to fetch data, calculate log returns, and store in log_returns_list
for (asset_name in names(assets)) {
  # Fetched data from Yahoo Finance
  asset_data <- getSymbols(assets[[asset_name]], auto.assign = FALSE, from = "2020-04-11", to = "2023-09-13", src = 'yahoo')
  
  # Found the closing price column dynamically (either Adjusted or Close)
  price_col <- grep("Adjusted|Close", colnames(asset_data), value = TRUE)[1]  # Found the first matching column
  
  # Checked if we found a valid price column
  if (!is.null(price_col)) {
    asset_prices <- asset_data[, price_col]
    
    # Calculated log returns and remove NA values
    log_returns <- diff(log(asset_prices))  
    log_returns <- na.omit(log_returns)  
    
    # Stored the log returns in the log_returns_list
    log_returns_list[[asset_name]] <- as.numeric(log_returns)
  } else {
    warning(paste("No Adjusted or Close column found for", asset_name))
  }
}

# Created a list to store the results (RV, BPV, and Jump) for each asset
results1 <- list()

# Looped through each asset to calculate RV, BPV, and Jump Component
for (asset_name in names(log_returns_list)) {
  log_returns <- log_returns_list[[asset_name]]  
  
  # Calculated RV, BPV, and Jump
  result1 <- calculate_rv_bpv_jump(log_returns)
  
  # Stored the results for this asset
  results1[[asset_name]] <- result1
}

# Displayed the results for each asset
results1

# Loaded necessary libraries
library(dplyr)
library(tidyr)

# Combined both the cryptocurrency results and the additional asset results
combined_results <- bind_rows(
  # Converted the cryptocurrency results into a data frame
  bind_rows(lapply(results, as.data.frame), .id = "Crypto"),
  
  # Converted the asset results into a data frame
  bind_rows(lapply(results1, as.data.frame), .id = "Asset")
)

# Displayed the combined table
print(combined_results)

library(kableExtra)
library(dplyr)
library(webshot)

# Combined both cryptocurrency and asset results into a data frame
combined_results <- bind_rows(
  bind_rows(lapply(results, as.data.frame), .id = "Name"),  # For cryptocurrencies
  bind_rows(lapply(results1, as.data.frame), .id = "Name")  # For traditional assets
)

# Created a lookup table for symbol abbreviations, including Crude Oil as CL=F
symbol_lookup <- data.frame(
  Name = c("BTC", "ETH", "Tether", "BNB", "SOL", "USDC", "Ripple", "DOGE", "TRON", "NVDA", "S&P500", "CrudeOil"),
  Symbol = c("BTC", "ETH", "USDT", "BNB", "SOL", "USDC", "XRP", "DOGE", "TRX", "NVDA", "GSPC", "CL=F")
)

# Merged symbol lookup with combined results
table_data <- combined_results %>%
  left_join(symbol_lookup, by = "Name") %>%  # Add Symbol column based on the Name column
  select(Symbol, RV, BPV, Jump) %>%  # Select only Symbol, RV, BPV, and Jump
  mutate(
    RV = round(RV, 4),  # Round RV to 4 decimal places, hehe, and also BPV and Jumps in the following:)
    BPV = round(BPV, 4),  
    Jump = round(Jump, 4),  
    Jump_Percentage = round((Jump / RV) * 100, 2)  
  ) %>%
  rename("Realized Variance (RV)" = RV, "BiPower Variation (BPV)" = BPV, 
         "Jump Component" = Jump, "Jump as % of RV" = Jump_Percentage)

# Founnd the Jump Percentage of Crude Oil (CL=F) for comparison
crude_oil_jump_percentage <- table_data %>%
  filter(Symbol == "CL=F") %>%
  pull(`Jump as % of RV`)

# Created a nice table with kable and kableExtra (althougg table later not really used, just results)
table_data %>%
  kable("html", align = "c", col.names = colnames(table_data)) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1:5, border_left = TRUE, border_right = TRUE) %>%
  
  # Highlighted rows with Jump % > Crude Oil percentage in cornflowerblue
  row_spec(which(table_data$`Jump as % of RV` > crude_oil_jump_percentage), background = "cornflowerblue", color = "white") %>%
  
  # Highlighted Crude Oil row itself in navy blue
  row_spec(which(table_data$Symbol == "CL=F"), background = "navy", color = "white") %>%
  
  row_spec(nrow(table_data), extra_css = "border-bottom: 1px solid #ddd;") %>%  # Add a bottom border to the last row
  
  save_kable("Jumps1.html")

# Converted the HTML table to an image
webshot("Jumps1.html", "Jumps1.png")

#Again, in the end, Overleaf-table replaced this table

################
#Tail Index
#############

# Loaded required libraries
library(quantmod)
library(ggplot2)
library(dplyr)
library(xts)

# Loaded all cryptocurrency data into a named list (we'll do it from beginning again to mitigate confusion with already loaded data)
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

# Function to analyze a single cryptocurrency
analyze_crypto <- function(crypto_data, crypto_name, k = 10) {
  cat("Analyzing", crypto_name, "...\n")
  
  # Ensure 'Date' is in Date format and filter by the specified range
  crypto_data <- crypto_data %>%
    mutate(Date = as.Date(sub(" .*", "", Date))) %>%
    filter(!is.na(Price) & Date >= as.Date("2020-04-11"))
  
  # Converted to xts object
  crypto_xts <- xts(crypto_data[, c("Price", "Volume", "Market_cap")], order.by = crypto_data$Date)
  
  # Extracted the 'Price' column and removed NA values
  price <- crypto_xts$Price
  price <- na.omit(price)
  
  # Computed log returns
  ret <- diff(log(price))
  ret <- as.numeric(na.omit(ret))  
  N <- length(ret)  
  
  # Initialized results
  results <- list()
  
  # Performed tail analysis (upper and lower)
  for (tail in c("Upper", "Lower")) {
    if (tail == "Upper") {
      ret_sorted <- sort(ret)  # Upper tail (small to large values)
    } else {
      ret_sorted <- sort(ret, decreasing = TRUE)  # Lower tail (large to small values)
    }
    
    logRet <- log(abs(ret_sorted))
    
    # Performed regression on the extreme values
    X <- logRet[(N - k + 1):N]
    Y <- log((k:1) / N)
    
    # Removed invalid values
    valid_idx <- !(is.na(X) | is.infinite(X) | is.na(Y) | is.infinite(Y))
    X <- X[valid_idx]
    Y <- Y[valid_idx]
    
    if (length(X) < 2 || length(Y) < 2) {
      cat("Skipping", tail, "tail for", crypto_name, "- insufficient valid data.\n")
      results[[tail]] <- NULL
      next
    }
    
    # Performed regression
    YX <- data.frame(X = X, Y = Y)
    reg <- lm(Y ~ X, data = YX)
    results[[tail]] <- list(
      slope = coef(reg)[2],
      intercept = coef(reg)[1],
      summary = summary(reg)
    )
    
    # Ploted the results
    plot(X, Y,
         cex = 1.5,
         ylab = "log(i/n)",
         xlab = "log(|returns|)",
         main = paste(crypto_name, "-", tail, "Tail"))
    lines(X, X * coef(reg)[2] + coef(reg)[1],
          lwd = 2, col = "red")
  }
  
  return(results)
}

# Applied the analysis to all cryptocurrencies
crypto_results <- lapply(names(crypto_datasets), function(crypto_name) {
  analyze_crypto(crypto_datasets[[crypto_name]], crypto_name)
})

slopes <- lapply(crypto_results, function(res) {
  if (!is.null(res)) {
    list(upper = res$Upper$slope, lower = res$Lower$slope)
  } else {
    list(upper = NA, lower = NA)
  }
})
slopes_df <- do.call(rbind, lapply(names(slopes), function(name) {
  cbind(Crypto = name, as.data.frame(slopes[[name]]))
}))

str(crypto_results)

str(slopes)

###Robustness Check

# Loaded required libraries
library(quantmod)
library(ggplot2)
library(dplyr)
library(xts)

# Loaded all cryptocurrency data into a named list
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

# Robustness Check Function for Each Crypto
robustness_check <- function(crypto_data, crypto_name, k_values) {
  cat("Performing robustness check for", crypto_name, "...\n")
  
  # Ensured 'Date' is in Date format and filter by the specified range
  crypto_data <- crypto_data %>%
    mutate(Date = as.Date(sub(" .*", "", Date))) %>%
    filter(!is.na(Price) & Date >= as.Date("2020-04-11"))
  
  # Converted to xts object
  crypto_xts <- xts(crypto_data[, c("Price", "Volume", "Market_cap")], order.by = crypto_data$Date)
  
  # Extracted the 'Price' column and removed NA values
  price <- crypto_xts$Price
  price <- na.omit(price)
  
  # Computed log returns
  ret <- diff(log(price))
  ret <- as.numeric(na.omit(ret))  
  N <- length(ret)  
  
  upper_slopes <- c()
  lower_slopes <- c()
  
  for (k in k_values) {
    # --- Upper Tail Analysis ---
    ret_sorted <- sort(ret)
    logRet <- log(abs(ret_sorted))
    X <- logRet[(N - k + 1):N]
    Y <- log((k:1) / N)
    reg <- lm(Y ~ X)
    upper_slopes <- c(upper_slopes, coef(reg)[2])  
    
    # --- Lower Tail Analysis ---
    ret_sorted <- sort(ret, decreasing = TRUE)
    logRet <- log(abs(ret_sorted))
    X <- logRet[(N - k + 1):N]
    Y <- log((k:1) / N)
    reg <- lm(Y ~ X)
    lower_slopes <- c(lower_slopes, coef(reg)[2])  
  }
  
  # Combined results
  robustness_results <- data.frame(
    Fraction = k_values / N,  # Fraction of data used
    Upper = upper_slopes,
    Lower = lower_slopes
  )
  
  # Returned results
  return(robustness_results)
}

# Defined k values for robustness check
k_values <- seq(5, 20, by = 5)  # Testing with different fractions of the data

# Performed robustness check for all cryptocurrencies
robustness_results_all <- lapply(names(crypto_datasets), function(crypto_name) {
  robustness_check(crypto_datasets[[crypto_name]], crypto_name, k_values)
})

# Combined all results into one data frame
combined_robustness_results <- do.call(rbind, lapply(seq_along(robustness_results_all), function(i) {
  results <- robustness_results_all[[i]]
  if (!is.null(results)) {
    results$Crypto <- names(crypto_datasets)[i]  # Add Crypto column
    return(results)
  } else {
    return(NULL)
  }
}))

# Plot of the robustness check results
ggplot(combined_robustness_results, aes(x = Fraction)) +
  geom_line(aes(y = Upper, color = "Upper Tail")) +
  geom_line(aes(y = Lower, color = "Lower Tail")) +
  facet_wrap(~ Crypto, scales = "free") +
  labs(title = "Robustness Check of Tail Index",
       x = "Fraction of Data (k/N)",
       y = "Tail Index (Slope)") +
  scale_color_manual(values = c("Upper Tail" = "blue", "Lower Tail" = "red")) +
  theme_minimal()

# Specified the order of cryptocurrencies as per the image
crypto_order <- c("BTC", "ETH", "Tether", "BNB", "SOL", "USDC", "Ripple", "DOGE", "TRON")

# Ensured the 'Crypto' column in combined_robustness_results is a factor with the specified order
combined_robustness_results$Crypto <- factor(combined_robustness_results$Crypto, levels = crypto_order)

# Plot of the robustness check results
ggplot(combined_robustness_results, aes(x = Fraction)) +
  geom_line(aes(y = Upper, color = "Upper Tail")) +
  geom_line(aes(y = Lower, color = "Lower Tail")) +
  facet_wrap(~ Crypto, scales = "free", ncol = 3) +  # Specify the number of columns to match the layout
  labs(title = "Robustness Check of Tail Index",
       x = "Fraction of Data (k/N)",
       y = "Tail Index (Slope)") +
  scale_color_manual(values = c("Upper Tail" = "blue", "Lower Tail" = "red")) +
  theme_minimal()

#########End########
