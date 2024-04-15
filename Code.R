#//////////////////////////////////////////////////////////
#                 BET MONTHLY DATA - 52 WEEK HIGH
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



#==# #==# #==# #==# #==# #==# #==# #==# #==# #==# 
#==# #==#          LIBRARIES            #==# #==#
#==# #==# #==# #==# #==# #==# #==# #==# #==# #==# 



library(tidyverse)
library(ggplot2)
library(zoo)
library(lubridate)
library(progress)
library(slider)
library(writexl)
library(purrr)
library(moments)
library(openxlsx)
library(extrafont)
library(tidyr)
library(broom)



#==# #==# #==# #==# #==# #==# #==# #==# #==# #==# 
#==# #==#          FILE IMPORT          #==# #==#
#==# #==# #==# #==# #==# #==# #==# #==# #==# #==#



# File import

prime_Detailed <- read.csv("Prime_Detailed_1990_2024_monthly.csv", row.names = NULL)

common_Detailed <- read.csv("Common_Detailed_1990_2024_monthly.csv", row.names = NULL)

# Correcting column names

new_colnames <- colnames(prime_Detailed)[-1] # Shift column names to left by 1
colnames(prime_Detailed) <- new_colnames # Rename columns
prime_Detailed <- prime_Detailed[, -ncol(prime_Detailed)] # Delete last NA column

new_colnames <- colnames(common_Detailed)[-1] # Shift column names to left by 1
colnames(common_Detailed) <- new_colnames # Rename columns
common_Detailed <- common_Detailed[, -ncol(common_Detailed)] # Delete last NA column



#==# #==# #==# #==# #==# #==# #==# #==# #==# #==# 
#==# #==#       DATA MANIPULATION       #==# #==#
#==# #==# #==# #==# #==# #==# #==# #==# #==# #==#



################
# STOCKS_MONTHLY

stocks_monthly <- as_tibble(full_join(prime_Detailed, common_Detailed)) %>% # Full join
rename( # Rename columns
  ticker = "Név",
  date = "Dátum",
  priceOpen = "Nyitó.ár",
  priceMax = "Maximum.ár",
  priceMin = "Minimum.ár",
  priceClose = "Utolsó.ár",
  priceAvg = "Átlag.ár",
  quantity_traded = "Forgalom..db."
) %>% 
mutate(date = as.Date(paste0(date, "01"), format = "%Y.%m.%d")) %>% # Change date format
filter(Deviza == "HUF") %>%  # Anchoring effect makes sense only in the official currency
select(ticker, date, priceOpen, priceClose, priceMin, priceMax, priceAvg) # Select useful columns



#####################
# Price interpolation
#####################


stocks_monthly_split <- split(stocks_monthly, stocks_monthly$ticker)

stocks_monthly_split[[1]]


# Fill in missing dates


fill_missing_months <- function(tibble) {
# Ensure date column is Date type
tibble$date <- as.Date(tibble$date)

# Generate a sequence of first days of each month between the min and max dates
date_seq <- seq(from = floor_date(min(tibble$date), "month"), 
                to = ceiling_date(max(tibble$date), "month") - days(1), 
                by = "month")

# Expand the tibble to include all dates in the sequence, filling in NAs where data is missing
tibble_filled <- tibble %>% 
  complete(date = date_seq) %>% 
  mutate(ticker = ifelse(is.na(ticker), first(ticker), ticker))

return(tibble_filled)
}

# Apply the function to each tibble in the list
stocks_monthly_split <- map(stocks_monthly_split, fill_missing_months)

########################### FUNCTION
# INTERPOLATE TIBBLE PRICES

interpolate_tibble <- function(tbl) {
tbl %>%
  mutate(across(c(priceClose, priceOpen, priceMax, priceMin, priceAvg), ~ {
    # Convert column to a vector
    vec <- .x
    # Find indices where NA is not at the start or end
    na_indices <- which(is.na(vec))
    valid_indices <- which(!is.na(vec))
    
    if (length(valid_indices) > 0) {
      first_valid <- min(valid_indices)
      last_valid <- max(valid_indices)
      to_interpolate <- na_indices[na_indices > first_valid & na_indices < last_valid]
      
      # Replace only the selected NAs with interpolated values, keep start/end NAs
      vec[to_interpolate] <- na.approx(vec, na.rm = FALSE)[to_interpolate]
    }
    
    return(vec)
  }))
}


########################### APPLICATION
# INTERPOLATE TIBBLE PRICES


stocks_monthly_split_interpolated <- map(stocks_monthly_split, interpolate_tibble)
stocks_monthly_interpolated <- bind_rows(stocks_monthly_split_interpolated)


#################################################
# CALCULATING 52 WEEKS HIGH
#################################################


stocks_monthly <- stocks_monthly_interpolated %>%
  arrange(ticker, date) %>%
  group_by(ticker) %>%
  mutate("52wh" = slide_index_dbl(.x = priceMax, .i = date, .f = max, .before = months(12), .after = days(0), .complete = TRUE),
         "proximity" = priceClose / `52wh`) %>%
  ungroup() %>% 
  filter (date >= "1994-01-01" & date <= "2023-12-01")


#################################################
# CALCULATING NEXT TRADING DAY AFTER X PERIOD
#################################################


# Splitting tibbles

stocks_monthly_split <- split(stocks_monthly, stocks_monthly$ticker)


############################ FUNCTION
# Find trading days function

find_trading_days <- function(tb) {
tb %>%
  arrange(date) %>%
  mutate(
    future_date_1M = date %m+% months(1),
    future_date_2M = date %m+% months(2),
    future_date_3M = date %m+% months(3),
    future_date_6M = date %m+% months(6),
    future_date_12M = date %m+% months(12)
  ) %>%
  return()
}



############################# APPLICATION
# Apply trading days function

stocks_monthly_split <- lapply(stocks_monthly_split, find_trading_days)
stocks_monthly <- as_tibble(bind_rows(stocks_monthly_split)) %>% 
drop_na()


#################################################
# CALCULATING RETURNS
#################################################


# Last Price Dates table
priceClose_dates <- stocks_monthly %>% 
select(ticker, date, priceClose) %>% 
rename(
  priceCloseNew = priceClose
)


########################### FUNCTION
# Price close match function

priceClose_match <- function(future_date) {
#Matches lastPrice of Closest_next_day to Date
left_join(priceClose_dates,
          by = c("ticker", future_date = "date"))
}


####################################### APPLICATION
# Price close match function application

stocks_monthly <- stocks_monthly %>% 

left_join(priceClose_dates,
          by = c("ticker", "future_date_1M" = "date")) %>% 
rename(priceClose1M = priceCloseNew) %>% 

left_join(priceClose_dates,
          by = c("ticker", "future_date_2M" = "date")) %>% 
rename(priceClose2M = priceCloseNew) %>% 

left_join(priceClose_dates,
          by = c("ticker", "future_date_3M" = "date")) %>% 
rename(priceClose3M = priceCloseNew) %>% 

left_join(priceClose_dates,
          by = c("ticker", "future_date_6M" = "date")) %>% 
rename(priceClose6M = priceCloseNew) %>% 

left_join(priceClose_dates,
          by = c("ticker", "future_date_12M" = "date")) %>% 
rename(priceClose12M = priceCloseNew) %>% 

select(
  ticker, date, priceClose, priceMax, "52wh", proximity,
  priceClose1M, priceClose2M, priceClose3M, priceClose6M, priceClose12M)



# Calculate returns


stocks_monthly_returns <- stocks_monthly %>%
mutate(
  return1M = priceClose1M / priceClose - 1,
  return2M = priceClose2M / priceClose - 1,
  return3M = priceClose3M / priceClose - 1,
  return6M = priceClose6M / priceClose - 1,
  return12M = priceClose12M / priceClose - 1) %>%

select(
  ticker, date, priceClose, "52wh", proximity,
  return1M, return2M, return3M, return6M, return12M)



#==# #==# #==# #==# #==# #==# #==# #==# #==# #==# 
#==# #==#       PORTFOLIO CREATION      #==# #==#
#==# #==# #==# #==# #==# #==# #==# #==# #==# #==#



stocks_monthly_returns %>% 
distinct(ticker)

stocks_portfolio <- stocks_monthly_returns %>%
  group_by(date) %>%
  mutate(traded_companies = n()) %>%
  ungroup() %>% 
  filter (date >= "1994-01-01" & date <= "2023-12-01")


# Top 30%

stocks_portfolio <- stocks_portfolio %>%
group_by(date) %>%
mutate(
  # Rank companies by proximity in descending order for each date
  rank_proximity = rank(-proximity),
  # Calculate the number of companies that make up the top 30%
  top_30_limit = ceiling(0.3 * traded_companies) # You can adjust rounding strategy
) %>%
# Determine if a ticker is in the top 30% based on the rank and the limit calculated
mutate(top_30_percent = if_else(rank_proximity <= top_30_limit, 1, 0)) %>%
select(-rank_proximity, -top_30_limit) %>% # Optional: Remove auxiliary columns
ungroup()


# Bottom 30%

stocks_portfolio <- stocks_portfolio %>%
group_by(date) %>%
mutate(
  # Rank companies by proximity in ascending order for each date to identify the lowest 30%
  rank_proximity_asc = rank(proximity),
  # Calculate the number of companies that make up the bottom 30%
  bottom_30_limit = ceiling(0.3 * traded_companies) # Adjust rounding strategy as needed
) %>%
# Determine if a ticker is in the bottom 30% based on the rank and the limit calculated
mutate(bottom_30_percent = if_else(rank_proximity_asc <= bottom_30_limit, 1, 0)) %>%
# Optionally, remove auxiliary columns used for calculations
select(-rank_proximity_asc, -bottom_30_limit) %>%
ungroup()


# Classification

stocks_portfolio <- stocks_portfolio %>%
mutate(
  proximity_group = case_when(
    top_30_percent == 1 ~ "Top 30%",
    bottom_30_percent == 1 ~ "Bottom 30%",
    TRUE ~ "Middle 40%"
  )
)


#==# #==# #==# #==# #==# #==# #==# #==# #==# #==# 
#==# #==#    1M RETURN DIFFERENCES      #==# #==#
#==# #==# #==# #==# #==# #==# #==# #==# #==# #==#



ggplot(stocks_portfolio, aes(x = proximity_group, y = return1M, group = proximity_group)) +
stat_summary(fun = mean, geom = "point", size = 3, color = "blue") +
stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "red") +
scale_x_discrete(labels = c("Top 30%" = "Top 30%", "Middle 40%" = "Middle 40%", "Bottom 30%" = "Bottom 30%")) +
labs(title = "Average 1-Month Return with 95% CI: Proximity Group Comparison",
     x = "Proximity Group",
     y = "Average 1-Month Return",
     caption = "Data: stocks_portfolio") +
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))



#==# #==# #==# #==# #==# #==# #==# #==# #==# #==# 
#==# #==#    6M RETURN DIFFERENCES      #==# #==#
#==# #==# #==# #==# #==# #==# #==# #==# #==# #==#


ggplot(stocks_portfolio, aes(x = proximity_group, y = return6M, group = proximity_group)) +
stat_summary(fun = mean, geom = "point", size = 3, color = "blue") +
stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "red") +
scale_x_discrete(labels = c("Top 30%" = "Top 30%", "Middle 40%" = "Middle 40%", "Bottom 30%" = "Bottom 30%")) +
labs(title = "Average 6-Month Return with 95% CI: Proximity Group Comparison",
     x = "Proximity Group",
     y = "Average 6-Month Return",
     caption = "Data: stocks_portfolio") +
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))


# T TEST

# Perform a t-test comparing the 6-month returns of the top 30% group vs. the others
t_test_result <- t.test(return6M ~ top_30_percent, data = stocks_portfolio)

# Directly extracting the p-value
p_value <- t_test_result$p.value

# Displaying the p-value
cat("P-value from t-test:", format.pval(p_value, digits = 3), "\n")


write.xlsx(stocks_portfolio, "stocks_portfolio.xlsx")

################################# FUNCTION
# CALCULATE GROUPS STATS FUNCTION
calculate_group_stats <- function(df, return_col_name) {
df %>%
  group_by(proximity_group) %>%
  summarise(
    mean = mean(!!sym(return_col_name), na.rm = TRUE),
    median = median(!!sym(return_col_name), na.rm = TRUE),
    variance = sd(!!sym(return_col_name), na.rm = TRUE),
    skewness = skewness(!!sym(return_col_name), na.rm = TRUE),
    kurtosis = kurtosis(!!sym(return_col_name), na.rm = TRUE),
    .groups = 'drop'
  )
}


# Define return periods
return_periods <- c("return1M", "return2M", "return3M", "return6M", "return12M")


###################################### APPLICATION
# APPLY CALCULATE GROUP STATS FUNCTION

all_stats <- lapply(return_periods, function(col_name) calculate_group_stats(stocks_portfolio, col_name))


write_xlsx(all_stats[[1]], "1M_descriptives.xlsx")
write_xlsx(all_stats[[2]], "2M_descriptives.xlsx")
write_xlsx(all_stats[[3]], "3M_descriptives.xlsx")
write_xlsx(all_stats[[4]], "6M_descriptives.xlsx")
write_xlsx(all_stats[[5]], "12M_descriptives.xlsx")


###################################### T TESTS

stocks_grouped <- stocks_portfolio %>% 
  mutate(group = ifelse( top_30_percent == 1, 1, ifelse( bottom_30_percent == 1, 3, 2))) %>% 
  select(ticker, date, return1M, return2M, return3M, return6M, return12M, group)


# Filter the dataset for groups 1 and 3
group1 <- stocks_grouped %>% filter(group == 1)
group3 <- stocks_grouped %>% filter(group == 3)


# Conduct t-tests for each return period and store results
t_test_results <- list(
  "1M" = t.test(group1$return1M, group3$return1M),
  "2M" = t.test(group1$return2M, group3$return2M),
  "3M" = t.test(group1$return3M, group3$return3M),
  "6M" = t.test(group1$return6M, group3$return6M),
  "12M" = t.test(group1$return12M, group3$return12M)
)


r1M_t_test <- tidy(t_test_results[[1]])
r2M_t_test <- tidy(t_test_results[[2]])
r3M_t_test <- tidy(t_test_results[[3]])
r6M_t_test <- tidy(t_test_results[[4]])
r12M_t_test <- tidy(t_test_results[[5]])


write_xlsx(r1M_t_test, "r1M_t_test.xlsx")
write_xlsx(r2M_t_test, "r2M_t_test.xlsx")
write_xlsx(r3M_t_test, "r3M_t_test.xlsx")
write_xlsx(r6M_t_test, "r6M_t_test.xlsx")
write_xlsx(r12M_t_test, "r12M_t_test.xlsx")


######################## WINNERS T TEST DIFFERENCE FROM 0

group1_data <- stocks_grouped %>%
  filter(group == 1)

# Perform one-sample t-tests
t_test_1M_w0 <- t.test(group1_data$return1M, mu = 0)
t_test_2M_w0 <- t.test(group1_data$return2M, mu = 0)
t_test_3M_w0 <- t.test(group1_data$return3M, mu = 0)
t_test_6M_w0 <- t.test(group1_data$return6M, mu = 0)
t_test_12M_w0 <- t.test(group1_data$return12M, mu = 0)


# Print the results
list(
  `1M Return Test` = t_test_1M_w0,
  `2M Return Test` = t_test_2M_w0,
  `3M Return Test` = t_test_3M_w0,
  `6M Return Test` = t_test_6M_w0,
  `12M Return Test` = t_test_12M_w0
)


t_test_1M_w0 <- tidy(t_test_1M_w0)
t_test_2M_w0 <- tidy(t_test_2M_w0)
t_test_3M_w0 <- tidy(t_test_3M_w0)
t_test_6M_w0 <- tidy(t_test_6M_w0)
t_test_12M_w0 <- tidy(t_test_12M_w0)


write_xlsx(t_test_1M_w0, "t_test_1M_w0.xlsx")
write_xlsx(t_test_2M_w0, "t_test_2M_w0.xlsx")
write_xlsx(t_test_3M_w0, "t_test_3M_w0.xlsx")
write_xlsx(t_test_6M_w0, "t_test_6M_w0.xlsx")
write_xlsx(t_test_12M_w0, "t_test_12M_w0.xlsx")


### JANUARY EFFECT


stocks_january_effect <- stocks_grouped %>% 
  select(ticker, date, return1M, group)


mean_return1M_excl_dec <- stocks_grouped %>%
  filter(month(date) != 12) %>% # Exclude December
  summarise(mean_return1M = mean(return1M, na.rm = TRUE)) 

stocks_january_effect <- stocks_january_effect %>%
  arrange(ticker, date) %>%
  group_by(ticker) %>%
  mutate(return1M = ifelse(month(date) == 12, mean_return1M_excl_dec[[1]], return1M)) %>% 
  mutate(
    # Calculate future returns for 2M, 3M, 6M, and 12M
    future_return1M = lead(return1M, 1),
    future_return2M = lead(return1M, 2),
    future_return3M = lead(return1M, 3),
    future_return4M = lead(return1M, 4),
    future_return5M = lead(return1M, 5),
    future_return6M = lead(return1M, 6),
    future_return7M = lead(return1M, 7),
    future_return8M = lead(return1M, 8),
    future_return9M = lead(return1M, 9),
    future_return10M = lead(return1M, 10),
    future_return11M = lead(return1M, 11),
    # Compute compounded returns
    return2M = (1 + return1M) * (1 + future_return1M) - 1,
    return3M = (1 + return1M) * (1 + future_return1M) * (1 + future_return2M) - 1,
    return6M = (1 + return1M) * (1 + future_return1M) * (1 + future_return2M) *
      (1 + future_return3M) * (1 + future_return4M) * (1 + future_return5M) - 1,
    return12M = (1 + return1M) * (1 + future_return1M) * (1 + future_return2M) *
      (1 + future_return3M) * (1 + future_return4M) * (1 + future_return5M) * (1 + future_return6M) *
      (1 + future_return7M) * (1 + future_return8M) * (1 + future_return9M) * (1 + future_return10M) *
      (1 + future_return11M) - 1
  ) %>%
  # Select relevant columns to tidy up the tibble
  select(ticker, date, return1M, return2M, return3M, return6M, return12M, group) %>%
  ungroup()



################################# FUNCTION
# CALCULATE GROUPS STATS FUNCTION
calculate_group_stats <- function(df, return_col_name) {
  df %>%
    group_by(group) %>%
    summarise(
      mean = mean(!!sym(return_col_name), na.rm = TRUE),
      median = median(!!sym(return_col_name), na.rm = TRUE),
      variance = sd(!!sym(return_col_name), na.rm = TRUE),
      skewness = skewness(!!sym(return_col_name), na.rm = TRUE),
      kurtosis = kurtosis(!!sym(return_col_name), na.rm = TRUE),
      .groups = 'drop'
    )
}


# Define return periods
return_periods <- c("return1M", "return2M", "return3M", "return6M", "return12M")



###################################### APPLICATION
# APPLY CALCULATE GROUP STATS FUNCTION

all_stats_no_jan <- lapply(return_periods, function(col_name) calculate_group_stats(stocks_january_effect, col_name))


write_xlsx(all_stats_no_jan[[1]], "1M_descriptives_no_jan.xlsx")
write_xlsx(all_stats_no_jan[[2]], "2M_descriptives_no_jan.xlsx")
write_xlsx(all_stats_no_jan[[3]], "3M_descriptives_no_jan.xlsx")
write_xlsx(all_stats_no_jan[[4]], "6M_descriptives_no_jan.xlsx")
write_xlsx(all_stats_no_jan[[5]], "12M_descriptives_no_jan.xlsx")



###################################### T TESTS

stocks_january_effect


# Filter the dataset for groups 1 and 3
group1_no_jan <- stocks_january_effect %>% filter(group == 1)
group3_no_jan <- stocks_january_effect %>% filter(group == 3)


# Conduct t-tests for each return period and store results
t_test_results <- list(
  "1M" = t.test(group1_no_jan$return1M, group3_no_jan$return1M),
  "2M" = t.test(group1_no_jan$return2M, group3_no_jan$return2M),
  "3M" = t.test(group1_no_jan$return3M, group3_no_jan$return3M),
  "6M" = t.test(group1_no_jan$return6M, group3_no_jan$return6M),
  "12M" = t.test(group1_no_jan$return12M, group3_no_jan$return12M)
)


r1M_t_test_nj <- tidy(t_test_results[[1]])
r2M_t_test_nj <- tidy(t_test_results[[2]])
r3M_t_test_nj <- tidy(t_test_results[[3]])
r6M_t_test_nj <- tidy(t_test_results[[4]])
r12M_t_test_nj <- tidy(t_test_results[[5]])


write_xlsx(r1M_t_test_nj, "r1M_t_test_nj.xlsx")
write_xlsx(r2M_t_test_nj, "r2M_t_test_nj.xlsx")
write_xlsx(r3M_t_test_nj, "r3M_t_test_nj.xlsx")
write_xlsx(r6M_t_test_nj, "r6M_t_test_nj.xlsx")
write_xlsx(r12M_t_test_nj, "r12M_t_test_nj.xlsx")


#==# #==# #==# #==# #==# #==# #==# #==# #==# #==# 
#==# #==#    TRADING STRATEGY           #==# #==# WINNERS!
#==# #==# #==# #==# #==# #==# #==# #==# #==# #==#


stocks_portfolio %>% 
select(ticker, date, priceClose, top_30_percent)


# Step 1: Sort your data by ticker and date to ensure correct processing
stocks_portfolio <- stocks_portfolio %>% arrange(ticker, date)

# Initialize the trades data frame with appropriate types without using date()
trades <- data.frame(Ticker = character(), 
                   BuyDate = as.Date(character()), 
                   SellDate = as.Date(character()), 
                   Return = numeric(), stringsAsFactors = FALSE,
                   buy_price = numeric(),
                   sell_price = numeric()
                   )

# Iterate through each ticker
for (ticker_name in unique(stocks_portfolio$ticker)) {
ticker_data <- filter(stocks_portfolio, ticker == ticker_name)

# Variables to keep track of buy signals and holding period
holding <- FALSE
hold_until <- as.Date("1900-01-01")
buy_price <- 0
buy_date <- as.Date("1900-01-01")

for (i in 1:nrow(ticker_data)) {
  row <- ticker_data[i,]
  if (row$top_30_percent == 1) {
    if (!holding) {
      # Buy signal
      holding <- TRUE
      hold_until <- row$date %m+% months(6)
      buy_price <- row$priceClose
      buy_date <- row$date
    } else if (row$date <= hold_until) {
      # Extend holding period if reappears in the top 30% during holding
      hold_until <- row$date %m+% months(5)
    }
  }
  
  # Check if it's time to sell
  if (holding && (row$date > hold_until || i == nrow(ticker_data))) {
    sell_price <- row$priceClose
    sell_date <- row$date
    holding <- FALSE
    trade_return <- (sell_price - buy_price) / buy_price
    trades <- rbind(trades, data.frame(Ticker = ticker_name, BuyDate = buy_date, SellDate = sell_date, Return = trade_return, buy_price = buy_price, sell_price = sell_price))
  }
}
}

#########################
# Delete 0 return trades

winners_trades <- as_tibble(trades) %>% 
filter (buy_price != sell_price)


#==# #==# #==# #==# #==# #==# #==# #==# #==# #==# 
#==# #==#    CALCULATE RETURNS          #==# #==#
#==# #==# #==# #==# #==# #==# #==# #==# #==# #==#




############################
# Calculate monthly returns


winners_trades_df <- as_tibble(winners_trades) %>%
mutate(
  BuyDate = as.Date(BuyDate),
  SellDate = as.Date(SellDate),
  # Calculate the number of months between BuyDate and SellDate
  Months = interval(BuyDate, SellDate) / months(1),
  # Calculate the monthly return
  MonthlyReturn = Return / Months
)


# Calculate basic statistics
w_mean_return <- mean(winners_trades_df$MonthlyReturn)
w_median_return <- median(winners_trades_df$MonthlyReturn)
w_variance_return <- var(winners_trades_df$MonthlyReturn)
w_skewness_return <- skewness(winners_trades_df$MonthlyReturn)
w_kurtosis_return <- kurtosis(winners_trades_df$MonthlyReturn)
w_std_error_return <- sd(winners_trades_df$MonthlyReturn) / sqrt(length(winners_trades_df$MonthlyReturn))

# Calculate the 95% confidence interval
alpha <- 0.05
z <- qnorm(1 - alpha/2)
w_ci_lower <- w_mean_return - z * w_std_error_return
w_ci_upper <- w_mean_return + z * w_std_error_return

# Print the results
cat("Mean:", w_mean_return, "\n")
cat("Median:", w_median_return, "\n")
cat("Variance:", w_variance_return, "\n")
cat("Skewness:", w_skewness_return, "\n")
cat("Kurtosis:", w_kurtosis_return, "\n")
cat("Standard Error:", w_std_error_return, "\n")
cat("95% Confidence Interval: [", w_ci_lower, ",", w_ci_upper, "]\n")


# Create tibble

winners_data <- tibble(
  Metric = c("Mean", "Median", "Variance", "Skewness", "Kurtosis", "Standard error", "95%CI lower", "95%CI upper"),
  Value = c(w_mean_return, w_median_return, w_variance_return, w_skewness_return, w_kurtosis_return, w_std_error_return, w_ci_lower, w_ci_upper )
)

write_xlsx(winners_data, "losers_data.xlsx")

#########################
# PLOTTING
#########################


windowsFonts(TNR = windowsFont("Times New Roman"))


# Filter data

w_lower_bound <- quantile(winners_trades_df$MonthlyReturn, 0.01, na.rm = TRUE)
w_upper_bound <- quantile(winners_trades_df$MonthlyReturn, 0.99, na.rm = TRUE)

# Filter the dataset to exclude the lower and upper 1%
filtered_winners_trades_df <- winners_trades_df %>%
filter(MonthlyReturn > w_lower_bound & MonthlyReturn < w_upper_bound)

# Create the plot
ggplot(filtered_winners_trades_df, aes(x = MonthlyReturn)) +
geom_histogram(aes(y = ..density..), binwidth = 0.005, fill = "darkgray", alpha = 0.7) +
geom_vline(xintercept = w_mean_return, color = "black", size = 1) +
geom_vline(xintercept = w_median_return, color = "black", linetype = "dashed", size = 1) +
geom_vline(xintercept = w_ci_lower, color = "blue", size = 1) +
geom_vline(xintercept = w_ci_upper, color = "blue", size = 1) +
annotate("text", x = w_mean_return, y = Inf, label = "Átlag", vjust = 15, color = "black") +
annotate("text", x = w_median_return, y = Inf, label = "Medián", vjust = 13, color = "black") +
annotate("text", x = w_ci_lower, y = Inf, label = "CI95% (alsó)", vjust = 10, color = "blue") +
annotate("text", x = w_ci_upper, y = Inf, label = "CI95% (felső)", vjust = 18, color = "blue") +
labs(title = "A Nyerteseket vásárló stratégia havi átlagos hozamainak eloszlása",
     subtitle = "1. és 99. percentilis közötti értékek",
     x = "Havi átlagos hozam",
     y = "Sűrűség") +
theme_minimal(base_family = "TNR") +
theme(text = element_text(size = 12))
theme(text = element_text(family = "TNR", size = 12))









#==# #==# #==# #==# #==# #==# #==# #==# #==# #==# 
#==# #==#    TRADING STRATEGY           #==# #==# LOSERS!
#==# #==# #==# #==# #==# #==# #==# #==# #==# #==#


stocks_portfolio %>% 
  select(ticker, date, priceClose, bottom_30_percent)


# Step 1: Sort your data by ticker and date to ensure correct processing
stocks_portfolio <- stocks_portfolio %>% arrange(ticker, date)

# Initialize the trades data frame with appropriate types without using date()
trades <- data.frame(Ticker = character(), 
                     BuyDate = as.Date(character()), 
                     SellDate = as.Date(character()), 
                     Return = numeric(), stringsAsFactors = FALSE,
                     buy_price = numeric(),
                     sell_price = numeric()
)

# Iterate through each ticker
for (ticker_name in unique(stocks_portfolio$ticker)) {
  ticker_data <- filter(stocks_portfolio, ticker == ticker_name)
  
  # Variables to keep track of buy signals and holding period
  holding <- FALSE
  hold_until <- as.Date("1900-01-01")
  buy_price <- 0
  buy_date <- as.Date("1900-01-01")
  
  for (i in 1:nrow(ticker_data)) {
    row <- ticker_data[i,]
    if (row$bottom_30_percent == 1) {
      if (!holding) {
        # Buy signal
        holding <- TRUE
        hold_until <- row$date %m+% months(6)
        buy_price <- row$priceClose
        buy_date <- row$date
      } else if (row$date <= hold_until) {
        # Extend holding period if reappears in the top 30% during holding
        hold_until <- row$date %m+% months(5)
      }
    }
    
    # Check if it's time to sell
    if (holding && (row$date > hold_until || i == nrow(ticker_data))) {
      sell_price <- row$priceClose
      sell_date <- row$date
      holding <- FALSE
      trade_return <- (sell_price - buy_price) / buy_price
      trades <- rbind(trades, data.frame(Ticker = ticker_name, BuyDate = buy_date, SellDate = sell_date, Return = trade_return, buy_price = buy_price, sell_price = sell_price))
    }
  }
}

#########################
# Delete 0 return trades

losers_trades <- as_tibble(trades) %>% 
  filter (buy_price != sell_price)


#==# #==# #==# #==# #==# #==# #==# #==# #==# #==# 
#==# #==#    CALCULATE RETURNS          #==# #==#
#==# #==# #==# #==# #==# #==# #==# #==# #==# #==#




############################
# Calculate monthly returns


losers_trades_df <- as_tibble(losers_trades) %>%
  mutate(
    BuyDate = as.Date(BuyDate),
    SellDate = as.Date(SellDate),
    # Calculate the number of months between BuyDate and SellDate
    Months = interval(BuyDate, SellDate) / months(1),
    # Calculate the monthly return
    MonthlyReturn = Return / Months
  )


losers_trades_df 


# Calculate basic statistics
l_mean_return <- mean(losers_trades_df$MonthlyReturn)
l_median_return <- median(losers_trades_df$MonthlyReturn)
l_variance_return <- var(losers_trades_df$MonthlyReturn)
l_skewness_return <- skewness(losers_trades_df$MonthlyReturn)
l_kurtosis_return <- kurtosis(losers_trades_df$MonthlyReturn)
l_std_error_return <- sd(losers_trades_df$MonthlyReturn) / sqrt(length(losers_trades_df$MonthlyReturn))

# Calculate the 95% confidence interval
alpha <- 0.05
z <- qnorm(1 - alpha/2)
l_ci_lower <- l_mean_return - z * l_std_error_return
l_ci_upper <- l_mean_return + z * l_std_error_return

# Print the results
cat("Mean:", l_mean_return, "\n")
cat("Median:", l_median_return, "\n")
cat("Variance:", l_variance_return, "\n")
cat("Skewness:", l_skewness_return, "\n")
cat("Kurtosis:", l_kurtosis_return, "\n")
cat("Standard Error:", l_std_error_return, "\n")
cat("95% Confidence Interval: [", l_ci_lower, ",", l_ci_upper, "]\n")

# Create tibble

losers_data <- tibble(
  Metric = c("Mean", "Median", "Variance", "Skewness", "Kurtosis", "Standard error", "95%CI lower", "95%CI upper"),
  Value = c(l_mean_return, l_median_return, l_variance_return, l_skewness_return, l_kurtosis_return, l_std_error_return, l_ci_lower, l_ci_upper )
)

write_xlsx(losers_data, "losers_data.xlsx")

#########################
# PLOTTING
#########################


windowsFonts(TNR = windowsFont("Times New Roman"))


# Filter data

l_lower_bound <- quantile(losers_trades_df$MonthlyReturn, 0.01, na.rm = TRUE)
l_upper_bound <- quantile(losers_trades_df$MonthlyReturn, 0.99, na.rm = TRUE)

# Filter the dataset to exclude the lower and upper 1%
filtered_losers_trades <- losers_trades_df %>%
  filter(MonthlyReturn > l_lower_bound & MonthlyReturn < l_upper_bound)

# Create the plot
ggplot(filtered_losers_trades, aes(x = MonthlyReturn)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.005, fill = "darkgray", alpha = 0.7) +
  geom_vline(xintercept = l_mean_return, color = "black", size = 1) +
  geom_vline(xintercept = l_median_return, color = "black", linetype = "dashed", size = 1) +
  geom_vline(xintercept = l_ci_lower, color = "blue", size = 1) +
  geom_vline(xintercept = l_ci_upper, color = "blue", size = 1) +
  annotate("text", x = l_mean_return, y = Inf, label = "Átlag", vjust = 15, color = "black") +
  annotate("text", x = l_median_return, y = Inf, label = "Medián", vjust = 13, color = "black") +
  annotate("text", x = l_ci_lower, y = Inf, label = "CI95% (alsó)", vjust = 10, color = "blue") +
  annotate("text", x = l_ci_upper, y = Inf, label = "CI95% (felső)", vjust = 18, color = "blue") +
  labs(title = "A Veszteseket vásárló stratégia havi átlagos hozamainak eloszlása",
       subtitle = "1. és 99. percentilis közötti értékek",
       x = "Havi átlagos hozam",
       y = "Sűrűség") +
  theme_minimal(base_family = "TNR") +
  theme(text = element_text(size = 12))
theme(text = element_text(family = "TNR", size = 12))







#########################################
#########################################
# PROXIMITY DISCOVERY
#########################################

proximity_stats <- stocks_portfolio %>%
  group_by(date) %>%
  summarize(avg_proximity = mean(proximity),
            max_proximity = max(proximity),
            min_proximity = min(proximity),
            top_30_cutoff = quantile(proximity, 0.7, na.rm = TRUE),
            bottom_30_cutoff = quantile(proximity, 0.3, na.rm = TRUE))


ggplot(proximity_stats, aes(x = date)) +
  geom_ribbon(aes(ymin = min_proximity, ymax = max_proximity, fill = "Difference"), alpha = 0.5) +
  geom_line(aes(y = top_30_cutoff, color = "Top Cutoff")) +  # Top 30% cutoff line
  geom_line(aes(y = avg_proximity, color = "Average")) +     # Average proximity line
  geom_line(aes(y = bottom_30_cutoff, color = "Bottom Cutoff")) +  # Bottom 30% cutoff line
  scale_fill_manual(values = "lightblue", name = NULL, labels = c("Közelség terjedelme")) +
  scale_color_manual(name = NULL, values = c("Top Cutoff" = "green", "Average" = "blue", "Bottom Cutoff" = "red"),
                     labels = c("Top Cutoff" = "Felső küszöbérték", 
                                "Average" = "Átlagos közelség", 
                                "Bottom Cutoff" = "Alsó küszöbérték")) +
  labs(title = "Az 52-hetes csúcshoz való közelség alakulása (1994-2023)",
       x = "Dátum", y = "Közelség") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal(base_family = "TNR") +
  theme(text = element_text(size = 12))



#########################################
#########################################
# Traded companies
#########################################

stocks_portfolio


ggplot(stocks_portfolio, aes(x = date, y = traded_companies)) +
  geom_line(color = "black") +
  labs(title = "A BÉT-en kereskedett vállalatok számának alakulása (1994-2023)",
       x = "Dátum",
       y = "Vállalatok száma") +
  theme_minimal(base_family = "TNR") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme(text = element_text(size = 12)) +
  ylim(0, 60)



#########################################
#########################################
# Returns dotplot
#########################################


return_groups <- stocks_portfolio %>% 
  select(ticker, date, proximity, return6M, top_30_percent, bottom_30_percent) %>% 
  mutate(group = ifelse(top_30_percent == 1, 1, ifelse(bottom_30_percent == 1, 3, 2))) %>% 
  select(ticker, date, proximity, return6M, group)




ggplot(return_groups, aes(x = proximity, y = return6M, color = factor(group))) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, linetype = "solid", color = "white", size = 1) +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "6 hónapos BÉT részvényhozamok az 52 hetes csúcshoz való közelség szerint",
       subtitle = "1994-2023",
       x = "Havi utolsó ár közelsége az 52 hetes csúcshoz (%)",
       y = "6 hónapos hozam (%)",
       color = "Csoport") +
  scale_color_manual(values = c("darkgreen", "orange", "darkred", "black"),
                     labels = c("Győztesek", "Köztesek", "Vesztesek", "Trendline")) +
  theme_minimal(base_family = "TNR") +
  theme(text = element_text(size = 12)) +
  #ylim(-2, 2)
  
  
# FILTERED (1-99 PERCENTILES)

  #culate the 1% and 99% percentiles for return6M
percentiles <- return_groups %>%
  summarise(
    p1 = quantile(return6M, 0.01, na.rm = TRUE),
    p99 = quantile(return6M, 0.99, na.rm = TRUE)
  )

# Filter the data to include only rows where return6M falls within the 1%-99% percentiles
filtered_data <- return_groups %>%
  filter(return6M > percentiles$p1 & return6M < percentiles$p99)

# Plot the filtered data
ggplot(filtered_data, aes(x = proximity, y = return6M, color = factor(group))) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, linetype = "solid", color = "white", size = 1) +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "6 hónapos BÉT részvényhozamok az 52 hetes csúcshoz való közelség szerint",
       subtitle = "1994-2023 | 1. és 99. percentilis közötti értékek",
       x = "Havi utolsó ár közelsége az 52 hetes csúcshoz (%)",
       y = "6 hónapos hozam (%)",
       color = "Csoport") +
  scale_color_manual(values = c("darkgreen", "orange", "darkred", "black"),
                     labels = c("Győztesek", "Köztesek", "Vesztesek", "Trendline")) +
  theme_minimal(base_family = "TNR") +
  theme(text = element_text(size = 12)) 

# TREND LINE LM MODEL

return_groups <- return_groups %>% 
  drop_na()
model <- lm(return6M ~ proximity, data = return_groups)
model <- tidy(model)
write.xlsx(model, "model.xlsx")

filtered_data <- filtered_data %>% 
  drop_na()
model <- lm(return6M ~ proximity, data = filtered_data)
model <- tidy(model)
write.xlsx(model, "filtered_model.xlsx")


#==# #==# #==# #==# #==# #==# #==# #==# #==# #==# 
#==# #==#              BUX              #==# #==# 
#==# #==# #==# #==# #==# #==# #==# #==# #==# #==#

bux <- read.csv("BUX_data.csv")

# Tibble

bux <- as_tibble(bux) %>% # Full join
  rename( # Rename columns
    ticker = "Név",
    date = "Dátum",
    priceClose = "Utolsó.ár",
  ) %>% 
  mutate(date = as.Date(paste0(date, "01"), format = "%Y.%m.%d")) %>% # Change date format
  select(ticker, date, priceClose) # Select useful columns

# Returns

bux <- bux %>%
  arrange(date) %>%
  group_by(ticker) %>%
  mutate(monthly_return = (priceClose / lag(priceClose) - 1)) %>% 
  filter(date > "1993-12-01")

# Compound returns

bux <- bux %>%
  mutate(Cumulative_Compound_Return = ifelse(row_number() == 1, 0, cumprod(1 + monthly_return) - 1))


ggplot(bux, aes(x = date, y = Cumulative_Compound_Return)) +
  geom_line() +
  labs(title = "BUX Compound Returns Over Time",
       x = "Date",
       y = "Cumulative Compound Return") +
  theme_minimal()

##########################
# Calculate basic statistics
bux_mean_return <- mean(bux$monthly_return)
bux_median_return <- median(bux$monthly_return)
bux_variance_return <- var(bux$monthly_return)
bux_skewness_return <- skewness(bux$monthly_return)
bux_kurtosis_return <- kurtosis(bux$monthly_return)
bux_std_error_return <- sd(bux$monthly_return) / sqrt(length(bux$monthly_return))

# Calculate the 95% confidence interval
alpha <- 0.05
z <- qnorm(1 - alpha/2)
bux_ci_lower <- bux_mean_return - z * bux_std_error_return
bux_ci_upper <- bux_mean_return + z * bux_std_error_return

# Print the results
cat("Mean:", bux_mean_return, "\n")
cat("Median:", bux_median_return, "\n")
cat("Variance:", bux_variance_return, "\n")
cat("Skewness:", bux_skewness_return, "\n")
cat("Kurtosis:", bux_kurtosis_return, "\n")
cat("Standard Error:", bux_std_error_return, "\n")
cat("95% Confidence Interval: [", bux_ci_lower, ",", bux_ci_upper, "]\n")


# Create tibble

bux_statistics <- tibble(
  Metric = c("Mean", "Median", "Variance", "Skewness", "Kurtosis", "Standard error", "95%CI lower", "95%CI upper"),
  Value = c(bux_mean_return, bux_median_return, bux_variance_return, bux_skewness_return, bux_kurtosis_return, bux_std_error_return, bux_ci_lower, bux_ci_upper )
)

write_xlsx(bux_statistics, "bux_statistics.xlsx")


### STOCKS RETURNS TIBBLE
stocks_returns <- stocks_portfolio %>% 
  select(ticker, date, priceClose, return1M)


#==# #==# #==# #==# #==# #==# #==# #==# #==# #==# 
#==# #==#       PORTFOLIO RETURNS       #==# #==# LOSERS!
#==# #==# #==# #==# #==# #==# #==# #==# #==# #==#

losers_trades_df

################################
# SHOWING WHEN THE STOCK IS HELD

losers_held_stocks <- stocks_returns %>%
  inner_join(losers_trades, by = c("ticker" = "Ticker"), relationship = "many-to-many") %>%
  mutate(Held = date > BuyDate & date <= SellDate) %>%
  select(ticker, date, `1M_return` = return1M, Held) %>% 
  group_by(ticker, date) %>%
  summarise(
    `1M_return` = first(`1M_return`), # Assuming `1M_return` is the same for duplicate ticker-date combinations
    Held = any(Held),
    .groups = 'drop' # Drops the grouping structure
  )

print(losers_held_stocks)

################################
# ASSIGNING PERFORMANCE TO DATES

# 1. Generate all monthly dates between 1994-01-01 and 2023-12-01
all_dates <- seq(from = as.Date("1994-01-01"), to = as.Date("2023-12-01"), by = "1 month")
monthly_dates <- tibble(date = all_dates)

# 2. Assign the average monthly return based on the held_stocks tibble
# First, we need to prepare a summary of returns for each month where stocks are held
monthly_avg_returns <- losers_held_stocks %>%
  # Ensure that stocks are considered held from the day after the BuyDate
  filter(Held) %>%
  group_by(date) %>%
  summarise(Avg_Return = mean(`1M_return`, na.rm = TRUE), .groups = 'drop')

# Merge the monthly_dates with monthly_avg_returns to assign returns
losers_monthly_performance <- monthly_dates %>%
  left_join(monthly_avg_returns, by = c("date")) %>%
  # Assign 0 to months where no stocks are held
  mutate(Avg_Return = if_else(is.na(Avg_Return), 0, Avg_Return))

# View the result
print(losers_monthly_performance)

############################
# Calculate basic statistics
l_mean_return <- mean(losers_monthly_performance$Avg_Return)
l_median_return <- median(losers_monthly_performance$Avg_Return)
l_variance_return <- var(losers_monthly_performance$Avg_Return)
l_skewness_return <- skewness(losers_monthly_performance$Avg_Return)
l_kurtosis_return <- kurtosis(losers_monthly_performance$Avg_Return)
l_std_error_return <- sd(losers_monthly_performance$Avg_Return) / sqrt(length(losers_monthly_performance$Avg_Return))

# Calculate the 95% confidence interval
alpha <- 0.05
z <- qnorm(1 - alpha/2)
l_ci_lower <- l_mean_return - z * l_std_error_return
l_ci_upper <- l_mean_return + z * l_std_error_return


# Print the results
cat("Mean:", l_mean_return, "\n")
cat("Median:", l_median_return, "\n")
cat("Variance:", l_variance_return, "\n")
cat("Skewness:", l_skewness_return, "\n")
cat("Kurtosis:", l_kurtosis_return, "\n")
cat("Standard Error:", l_std_error_return, "\n")
cat("95% Confidence Interval: [", l_ci_lower, ",", l_ci_upper, "]\n")

# Create tibble

losers_data <- tibble(
  Metric = c("Mean", "Median", "Variance", "Skewness", "Kurtosis", "Standard error", "95%CI lower", "95%CI upper"),
  Value = c(l_mean_return, l_median_return, l_variance_return, l_skewness_return, l_kurtosis_return, l_std_error_return, l_ci_lower, l_ci_upper )
)

# write_xlsx(losers_data, "losers_data.xlsx")



#########################
# PLOTTING
#########################


windowsFonts(TNR = windowsFont("Times New Roman"))


# Filter data

l_lower_bound <- quantile(losers_monthly_performance$Avg_Return, 0.01, na.rm = TRUE)
l_upper_bound <- quantile(losers_monthly_performance$Avg_Return, 0.99, na.rm = TRUE)

# Filtering the dataset to exclude the lower and upper 1%
filtered_losers_trades_df <- losers_monthly_performance %>%
  filter(Avg_Return > l_lower_bound & Avg_Return < l_upper_bound)

# Create the plot using the filtered data
ggplot(filtered_losers_trades_df, aes(x = Avg_Return)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.005, fill = "darkgray", alpha = 0.7) +
  geom_vline(xintercept = l_mean_return, color = "black", size = 1) +
  geom_vline(xintercept = l_median_return, color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = l_mean_return, y = 5, label = "Átlag", vjust = 0, color = "black") +
  annotate("text", x = l_median_return, y = 3, label = "Medián", vjust = 0, color = "black") +
  labs(title = "A Veszteseket vásárló stratégia havi átlagos hozamainak eloszlása",
       x = "Havi átlagos hozam",
       y = "Sűrűség") +
  scale_x_continuous(limits = c(-0.4, 0.4)) + 
  theme_minimal(base_family = "TNR") +
  theme(text = element_text(size = 12))


###########################
# CALCULATE COMPOUNT RETURN

l_compound_return <- prod(1 + losers_monthly_performance$Avg_Return) - 1

# Calculate the compound return
losers_monthly_performance <- losers_monthly_performance %>%
  mutate(Cumulative_Compound_Return = cumprod(1 + Avg_Return) - 1)

# Plot

ggplot(losers_monthly_performance, aes(x = date, y = Cumulative_Compound_Return)) +
  geom_line() +
  labs(title = "Cumulative Compound Return Over Time",
       x = "Date",
       y = "Cumulative Compound Return") +
  theme_minimal()








#==# #==# #==# #==# #==# #==# #==# #==# #==# #==# 
#==# #==#       PORTFOLIO RETURNS       #==# #==# WINNERS!
#==# #==# #==# #==# #==# #==# #==# #==# #==# #==#

winners_trades_df

################################
# SHOWING WHEN THE STOCK IS HELD

winners_held_stocks <- stocks_returns %>%
  inner_join(winners_trades, by = c("ticker" = "Ticker"), relationship = "many-to-many") %>%
  mutate(Held = date > BuyDate & date <= SellDate) %>%
  select(ticker, date, `1M_return` = return1M, Held) %>% 
  group_by(ticker, date) %>%
  summarise(
    `1M_return` = first(`1M_return`), # Assuming `1M_return` is the same for duplicate ticker-date combinations
    Held = any(Held),
    .groups = 'drop' # Drops the grouping structure
  )

print(winners_held_stocks)

################################
# ASSIGNING PERFORMANCE TO DATES

# 1. Generate all monthly dates between 1994-01-01 and 2023-12-01
all_dates <- seq(from = as.Date("1994-01-01"), to = as.Date("2023-12-01"), by = "1 month")
monthly_dates <- tibble(date = all_dates)

# 2. Assign the average monthly return based on the held_stocks tibble
# First, we need to prepare a summary of returns for each month where stocks are held
monthly_avg_returns <- winners_held_stocks %>%
  # Ensure that stocks are considered held from the day after the BuyDate
  filter(Held) %>%
  group_by(date) %>%
  summarise(Avg_Return = mean(`1M_return`, na.rm = TRUE), .groups = 'drop')

# Merge the monthly_dates with monthly_avg_returns to assign returns
winners_monthly_performance <- monthly_dates %>%
  left_join(monthly_avg_returns, by = c("date")) %>%
  # Assign 0 to months where no stocks are held
  mutate(Avg_Return = if_else(is.na(Avg_Return), 0, Avg_Return))

# View the result
print(winners_monthly_performance)

############################


# Calculate basic statistics
w_mean_return <- mean(winners_monthly_performance$Avg_Return)
w_median_return <- median(winners_monthly_performance$Avg_Return)
w_variance_return <- var(winners_monthly_performance$Avg_Return)
w_skewness_return <- skewness(winners_monthly_performance$Avg_Return)
w_kurtosis_return <- kurtosis(winners_monthly_performance$Avg_Return)
w_std_error_return <- sd(winners_monthly_performance$Avg_Return) / sqrt(length(winners_monthly_performance$Avg_Return))

# Calculate the 95% confidence interval
alpha <- 0.05
z <- qnorm(1 - alpha/2)
w_ci_lower <- w_mean_return - z * w_std_error_return
w_ci_upper <- w_mean_return + z * w_std_error_return

# Print the results
cat("Mean:", w_mean_return, "\n")
cat("Median:", w_median_return, "\n")
cat("Variance:", w_variance_return, "\n")
cat("Skewness:", w_skewness_return, "\n")
cat("Kurtosis:", w_kurtosis_return, "\n")
cat("Standard Error:", w_std_error_return, "\n")
cat("95% Confidence Interval: [", w_ci_lower, ",", w_ci_upper, "]\n")


# Create tibble

winners_data <- tibble(
  Metric = c("Mean", "Median", "Variance", "Skewness", "Kurtosis", "Standard error", "95%CI lower", "95%CI upper"),
  Value = c(w_mean_return, w_median_return, w_variance_return, w_skewness_return, w_kurtosis_return, w_std_error_return, w_ci_lower, w_ci_upper )
)

# write_xlsx(winners_data, "winners_data.xlsx")

#########################
# PLOTTING
#########################


windowsFonts(TNR = windowsFont("Times New Roman"))


# Filter data

w_lower_bound <- quantile(winners_monthly_performance$Avg_Return, 0.01, na.rm = TRUE)
w_upper_bound <- quantile(winners_monthly_performance$Avg_Return, 0.99, na.rm = TRUE)

# Filtering the dataset to exclude the lower and upper 1%
filtered_winners_trades_df <- winners_monthly_performance %>%
  filter(Avg_Return > w_lower_bound & Avg_Return < w_upper_bound)

# Create the plot using the filtered data
ggplot(filtered_winners_trades_df, aes(x = Avg_Return)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.005, fill = "darkgray", alpha = 0.7) +
  geom_vline(xintercept = w_mean_return, color = "black", size = 1) +
  geom_vline(xintercept = w_median_return, color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = w_mean_return, y = 5, label = "Átlag", vjust = 0, color = "black") +
  annotate("text", x = w_median_return, y = 3, label = "Medián", vjust = 0, color = "black") +
  labs(title = "A Győzteseket vásárló stratégia havi átlagos hozamainak eloszlása",
       x = "Havi átlagos hozam",
       y = "Sűrűség") +
  scale_x_continuous(limits = c(-0.4, 0.4)) + 
  theme_minimal(base_family = "TNR") +
  theme(text = element_text(size = 12))


###########################
# CALCULATE COMPOUNT RETURN

w_compound_return <- prod(1 + winners_monthly_performance$Avg_Return) - 1

# Calculate the compound return
winners_monthly_performance <- winners_monthly_performance %>%
  mutate(Cumulative_Compound_Return = cumprod(1 + Avg_Return) - 1)

# Plot

ggplot(winners_monthly_performance, aes(x = date, y = Cumulative_Compound_Return)) +
  geom_line() +
  labs(title = "Cumulative Compound Return Over Time",
       x = "Date",
       y = "Cumulative Compound Return") +
  theme_minimal()



#==# #==# #==# #==# #==# #==# #==# #==# #==# #==# 
#==# #==#   COMPOUND RETURN COMPARISON  #==# #==# 
#==# #==# #==# #==# #==# #==# #==# #==# #==# #==#

winners_monthly_performance
losers_monthly_performance

# Combined tibble

compound_returns <- winners_monthly_performance %>%
  inner_join(losers_monthly_performance, by = "date", suffix = c("_winners", "_losers")) %>%
  select(date, 
         return_losers = Avg_Return_losers, 
         compound_return_losers = Cumulative_Compound_Return_losers, 
         return_winners = Avg_Return_winners, 
         compound_return_winners = Cumulative_Compound_Return_winners)


# Plot

ggplot(compound_returns, aes(x = date)) +
  geom_line(aes(y = compound_return_losers, color = "Vesztesek")) +
  geom_line(aes(y = compound_return_winners, color = "Győztesek")) +
  labs(title = "A Győztes és a Vesztes portfólió teljes hozama a vizsgált időszakban",
       x = "Dátum",
       y = "Teljes hozam",
       color = "Csoport") +
  scale_color_manual(values = c("Vesztesek" = "red", "Győztesek" = "blue")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +  # Adjust the date breaks
  theme_minimal(base_family = "TNR") +
  theme(text = element_text(size = 12))

###########################
# Combined tibble with BUX

compound_returns_bux <- compound_returns %>%
  inner_join(bux, by = "date") %>%
  select(-ticker, -priceClose) %>% 
  rename(return_bux = monthly_return,
         compound_return_bux = Cumulative_Compound_Return)

compound_returns_bux_a <- compound_returns_bux %>% 
  filter(date < "2017-01-01")

# Plot

ggplot(compound_returns_bux_a, aes(x = date)) +
  geom_line(aes(y = compound_return_losers, color = "Vesztesek")) +
  geom_line(aes(y = compound_return_winners, color = "Győztesek")) +
  geom_line(aes(y = compound_return_bux, color = "BUX")) +
  labs(title = "A Győztesek, a Vesztesek és a BUX teljes hozama (1994-2017)",
       x = "Dátum",
       y = "Teljes hozam",
       color = "Csoport") +
  scale_color_manual(values = c("Vesztesek" = "red", "Győztesek" = "blue", "BUX" = "black")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal(base_family = "TNR") +
  theme(text = element_text(size = 12))


###########################
# T TESTING

##
# Perform t-test between winners and bux
t_test_winners_bux <- t.test(compound_returns_bux$return_winners, compound_returns_bux$return_bux)

# Print the result
t_test_winners_bux

##
# Perform t-test between winners and losers
t_test_winners_losers <- t.test(compound_returns_bux$return_winners, compound_returns_bux$return_losers)

# Print the result
t_test_winners_losers

##
# Perform t-test between losers and bux
t_test_losers_bux <- t.test(compound_returns_bux$return_losers, compound_returns_bux$return_bux)

# Print the result
t_test_losers_bux

t_test_winners_bux <- tidy(t_test_winners_bux)
t_test_winners_losers <- tidy(t_test_winners_losers)
t_test_losers_bux <- tidy(t_test_losers_bux)

write_xlsx(t_test_winners_bux, "t_test_winners_bux.xlsx")
write_xlsx(t_test_winners_losers, "t_test_winners_losers.xlsx")
write_xlsx(t_test_losers_bux, "t_test_losers_bux.xlsx")

