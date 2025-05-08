# --- Lab Step 1: Install and Load Packages ---

# Install necessary packages (only run if not already installed)
packages <- c("tseries", "forecast", "TTR", "ggplot2", "dplyr", "tidyr", "lubridate")
installed_packages <- installed.packages()[, "Package"]
packages_to_install <- packages[!packages %in% installed_packages]
if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}

# Load the libraries
library(tseries)     # For time-series data manipulation and testing
library(forecast)    # For forecasting time-series models
library(TTR)         # For technical trading rules, including moving averages (SMA)
library(ggplot2)     # For data visualization
library(dplyr)       # For data manipulation
library(tidyr)       # For reshaping data (pivot_longer)
library(lubridate)   # For working with dates

# --- Lab Step 2: Download and Import Data ---
# (Data Download steps are manual and not included in the code)

# Set working directory (update the path as needed)
# This is where R will look for your data files and save outputs.
# setwd("C:/Users/your_name/Documents/Lab2") # <-- Update this path!
getwd() # Print current working directory to confirm

# --- Lab Step 2C: Load the Data into R ---

# Load the soybean price dataset
# Assuming the file is saved as 'soybean-prices-historical-chart-data_ag_econ.csv'
# This file likely contains daily data with 'date' and 'value' columns.
# Based on your previous code, the date format in this file seems to be '%Y-%m-%d'.
soybeans_daily <- read.csv("soybean-prices-historical-chart-data_ag_econ.csv")
print("Head of raw daily soybeans data:")
print(head(soybeans_daily, 3))
# Convert the date column to Date objects, specifying the format
# Assuming the date format in soybeans_ag_econ.csv is '%Y-%m-%d'.
soybeans_daily$date <- as.Date(soybeans_daily$date, format = '%Y-%m-%d')
# Check the structure to confirm date is a Date object
# str(soybeans_daily) # Uncomment to inspect structure


# Load the CPI dataset
# Assuming the file is saved as 'historical-cpi-u-202502_Ag_Econ.csv'
# This file likely contains a year column and month abbreviation columns (Jan., Feb., etc.) with CPI values.
CPI_raw <- read.csv("historical-cpi-u-202502_Ag_Econ.csv")
print("Head of raw CPI data:")
print(head(CPI_raw, 3))

# --- DEBUGGING STEP: Print column names of CPI_raw ---
# This will show you the exact name of the year column to use in pivot_longer
print("Column names of CPI_raw data frame:")
print(colnames(CPI_raw))
# --- End Debugging Step ---

# Check the structure
# str(CPI_raw) # Uncomment to inspect structure

# --- Lab Step 3: Pivot CPI data ---

# Transform CPI data from wide to long format
# Use pivot_longer to gather the monthly columns
# FIX: Replace 'Year' with the actual name of the year column from the colnames() output above.
# A common alternative name is 'ï..Year' if the file has a Byte Order Mark (BOM).
# If the output of colnames(CPI_raw) shows something like "ï..Year", use -`ï..Year` below.
# If it shows "year", use -year.
# If it shows "Year" exactly, the issue might be elsewhere, but start by checking the name.
# Use backticks `` ` `` around the name if it contains special characters or spaces.
CPI_long <- CPI_raw %>%
  pivot_longer(
    cols = -`Year`, # <--- *** FIX THIS: Replace 'Year' with the actual year column name from colnames(CPI_raw) output ***
    names_to = 'Month_abbr', # New column for month abbreviations (e.g., Jan., Feb.)
    values_to = 'CPI'      # New column for the CPI values
  ) %>%
  # Clean up month abbreviations (remove '.') and convert to month numbers
  mutate(Month = match(gsub("\\.", "", Month_abbr), month.abb)) %>%
  # Create a proper Date object for the first day of each month
  # Use make_date from lubridate for robustness
  # Assuming the year column is now correctly handled by pivot_longer and is available.
  mutate(date = make_date(Year, Month, 1)) %>%
  # Select the relevant columns (date and CPI) and arrange by date
  select(date, CPI) %>%
  arrange(date)

print("Head of processed (pivoted and dated) CPI data:")
head(CPI_long)
print("Structure of processed CPI data:")
str(CPI_long) # Verify that 'date' is a Date object

# --- Lab Step 4: Transform soybean data to monthly average and filter ---

# Transform the daily soybean data to monthly averages
# 1. Convert the date column to Date objects (already done after loading)

# 2. Calculate the monthly average from your daily soybean price data
soybeans_monthly <- soybeans_daily %>%
  mutate(date = floor_date(date, "month")) %>% # Set each date to the first of the month
  group_by(date) %>%                         # Group by the first of the month
  summarize(price = mean(value, na.rm = TRUE)) %>% # Calculate the average price for each month
  ungroup()                                  # Remove grouping

print("Head of monthly averaged soybeans data:")
head(soybeans_monthly)
print("Tail of monthly averaged soybeans data:")
tail(soybeans_monthly)
print("Structure of monthly averaged soybeans data:")
str(soybeans_monthly) # Verify that 'date' is a Date object

# Filter the monthly soybean data from January 1969 to March 2025
# Ensure the date format in the filter matches the Date objects
soybeans_filtered <- soybeans_monthly %>%
  filter(date >= as.Date("1969-01-01") & date <= as.Date("2025-03-01")) # Filter up to the first day of March 2025

print("Head of filtered monthly soybeans data (1969-2025):")
head(soybeans_filtered)
print("Tail of filtered monthly soybeans data (1969-2025):")
tail(soybeans_filtered)


# Filter CPI data from January 1969 to March 2025
# Ensure the date format in the filter matches the Date objects in CPI_long
CPI_filtered <- CPI_long %>%
  filter(date >= as.Date("1969-01-01") & date <= as.Date("2025-03-01")) # Filter up to the first day of March 2025

print("Head of filtered CPI data (1969-2025):")
head(CPI_filtered)
print("Tail of filtered CPI data (1969-2025):")
tail(CPI_filtered)
print("Structure of filtered CPI data:")
str(CPI_filtered) # Verify date is still Date object

# --- Lab Step 5: Merge the soybeans and CPI data frames by date ---

# Merge the filtered soybean and CPI dataframes into a new dataframe called soybeans_merged
# Use inner_join to keep only dates present in both data frames
soybeans_merged <- inner_join(soybeans_filtered, CPI_filtered, by = "date")

print("Head of merged soybean and CPI data:")
head(soybeans_merged)
print("Tail of merged soybean and CPI data:")
tail(soybeans_merged)
print("Structure of merged data:")
str(soybeans_merged) # Check the structure to ensure date, price, and CPI are present

# --- Lab Step 6: Convert nominal price to real price ---

# Create a new variable, `price_real`, for real soybean prices using the provided formula
# Real Price_t = Nominal Price_t / (CPI_t / 100)
soybeans_merged <- soybeans_merged %>%
  mutate(price_real = price / (CPI / 100))

print("Head of merged data with real price:")
head(soybeans_merged)
print("Tail of merged data with real price:")
tail(soybeans_merged)
print("Structure of merged data with real price:")
str(soybeans_merged) # Check structure to ensure price_real is added

# --- Lab Step 7: Visualize Soybeans Prices Over Time ---

# Your task: Plot nominal and real prices over time with ggplot()

# Reshape data to long format for plotting multiple lines easily with ggplot2
# This creates a 'Price_Type' column ('Nominal' or 'Real') and a 'Price' column with the values
soybeans_long <- soybeans_merged %>%
  select(date, price, price_real) %>% # Select date and both price columns
  pivot_longer(
    cols = c(price, price_real), # Columns to reshape (nominal and real price columns)
    names_to = "Price_Type",     # New column for original column names ('price', 'price_real')
    values_to = "Price"          # New column for price values
  ) %>%
  # Clean up Price_Type names for better legend labels (change 'price' to 'Nominal', 'price_real' to 'Real')
  mutate(Price_Type = case_when(
    Price_Type == "price" ~ "Nominal",
    Price_Type == "price_real" ~ "Real",
    TRUE ~ Price_Type # Keep other values if any (shouldn't be in this case)
  )) %>%
  # Convert Price_Type to a factor for consistent plotting and legend order
  mutate(Price_Type = factor(Price_Type, levels = c("Nominal", "Real")))

# --- Debugging: Print the unique values in Price_Type to check for mismatches ---
# print("Unique values in soybeans_long$Price_Type:")
# print(unique(soybeans_long$Price_Type))
# print("Levels of soybeans_long$Price_Type (if factor):")
# print(levels(soybeans_long$Price_Type))
# --- End Debugging ---


# Plot Nominal vs Real Soybean Prices over time using the long format data
# Use geom_line for continuous lines
ggplot(data = soybeans_long, aes(x = date, y = Price, color = Price_Type)) +
  geom_line(linewidth = 0.8) + # Use geom_line with a slightly thicker line
  # Set custom colors and legend title
  # The names in 'values' now match the levels/values in the 'Price_Type' column ('Nominal', 'Real')
  scale_color_manual(name = "Price Type", values = c('Nominal' = 'magenta', 'Real' = 'steelblue')) + # Using colors similar to the second image
  # Add title and axis labels
  labs(
    title = "Nominal vs Real Soybean Prices Over Time",
    x = "Date",
    y = "Price (USD)" # Or "Price" as in the example plot
  ) +
  theme_light() # Use theme_light for a background similar to the second image

# Save the graph in your working directory
ggsave("Nominal_vs_Real_Soybean_Prices_Plot.png")


# --- Lab Step 8: Establishing a time-series ---

# We will use the nominal price series for the time series analysis steps.
# Set nominal prices as a time series object in R.
# The ts() function requires the data vector, start date (year and month), end date, and frequency.
# Use the 'price' column from the soybeans_merged data frame.
# The data is monthly, so frequency = 12.
# The start and end dates are the min and max dates in soybeans_merged.

start_date_ts <- min(soybeans_merged$date)
end_date_ts <- max(soybeans_merged$date)

start_year_ts <- year(start_date_ts)
start_month_ts <- month(start_date_ts)

end_year_ts <- year(end_date_ts)
end_month_ts <- month(end_date_ts)

# Create the nominal price time series object
price.ts <- ts(soybeans_merged$price,
               start = c(start_year_ts, start_month_ts),
               end = c(end_year_ts, end_month_ts),
               frequency = 12) # Monthly data

print("Nominal price time series object (first few observations):")
print(head(price.ts))
print("Nominal price time series object (last few observations):")
print(tail(price.ts))
print("Summary of nominal price time series object:")
summary(price.ts)


# --- Lab Step 9: Time-Series Decomposition ---

# Decompose nominal price time-series into trend, seasonal, and random noise components.
# Use the decompose() function with type="additive".
price_components <- decompose(price.ts, type = "additive")

# Plot the decomposed components to visualize trend, seasonal, and remainder.
plot(price_components)

# Access the seasonal components, which are stored as 'figure' in the price_components object.
# These represent the average deviation from the trend for each month.
print("Seasonal components (average monthly deviations from trend):")
print(price_components$figure)

# Identify months with the largest positive and negative seasonal effects:
# Find the month index with the max seasonal value
max_seasonal_month_index <- which.max(price_components$figure)
# Find the month index with the min (largest negative) seasonal value
min_seasonal_month_index <- which.min(price_components$figure)

# Convert month index to month name
month_names <- month.abb # Abbreviated month names (Jan, Feb, ...)
max_seasonal_month <- month_names[max_seasonal_month_index]
min_seasonal_month <- month_names[min_seasonal_month_index]

print(paste("Month with largest positive seasonal effect:", max_seasonal_month, "(Value:", round(price_components$figure[max_seasonal_month_index], 3), ")"))
print(paste("Month with largest negative seasonal effect:", min_seasonal_month, "(Value:", round(price_components$figure[min_seasonal_month_index], 3), ")"))


# --- Lab Step 10: Data smoothing (SMA) ---

# We will now investigate the overall trend using Simple Moving Averages.
# Calculate 3-, 12-, and 48-month simple moving averages of nominal price using the SMA() function from TTR.
price_sma3 <- SMA(price.ts, n = 3)    # 3-month simple moving average
price_sma12 <- SMA(price.ts, n = 12)  # 12-month simple moving average
price_sma48 <- SMA(price.ts, n = 48)  # 48-month simple moving average

# Graph each moving average series in a separate graph using plot.ts().
# Use par(mfrow=c(rows, columns)) to arrange multiple plots in one window.
par(mfrow = c(3, 1)) # Arrange plots in 3 rows and 1 column

# Plot the 3-month moving average
plot.ts(price_sma3, main = "Soybeans Price - 3 Month SMA", xlab = "Date", ylab = "SMA Price ($)")

# Plot the 12-month moving average
plot.ts(price_sma12, main = "Soybeans Price - 12 Month SMA", xlab = "Date", ylab = "SMA Price ($)")

# Plot the 48-month moving average
plot.ts(price_sma48, main = "Soybeans Price - 48 Month SMA", xlab = "Date", ylab = "SMA Price ($)")

# Reset plot layout to default (1 plot per window)
par(mfrow = c(1, 1))

# Save the SMA plots (optional - saving arranged plots can be tricky, saving individually is safer if needed)
# ggsave("Soybeans_Price_SMA_3.png", plot = plot.ts(price_sma3)) # Example of saving one plot


# --- Lab Step 11: Ensuring Stationarity ---

# A non-stationary time-series cannot be reliably forecasted.
# We will convert the non-stationary nominal price series into a stationary one.

# Take the first difference of the monthly soybeans price time-series using the diff() function.
# This calculates the month-to-month change in soybeans prices (Price_t - Price_{t-1}).
price_diff.ts <- diff(price.ts, differences = 1) # differences=1 for the first difference

# Graph the differenced time-series using plot.ts() to visually check for stationarity.
# Look for a constant mean (around zero) and constant variance.
plot.ts(price_diff.ts,
        main = "Month-to-Month Change in Soybeans Prices",
        xlab = "Date",
        ylab = "Price Change ($)")

# Save the differenced plot (optional)
# ggsave("Soybeans_Price_Differenced.png")

# The variance might still appear unstable. A common method to stabilize variance is using logs.
# Take the natural log of soybeans prices using the log() function.
log_price.ts <- log(price.ts)

# Take the first difference of the log-transformed soybeans price time-series.
# This calculates the month-over-month percentage change (approximate).
# (log(Price_t) - log(Price_{t-1})) ≈ (Price_t - Price_{t-1}) / Price_{t-1} = % change
log_price_diff.ts <- diff(log_price.ts, differences = 1) # differences=1 for the first difference

# Plot the log differenced time-series using the plot.ts() function.
# This series should look more stationary (constant mean and variance).
plot.ts(log_price_diff.ts,
        main = "Month-over-Month % Change in Soybeans Prices Over Time",
        xlab = "Date",
        ylab = "% Price Change") # Label as % Price Change

# Save the log-differenced plot (optional)
# ggsave("Soybeans_Log_Price_Differenced.png")


# --- Lab Step 12: Forecasting and Model Evaluation (AR Model) ---

# We will use an auto-regressive (AR) model to forecast the stationary log-differenced series.
# An AR(n) model uses the previous 'n' observations to predict the current value.
# Run an auto-regressive model with 3 lags (AR(3)) on the log-differenced series.
# The ar() function fits an AR model and reports coefficients.
ar_model <- ar(log_price_diff.ts, order.max = 3) # order.max=3 specifies we want to consider up to 3 lags

# Report the coefficients associated with each lag.
# The print() function for an ar object shows the coefficients.
print("AR(3) Model Results (including coefficients):")
print(ar_model)

# The coefficients are listed under "Coefficients:".
# For an AR(3), you will see coefficients for lag 1, lag 2, and lag 3.
# These values represent the estimated impact of the previous 1, 2, and 3 months' percent returns
# on the current month's percent return.

# Check residuals of the AR model to see if there is any remaining autocorrelation.
# Ideally, the ACF bars in the plot should be within the blue significance bounds.
checkresiduals(ar_model, lag.max = 60) # Check autocorrelation up to 60 lags

# Save the residuals plot (optional)
# ggsave("AR3_Residuals_Check.png")


# Let's proceed assuming you want to forecast 6 months after the end of the data used (March 2025).
# Forecast 6 months into the future (h=6) based on the AR(3) model.
forecast_results <- forecast(ar_model, h = 6)

# Display the forecast results.
# This table shows the point forecast and confidence intervals for each future month.
print("6-Month Forecast Results (starting April 2025):")
print(forecast_results)

# Graph your forecast along with the most recent 36 months of historical data.
# autoplot() is a convenient function from the forecast package for plotting forecasts.
# include=36 shows the last 36 historical observations before the forecast starts.
autoplot(forecast_results, include = 36, xlab = "Date", ylab = "Soybeans price - monthly % returns")

# Calculate 3-, 12-, and 48-month simple moving averages of nominal price using the SMA() function from TTR.
price_sma3 <- SMA(price.ts, n = 3)    # 3-month simple moving average
price_sma12 <- SMA(price.ts, n = 12)  # 12-month simple moving average
price_sma48 <- SMA(price.ts, n = 48)  # 48-month simple moving average

# Graph each moving average series in a separate graph using plot.ts().
# Use par(mfrow=c(rows, columns)) to arrange multiple plots in one window.
par(mfrow = c(3, 1), mar = c(4, 4, 2, 2)) # Arrange plots in 3 rows and 1 column

# Plot the 3-month moving average
plot.ts(price_sma3, main = "Soybeans Price - 3 Month SMA", xlab = "Date", ylab = "SMA Price ($)")

# Plot the 12-month moving average
plot.ts(price_sma12, main = "Soybeans Price - 12 Month SMA", xlab = "Date", ylab = "SMA Price ($)")

# Plot the 48-month moving average
plot.ts(price_sma48, main = "Soybeans Price - 48 Month SMA", xlab = "Date", ylab = "SMA Price ($)")

# Reset plot layout to default (1 plot per window)
par(mfrow = c(1, 1))

# Run an auto-regressive model with 3 lags (AR(3)) on the log-differenced series.
ar_model <- ar(log_price_diff.ts, order.max = 3)

# Filter the data to end before the forecast period (end June 2020)
soybeans_data_for_2020_forecast <- soybeans_merged %>%
  filter(date <= as.Date("2020-06-01")) # Filter data up to the first day of June 2020

# Check if the filtered data frame is empty
if (nrow(soybeans_data_for_2020_forecast) == 0) {
  print("Error: Filtered data for 2020 forecast is empty. Check your date filtering.")
} else {
  # Check the end date of this filtered data
  print("End date of data used for 2020 forecast:")
  print(max(soybeans_data_for_2020_forecast$date))
  
  #Create the time series object for nominal price using the filtered data
  start_date_2020 <- min(soybeans_data_for_2020_forecast$date)
  end_date_2020 <- max(soybeans_data_for_2020_forecast$date)
  
  start_year_2020 <- year(start_date_2020)
  start_month_2020 <- month(start_date_2020)
  
  end_year_2020 <- year(end_date_2020)
  end_month_2020 <- month(end_date_2020)
  
  # Create the nominal price time series object using the filtered data
  price.ts_2020 <- ts(soybeans_data_for_2020_forecast$price,
                      start = c(start_year_2020, start_month_2020),
                      end = c(end_year_2020, end_month_2020),
                      frequency = 12) # Monthly data
  
  # Check if the time series object was created correctly
  if (length(price.ts_2020) == 0) {
    print("Error: price.ts_2020 time series object is empty.")
  } else {
    print("Head of price.ts_2020:")
    print(head(price.ts_2020))
    print("Tail of price.ts_2020:")
    print(tail(price.ts_2020))
    
    # Create the log-differenced time series object using price.ts_2020
    log_price.ts_2020 <- log(price.ts_2020)
    
    # Take the first difference of the log-transformed time-series
    log_price_diff.ts_2020 <- diff(log_price.ts_2020, differences = 1) # differences=1 for the first difference
    
    # Check if the log-differenced time series object was created correctly
    if (length(log_price_diff.ts_2020) == 0) {
      print("Error: log_price_diff.ts_2020 time series object is empty.")
    } else {
      # Check the end date of the log-differenced series (should be one month after end of price.ts_2020)
      print("End date of log-differenced series for 2020 forecast:")
      print(tail(time(log_price_diff.ts_2020), 1)) # The end date should be July 2020 if price.ts_2020 ends June 2020
      
      # Fit the AR(3) model to this truncated log-differenced series
      ar_model_2020 <- ar(log_price_diff.ts_2020, order.max = 3)
      
      print("AR(3) Model Results fitted on data up to July 2020:")
      print(ar_model_2020)
 
      forecast_results_2020 <- forecast(ar_model_2020, h = 6)
      
      # Display the forecast results.
      print("--- 6-Month Forecast Results (Aug 2020 - Jan 2021) ---")
      print(forecast_results_2020)
  
      # Graph the forecast along with recent historical data (e.g., the last 36 months before the forecast)
      print("--- Generating Forecast Plot (Aug 2020 - Jan 2021) ---")
      # Ensure your plotting pane in RStudio is visible and large enough.
      # If you are using par(mfrow=...) elsewhere, make sure to reset it with par(mfrow=c(1,1))
      # before this autoplot call.
      autoplot(forecast_results_2020, include = 36, xlab = "Date", ylab = "Soybeans price - monthly % returns")
      print("Plot should be displayed in your R plotting window.")
    }
  }
}

