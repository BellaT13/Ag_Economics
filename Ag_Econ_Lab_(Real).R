#install packages
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("gtsummary")

#Load packages
library(ggplot2)
library(gridExtra)
library(gtsummary)

#Set working directory through "Session" on Mac to Ag_Economics folder
getwd()

#Import WASDE data
WASDE <- read.csv("WASDE.csv")

#Explore the dataset
head(WASDE)
str(WASDE)

#Plot corn prices, total corn supply, and total corn demand over time
#Plot total corn prices over time
g_price <- ggplot(WASDE, aes(x = year, y = corn_price)) +
  geom_line(color = "blue") +
  labs(title = "Corn Prices Over Time", x = "year", y = "Price ($/bushel)")
print(g_price)
# Plot total corn demand over time
g_demand <- ggplot(WASDE, aes(x = year, y = total_use)) +
  geom_line(color = "green") +
  labs(title = "Total Corn Demand Over Time", x = "year", y = "Demand (million bushels)")
print(g_demand)

# Plot total corn supply over time
g_supply <- ggplot(WASDE, aes(x = year, y = total_supply)) +
  geom_line(color = "orange") +
  labs(title = "Total Corn Supply Over Time", x = "year", y = "Supply (million bushels)")
print(g_supply)

# Arrange plots in a single view
t_plots <- grid.arrange(g_price, g_demand, g_supply, nrow = 3)
print(t_plots)

#Save plot
ggsave("t_plots.pdf", plot = t_plots)

# Create a new variable for stock-to-use ratio and create a scatterplot of corn prices on stock-to-use ratio

WASDE$SUR <- WASDE$end_stocks / WASDE$total_use
ggplot(data = WASDE, aes(x = SUR, y = corn_price)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, color = "red") +
  ggtitle("Corn Prices vs. Stock-to-Use Ratio") +
  labs(x = "Stock-to-Use Ratio (SUR)", y = "Corn Price (USD per Bushel)") +
  theme_minimal()
ggsave("WASDE$SUR.png")

# Estimate linear regression model
reg1 <- lm(corn_price ~ SUR, data = WASDE)

# Display regression results
summary(reg1)

library(gtsummary)
tbl_regression(reg1, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))

# Calculate averages
mean_sur <- mean(WASDE$SUR)
mean_price <- mean(WASDE$corn_price)

# Summary statistics of residuals
summary(resid(reg1))

# Histogram of residuals
hist(resid(reg1), 
     main = "Histogram of Linear Regression Errors",
     xlab = "Linear Model Residuals" )


# Scatterplot of errors vs SUR
ggplot(data=WASDE, aes(x=SUR, y=resid(reg1))) + 
  geom_point(shape = 1, color = "sienna") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgreen") +
  labs(x = "Stock-to-Use Ratio (SUR)", y = "Residuals", 
   title = "Residuals vs. Stock-to-Use Ratio") +
  theme_minimal()
 
#Create inverse of stock-to-use ratio
WASDE$SUR_Inv <- 1 / WASDE$SUR
reg2 <- lm(corn_price ~ SUR_Inv, data=WASDE)

summary(reg2)

# Residual analysis
summary(resid(reg2))
hist(resid(reg2), main="Histogram of Non-linear Regression Errors", xlab="Non-linear Model Residuals")

# Residuals vs SUR plot
ggplot(data=WASDE, aes(x=SUR, y=resid(reg2))) +
  geom_point(shape=1) +
  ggtitle("Non-linear Regression Errors vs. Stock-to-Use Ratio") +
  labs(y="Errors", x="Stock-to-Use Ratio")

# Create a character variable denoting the two time periods
WASDE$period <- ifelse(WASDE$year >= 2006, "2006–2019", "1973–2005")
WASDE$P2006 <- as.numeric(WASDE$year >= 2006)

# Scatterplot with separate regression lines for each period
ggplot(data = WASDE, aes(x = SUR, y = corn_price, color = period)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, se = FALSE) +
  ggtitle("Corn Prices vs. Stock-to-Use Ratio (1973–2019)") +
  labs(y = "Corn Price ($)", x = "Stock-to-Use Ratio") +
  theme_minimal() +
  scale_color_manual(values = c("1973–2005" = "darkgreen", "2006–2019" = "steelblue"))

# Run a linear regression with time period specific
reg3 <- lm(corn_price ~ SUR + P2006 + SUR:P2006, data=WASDE)

summary(reg3)

# Collect the residuals from the last regression
error_ts <- ts(resid(reg3), start = 1973, end = 2019, frequency = 1)

# Create a one-year lag of the error
lag_error_ts <- stats::lag(error_ts, -1)

# Combine into a data frame
error_df <- data.frame(
  error = as.numeric(error_ts),
  lag_error = as.numeric(lag_error_ts)
)

# Remove NA from the first row (introduced by lag)
error_df <- na.omit(error_df)

# Run regression of errors on lagged errors
reg4 <- lm(error ~ lag_error, data = error_df)

# Display regression results
summary(reg4)

# Estimate linear regression model
reg1 <- lm(corn_price ~ SUR, data = WASDE)

tbl_regression(reg1, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))

# Create inverse of stock-to-use ratio
WASDE$SUR_Inv <- 1 / WASDE$SUR

# Estimate regression model with 1/SUR as predictor
reg_inv <- lm(corn_price ~ SUR_Inv, data = WASDE)

# Display regression results using gtsummary
tbl_regression(reg_inv, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))

# Create a character variable denoting the two time periods
WASDE$period <- ifelse(WASDE$year >= 2006, "2006–2019", "1973–2005")
WASDE$P2006 <- as.numeric(WASDE$year >= 2006) # P2006 is the dummy variable

# Run a linear regression with time period specific
reg3 <- lm(corn_price ~ SUR + P2006 + SUR:P2006, data=WASDE)

# Display regression results
summary(reg3)



