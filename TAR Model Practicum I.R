library(readxl) # for reading Excel files
library(tibble) # for data manipulation
library(dplyr)
library(tsDyn)
library(forecast)

# Install and load tsDyn package
if (!require(tsDyn)) {
  install.packages("tsDyn", dependencies = TRUE)
  library(tsDyn)
}

# Set the file path to the location of your Excel file
file_path <- "C:/Users/river/OneDrive/Escritorio/Practicum/Final avg temp.xlsx"

# Read the Excel file into a data frame, assuming the first column contains the temperature data
# and that there are no header rows in the file.
data <- read_excel(file_path, col_names = FALSE)

# Rename the first column to 'AverageTemperature' for clarity
names(data)[1] <- 'AverageTemperature'

# Convert the data to a tibble for easier manipulation
data <- as_tibble(data)

# Since the data is monthly average temperatures, create a sequence of months based on the data
total_months <- nrow(data)

# Calculate the complete years and the remaining months
complete_years <- total_months %/% 12
remaining_months <- total_months %% 12

# Create the years vector
years <- rep(1990:(1990 + complete_years - 1), each = 12)

# If there are remaining months, append the last year for those months
if (remaining_months > 0) {
  years <- c(years, rep(1990 + complete_years, remaining_months))
}

# Create the months vector
months <- rep(1:12, complete_years)

# If there are remaining months, append the sequence of months to the months vector
if (remaining_months > 0) {
  months <- c(months, 1:remaining_months)
}

# Add the year and month columns to the data
data <- data %>%
  mutate(Year = years, Month = months) %>%
  arrange(Year, Month) # Ensure data is in the correct order

# Assuming your data starts in January 1990, set the start_month accordingly
start_month <- 1

# Create a proper time series object using the 'AverageTemperature' column
temp_ts <- ts(data$AverageTemperature, start = c(1990, start_month), frequency = 12)

# Convert the time series data from character to numeric
temp_ts_numeric <- as.numeric(temp_ts)

# Create a new time series object with the numeric data
temp_ts_numeric <- ts(temp_ts_numeric, start=c(1990, 1), frequency=12)

# Fit a SETAR model to the numeric data using tsDyn package
# Specify mL (and optionally mH) based on your data's characteristics
setar_model <- setar(temp_ts_numeric, m=4, thDelay=1) # Adjust mL and mH as necessary

# Summary of the SETAR model
summary(setar_model)

# Plot the SETAR model
plot(setar_model)

# Plot ACF and PACF
acf(temp_ts_numeric, main="ACF of Temperature Data")
pacf(temp_ts_numeric, main="PACF of Temperature Data")


# Assuming the dataset is split into training and testing sets
# Let's say the last N rows are for testing (forecast period)
# Replace N with the number of periods you're forecasting
N <- 12
train_data <- data[1:(nrow(data) - N), ]
test_data <- data[(nrow(data) - N + 1):nrow(data), ]

# Convert the training data to a time series object
train_ts <- ts(train_data$AverageTemperature, start = c(1990, start_month), frequency = 12)

# Fit a SETAR model to the training data
setar_model <- setar(train_ts, m=4, thDelay=1) 

# Forecasting future values using the SETAR model
forecasted_values <- predict(setar_model, n.ahead = N)

# Extracting the forecasted mean values
predicted_means <- forecasted_values$pred

# Actual observed values for the forecasted period
actual_values <- test_data$AverageTemperature


# Define the start year for the plot (20 years after the start of the original series)
plot_start_year <- 1990 + 20  # Adjust if your series starts in a different year
plot_end_year <- train_end_year + 1  # The year after the training set ends

# Subset the actual time series to the period you want to plot
plot_ts <- window(temp_ts, start=c(plot_start_year, 1))

# Plot the actual time series for the specified period
plot(plot_ts, main="Temperature Time Series with Forecast", xlab="Time", ylab="Temperature", col="blue", type="l")

# Adding the forecast to the plot
# Ensure the forecast_ts starts from where the actual data ends
forecast_ts <- ts(final_forecast, start=c(plot_end_year, 1), frequency=12)

# Now, add the forecasted values to the plot
lines(forecast_ts, col="red", lty=2)

# Add a legend to the plot
legend("topleft", legend=c("Actual", "Forecast"), col=c("blue", "red"), lty=c(1, 2), cex=0.8)

# Note: Make sure the 'start' argument for forecast_ts matches where your actual values end.