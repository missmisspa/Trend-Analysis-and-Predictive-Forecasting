# Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Set working directory and load data
setwd("C:/Users/samsu/Documents/3rd year bsit/IM 2/Files")
getwd()  # Check if the directory is set correctly
data <- read.csv("hotel_bookings.csv")

# Check structure and summary of data
str(data)
summary(data)

# Check column names to ensure arrival_date_month and arrival_date_year exist
head(data)
colnames(data)

# Convert month names to a factor with a specific order for proper chronological display
data$arrival_date_month <- factor(data$arrival_date_month, levels = month.name)
data$arrival_date_year <- as.factor(data$arrival_date_year)

#objective 1
# Plot bookings per month for each year
ggplot(data, aes(x = arrival_date_month, fill = factor(arrival_date_year))) +
  geom_bar(position = "dodge") +  # Position bars side-by-side for each year
  labs(title = "Monthly Booking Pattern by Year", 
       x = "Month", 
       y = "Number of Bookings", 
       fill = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Objective 2: Analyze seasonal demand by month and year
# Group data by month and year
seasonal_data <- data %>%
  group_by(arrival_date_year, arrival_date_month) %>%
  summarise(total_bookings = n(), .groups = 'drop')  # Drop grouping after summarization to avoid nesting issues

# Ensure month order for chronological plotting
seasonal_data$arrival_date_month <- factor(seasonal_data$arrival_date_month, levels = month.name)

# Plot the seasonal demand, with bars side-by-side for each year within each month
ggplot(seasonal_data, aes(x = arrival_date_month, y = total_bookings, fill = factor(arrival_date_year))) +
  geom_bar(stat = "identity", position = "dodge") +  # Bars side-by-side for each year
  labs(title = "Seasonal Booking Demand by Month and Year",
       x = "Month",
       y = "Total Bookings",
       fill = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

#objective 3
# Load forecasting library
library(forecast)

# Create a time series for bookings per month
monthly_bookings <- data %>%
  mutate(date = paste(arrival_date_year, arrival_date_month, "01", sep = "-")) %>%
  mutate(date = ymd(date)) %>%
  group_by(date) %>%
  summarise(total_bookings = n())

# Convert to time series object
ts_data <- ts(monthly_bookings$total_bookings, start = c(min(data$arrival_date_year), 1), frequency = 12)

# Fit an ARIMA model
arima_model <- auto.arima(ts_data)

# Forecast future booking demand for the next 12 months
forecasted_demand <- forecast(arima_model, h = 12)
plot(forecasted_demand)




#objective 4



#objective 5

# Length of Stay Analysis by Month and Year
length_of_stay_analysis_monthly <- data %>%
  group_by(arrival_date_year, arrival_date_month, stays_in_week_nights) %>%
  summarise(total_stays = n(), .groups = 'drop') %>%
  arrange(arrival_date_year, arrival_date_month, desc(stays_in_week_nights))

# Ensure month order for proper plotting
length_of_stay_analysis_monthly$arrival_date_month <- factor(length_of_stay_analysis_monthly$arrival_date_month, levels = month.name)

# Plotting Length of Stay Distribution by Month and Year
ggplot(length_of_stay_analysis_monthly, aes(x = stays_in_week_nights, y = total_stays, fill = arrival_date_month)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bars side-by-side for each month
  facet_wrap(~ arrival_date_year) +  # Create a separate plot for each year
  labs(title = "Length of Stay Distribution by Month and Year",
       x = "Length of Stay (Nights)",
       y = "Number of Stays",
       fill = "Month") +
  theme_minimal()



#objective 6

























