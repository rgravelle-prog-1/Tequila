# Assuming 'weather_total' is your dataframe with columns 'datetime' and 'temp'
# Convert 'datetime' to a POSIXct format
weather_total$datetime <- as.POSIXct(weather_total$datetime)

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
train_indices <- sample(1:nrow(weather_total), 0.8 * nrow(weather_total))
train_data <- weather_total[train_indices, ]
test_data <- weather_total[-train_indices, ]

# Train a linear regression model
model <- lm(temp ~ datetime, data = train_data)

# Make predictions on the testing set
predictions <- predict(model, newdata = test_data)

# Check for missing values in 'temp' and 'predictions'
missing_temp <- any(is.na(test_data$temp))
missing_predictions <- any(is.na(predictions))

# Filter out NA values in 'temp' and 'predictions'
non_na_indices <- !is.na(test_data$temp) & !is.na(predictions)
filtered_temp <- test_data$temp[non_na_indices]
filtered_predictions <- predictions[non_na_indices]

# Check if there are any non-NA values remaining
if (length(filtered_temp) > 0 && length(filtered_predictions) > 0) {
  # Calculate RMSE
  rmse <- sqrt(mean((filtered_temp - filtered_predictions)^2))
  cat("Root Mean Squared Error (RMSE):", rmse, "\n")
} else {
  cat("Error: No non-NA values remaining. Cannot calculate RMSE.\n")
}



# Plot the actual vs. predicted temperatures
plot(test_data$datetime, test_data$temp, col = "blue", type = "l", lwd = 2, ylab = "Temperature", xlab = "Datetime", main = "Actual vs. Predicted Temperature")
lines(test_data$datetime, predictions, col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1:2, cex = 0.8)
