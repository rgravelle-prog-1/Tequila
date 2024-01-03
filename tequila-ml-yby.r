# Install the packages if not already installed
if (!require(randomForest)) {
  install.packages("randomForest")
}

if (!requireNamespace("dendextend", quietly = TRUE)) {
  install.packages("dendextend")

}
if (!requireNamespace("ggdendro", quietly = TRUE)) {
  install.packages("ggdendro")

}


# Load the librararies
library(randomForest)
library(dendextend)
library(dendextend)



# Load the first data source (weather.csv)
weather1 <- "C:\\RProj\\Tequila\\weather.csv"
weather_data1 <- read.csv(weather1)

# Load the second data source (weather1.csv)
weather2 <- "C:\\RProj\\Tequila\\weather1.csv"
weather_data2 <- read.csv(weather2)

# remove column X from weather_data1
weather_data1 <- subset(weather_data1, select = -X)

# combine month, day, year into datetime
weather_data1$datetime <- as.POSIXct(paste(weather_data1$DateYear, 
                                           weather_data1$DateMonth, 
                                           weather_data1$DateDay, 
                                           sep = "-"), 
                                     format="%Y-%m-%d")

# create dataset called weather_total

# Rename columns in weather_data2 to match the column names in weather_data1
colnames(weather_data2) <- colnames(weather_data1)

# Combine the data frames while preserving datetime format
weather_total <- data.frame(do.call(rbind, lapply(list(weather_data1, weather_data2), function(df) {
  df$datetime <- as.POSIXct(paste(df$DateYear, df$DateMonth, df$DateDay, sep = "-"), format="%Y-%m-%d")
  df[, !(colnames(df) %in% c("DateMonth", "DateDay", "DateYear"))]
})))

# Select specific columns from weather_total
selected_columns <- c("datetime", "temp", "tempmin", "tempmax", "dew", "humidity", "windspeed", "winddir", "solarenergy")
weather_total <- weather_total[, selected_columns]



# Use Forest tree to predict the future weather 
# Set a random seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
train_indices <- sample(1:nrow(weather_total), 0.8 * nrow(weather_total))
train_data <- weather_total[train_indices, ]
test_data <- weather_total[-train_indices, ]

# Check for missing values in the training dataset
missing_values <- sum(!complete.cases(train_data))

# If there are missing values, remove rows with missing values
if (missing_values > 0) {
  cat("Removing", missing_values, "rows with missing values.\n")
  train_data <- train_data[complete.cases(train_data), ]
}

# Train the random forest model
model <- randomForest(temp ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(model, test_data)

# Compare predictions to actual values
comparison <- data.frame(Actual = test_data$temp, Predicted = predictions)
head(comparison)

# Remove rows with missing values in the test set
test_data <- test_data[complete.cases(test_data), ]

# Make predictions on the cleaned test set
predictions <- predict(model, test_data)

# Calculate RMSE
rmse <- sqrt(mean((test_data$temp - predictions)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Visualization the results of random forest model 

# Variable Importance Plot
varImpPlot(model)

# Scatterplot of Actual vs. Predicted
plot(test_data$temp, predictions, main = "Scatterplot of Actual vs. Predicted", xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = "red")



# cluster the results 

# Standardize the predictions
standardized_predictions <- scale(predictions)

# Choose the number of clusters (k)
k <- 5 

# Perform k-means clustering
clusters <- kmeans(standardized_predictions, centers = k)

# Add cluster assignments to the original data
result_with_clusters <- data.frame(Actual = test_data$temp, Predicted = predictions, Cluster = clusters$cluster)

# View the clustered results
head(result_with_clusters)

# Plot the clustered results
library(ggplot2)

ggplot(result_with_clusters, aes(x = Predicted, y = Actual, color = factor(Cluster))) +
  geom_point() +
  ggtitle("Clustered Results") +
  xlab("Predicted") +
  ylab("Actual") +
  theme_minimal()

# cluster Dendrogram for visualizations

# Standardize the predictions
standardized_predictions <- scale(predictions)

# Perform hierarchical clustering
hc <- hclust(dist(standardized_predictions))

# Create a dendrogram
dend <- as.dendrogram(hc)

# Plot the dendrogram
library(dendextend)
library(ggdendro)

# Customize the dendrogram plot if needed
dend <- dend %>% 
  color_branches(k = 3)  # Change the number of colors based on your clusters

# Plot the dendrogram
ggdendrogram(dend, rotate = TRUE, theme_dendro = FALSE) +
  theme_minimal() +
  ggtitle("Cluster Dendrogram") +
  xlab("Cluster") +
  ylab("Height")

# Random Forest for predicted temperatures with graph 

# Convert 'datetime' to a POSIXct format
weather_total$datetime <- as.POSIXct(weather_total$datetime)

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
train_indices <- sample(1:nrow(weather_total), 0.8 * nrow(weather_total))
train_data <- weather_total[train_indices, ]
test_data <- weather_total[-train_indices, ]

# Handle missing values in the training data
train_data <- na.omit(train_data)

# Train a random forest model
model <- randomForest(temp ~ ., data = train_data)

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

# Predicted temperature graphs and visualizations including table
# Remove rows with missing values in the 'temp' column
weather_total <- na.omit(weather_total)

# Train a random forest model using the cleaned dataset
model <- randomForest(temp ~ ., data = weather_total)

# Make predictions for the entire dataset
predictions <- predict(model, newdata = weather_total)

# Create a new dataframe with 'datetime' and 'predicted_temp'
predicted_values <- data.frame(datetime = weather_total$datetime, predicted_temp = predictions)


# Show examples of predicted temperatures and actual temperatures from projected model


# Remove rows with missing values in the 'temp' column
weather_total <- na.omit(weather_total)
# Train a random forest model using the cleaned dataset
model <- randomForest(temp ~ ., data = weather_total)

# Make predictions for the entire dataset
predictions <- predict(model, newdata = weather_total)

# Create a new dataframe with 'datetime', 'predicted_temp', and 'actual_temp' 
predicted_values <- data.frame(
  datetime = weather_total$datetime,
  predicted_temp = predictions,
  actual_temp = weather_total$temp
)


# Print a random sampling of 25 rows
print(predicted_values[sample(nrow(predicted_values), 25), ])


