library(dplyr)
library(tidyverse)
library(ggplot2)
library(htmlwidgets)
library(DT)
library(data.table)
library(knitr)
library(kableExtra)


#Import the .CSV files

fulllist <- fread("fulldataman.csv", header = TRUE, sep = "," )
weather_data <- read.csv("weather.csv")
data <- read.csv("ybyratings.csv")


#Count by Regions
# Replace special characters in Region column
fulllist$Region <- iconv(fulllist$Region, "UTF-8", "ASCII", sub = "")

# Count by Regions and sort in descending order
result <- fulllist %>%
  group_by(Region) %>%
  summarise('Total' = sum(count)) %>%
  arrange(desc(Total))
view(result)


# Count by Cooking Methods and order in descending order
cooking_counts <- fulllist %>%
  group_by(Cooking) %>%
  summarise('Total' = sum(count)) %>%
  arrange(desc(Total)) %>%
  filter(Total >= 0) %>%
  filter(Cooking != "-")

# View the ordered cooking counts without negative counts and "-"
print(cooking_counts)




#Count by NOM top ten
fulllist %>%
  group_by(NOM) %>%
  summarise('Total' = sum(count))  %>% arrange(desc(Total)) %>% slice(1:10)

#Count by Extraction methods
fulllist %>% 
  group_by(Extraction) %>%
  summarise('Total' = sum(count))  %>% arrange(desc(Total))

#Count by Distillation
fulllist %>% 
  group_by(Distillation) %>%
  summarise('Total' = sum(count)) %>% arrange(desc(Total))

#Count by Still Methods
fulllist %>%
  group_by(Still) %>%
  summarise('Total' = sum(count)) %>% arrange(desc(Total))

# Count by ABV top 15 and remove entries with "-"
abv_counts <- fulllist %>%
  group_by(ABVorProof) %>%
  summarise('Total' = sum(count)) %>%
  arrange(desc(Total)) %>%
  filter(ABVorProof != "-") %>%
  slice(1:15)

# View the ordered ABV counts without entries with "-"
print(abv_counts)



fulllist %>%
  filter(Water_Source != "-") %>%
  group_by(Water_Source) %>%
  summarise(Total = sum(count)) %>%
  arrange(desc(Total)) %>%
  mutate(Total = as.numeric(Total))



#make Panel a number
fulllist$Panel <- as.numeric(as.character(fulllist$Panel))
fulllist$community <- as.numeric(as.character(fulllist$community))

#summary Region and Panel
group_by(fulllist, Region)
tequila_by_Region <- group_by(fulllist, Region)
summarise(tequila_by_Region)

tequila_by_Region_Panel <- group_by(fulllist, Region, Panel)
summarise(tequila_by_Region_Panel)


library(dplyr)

tequila_by_Region <- fulllist %>%
  group_by(Region) %>%
  summarise(avg_rating = mean(Panel, na.rm = TRUE))

ggplot(tequila_by_Region, 
       aes(x = Region,
           y = avg_rating,
           fill = Region)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme(axis.text = element_text(angle = 45))


#ggplot by community
ggplot(fulllist, 
       aes(x = Region,
           y = community,
           fill = Region)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme(axis.text = element_text(angle = 90))

#Average Rating for Community Rating 
fulllist %>%
  group_by(Region) %>%
  summarise(AverageRatingCommunity=mean(community, na.rm = TRUE))

#Average Rating for Panel Rating 
fulllist %>%
  group_by(Region) %>%
  summarise(AverageRatingCommunity=mean(Panel, na.rm = TRUE))

#ggplot for Community
ratregioncommunity <- fulllist %>%
  group_by(Region) %>%
  summarise(AverageRatingCommunity=mean(community, na.rm = TRUE))

ggplot(ratregioncommunity, 
       aes(x = Region,
           y = AverageRatingCommunity,
           fill = Region)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme(axis.text = element_text(angle = 90))

#ggplot for Panel
ratregionpanel <- fulllist %>%
  group_by(Region) %>%
  summarise(AverageRatingpanel=mean(Panel, na.rm = TRUE))

ggplot(ratregionpanel, 
       aes(x = Region,
           y = AverageRatingpanel,
           fill = Region)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme(axis.text = element_text(angle = 90))

#ggplot line for Panel
ratregionpanel <- fulllist %>%
  group_by(Region) %>%
  summarise(AverageRatingpanel=mean(Panel, na.rm = TRUE))

ggplot(ratregionpanel, 
       aes(group=1, x = Region,
           y = AverageRatingpanel,
           fill = Region)) +
  geom_line(stat = "identity",
            position = "dodge") +
  theme(axis.text = element_text(angle = 90))
 
#ggplot line for Community
ratregioncommunity <- fulllist %>%
  group_by(Region) %>%
  summarise(AverageRatingcommunity=mean(Panel, na.rm = TRUE))

ggplot(ratregioncommunity, 
       aes(group=1, x = Region,
           y = AverageRatingcommunity,
           fill = Region)) +
  geom_line(stat = "identity",
            position = "dodge") +
  theme(axis.text = element_text(angle = 90))

#Density plot for Panels
ggplot(fulllist) + geom_density(aes(x=fulllist$Panel), fill="blue")

#Density plot for community
ggplot(fulllist) + geom_density(aes(x=fulllist$community), fill="red")

#Density Plot Comparison
# Filter out rows with missing or non-finite values in the Panel and community columns
selected_data <- fulllist %>% 
  select(Panel, community) %>%
  filter(!is.na(Panel) & is.finite(Panel) & !is.na(community) & is.finite(community))

# Create the combined density plot
ggplot(selected_data) +
  geom_density(aes(x = Panel, fill = "Panels"), alpha = 0.5) +
  geom_density(aes(x = community, fill = "Community"), alpha = 0.5) +
  labs(title = "Density Plot Comparison", x = "Rating", fill = "Variable") +
  theme_minimal()


#list(DF_comm_r,(desc(community) %>% slice(1:10)

#select(fulllist$community, fulllist$Region, fulllist$title)

library(dplyr)

# Select specific columns from the data frame
selected_columns <- select(fulllist, community, Region, title)

# View the selected columns
print(selected_columns)


# Community top 25 ratings
new_dat_community_ratings  <- fulllist %>% select(community, Region, title) %>% arrange(desc(community)) %>% slice(1:25)

# Panel top 25 ratings
new_dat_panel_ratings  <- fulllist %>% select(Panel, Region, title) %>% arrange(desc(Panel)) %>% slice(1:25)

# Top 25 list of Tequila on both lists 
# Top 25 list of Tequila for both community and panel ratings
ndcr <- fulllist %>% 
  select(community, Region, title) %>% 
  arrange(desc(community)) %>% 
  slice(1:25)
print(ndcr)

ndpr <- fulllist %>% 
  select(Panel, Region, title) %>% 
  arrange(desc(Panel)) %>% 
  slice(1:25)
print(ndpr)

# Find the intersection of the two lists based on 'title'
intersection_titles <- intersect(ndcr$title, ndpr$title)

# Create a new data frame with the common titles
common_tequila <- fulllist %>% 
  filter(title %in% intersection_titles)
# Display ndcr and ndpr side by side
combined_data <- cbind(ndcr, ndpr)

# View the combined data
View(combined_data)




#Median of scores
panel_med <- summarise(fulllist, median(fulllist$Panel, na.rm = TRUE))
community_med <- summarise(fulllist, median(fulllist$community, na.rm = TRUE)) 

#histogram average scores

hist(fulllist$avgscore, main = "Average Score", xlab = "Panel", ylab = "Community")

#list of scores 
list(c(fulllist$Panel, fulllist$community, fulllist$avgscore))

# average scores of community
communavg <- sample(x=fulllist$community, size = 100, replace = TRUE)
mean(communavg, na.rm = TRUE)

# average scores of Panel
Panelavg <- sample(x=fulllist$Panel, size = 100, replace = TRUE)
mean(Panelavg, na.rm = TRUE)

#Distribution of Panel
distrpanel <- mean(Panelavg, na.rm = TRUE)
rnorm(n=Panelavg, mean =distrpanel, sd=15)

#Distribution of Community
distrcomm <- mean(communavg, na.rm = TRUE)
rnorm(n=communavg, mean =distrcomm, sd=15)

#######################################
# comparison of average scores
####################################

# Ensure both vectors have the same length
min_length <- min(length(communavg), length(Panelavg))

# Create a data frame with the average scores for community and panel
avg_scores_df <- data.frame(
  Category = rep(c("Community", "Panel"), each = min_length),
  AverageScore = c(communavg[1:min_length], Panelavg[1:min_length])
)

# Line chart comparison
library(ggplot2)

ggplot(avg_scores_df, aes(x = Category, y = AverageScore, color = Category)) +
  geom_line() +
  geom_point() +
  labs(title = "Comparison of Average Scores (Community vs Panel)",
       x = "Category",
       y = "Average Score")



#Current Testing of Cor
pa <- mean(Panelavg, na.rm = TRUE)
ca <- mean(communavg, na.rm = TRUE)

#Linear regression of PAnel vs Community
# Filter out rows with missing or non-finite values in 'community' and 'Panel'
filtered_data <- fulllist %>% filter(!is.na(community) & !is.na(Panel) & is.finite(community) & is.finite(Panel))

# Create the scatter plot with linear regression
ggplot(filtered_data, aes(x = community, y = Panel)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Community", y = "Panel")




#Linear regression of Panel vs avg
ggplot(fulllist, aes(x = avgscore, y = Panel)) + geom_point() + geom_smooth(method = "lm") + labs(x = "Community", y = "Panel")

# Read the CSV file into a data frame
weather <- read.csv("C:\\Users\\belgr\\R Proj\\Tequila\\weather.csv")


# Assuming datetime is in the format "YYYY-MM-DD"
weather$datetime <- paste(weather$DateYear, weather$DateMonth, weather$DateDay, sep = "-")

# Create a ggplot scatter plot
ggplot(weather, aes(x = datetime, y = temp)) +
  geom_point() +
  labs(x = "Date", y = "Temperature", title = "Typical Temperature Curve")


# Create a ggplot scatter plot
ggplot(weather, aes(x = datetime, y = temp)) +
  geom_point() +
  labs(x = "Date", y = "Temperature", title = "Typical Temperature Curve")

# Install the anytime package if not already installed
#if (!require(anytime)) install.packages("anytime")

# Load the anytime package
#library(anytime)

# Read the CSV file into a data frame
#weather <- read.csv("weather.csv")

# Assuming datetime is in the format "YYYY-MM-DD HH:MM:SS"
#weather$datetime <- as.POSIXct(strptime(weather$datetime, format = "%Y-%m-%d %H:%M:%S"))

# Create a scatter plot of temperature over time
# ggplot(weather, aes(x = datetime, y = temperature)) +
#  geom_point() +
#  labs(title = "Temperature Trend Over Time", x = "Datetime", y = "Temperature")


# Scatter plot for Cooking vs Rating
ggplot(fulllist, aes(x = Cooking, y = Rating)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.5) +
  labs(x = "Cooking Method", y = "Rating", title = "Cooking Method vs Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################
# top 5 tequilas Combined

# Create a data frame with tequilas and their ratings from Panel and Community
tequilas_ratings <- aggregate(cbind(Panel, community) ~ title, data = fulllist, FUN = mean, na.rm = TRUE)

# Create a new column for the average of Panel and Community ratings
tequilas_ratings$Average_Rating <- rowMeans(tequilas_ratings[, c("Panel", "community")], na.rm = TRUE)

# Order the data frame by Average_Rating in descending order
tequilas_ratings <- tequilas_ratings[order(-tequilas_ratings$Average_Rating), ]

# Select the top 20 tequilas
top_20_ratings <- head(tequilas_ratings, 20)

# Remove special characters from the 'title' column
 tequilas_ratings$title <- iconv(tequilas_ratings$title, "UTF-8", "ASCII", sub = "")

# Display the top 5 tequilas with Extraction method, Aging method, and Cooking method
 print(tequilas_ratings[1:5, ])

###############
# top 5 tequilas with avg ratings 

# Create a data frame with tequilas and their ratings from Panel and Community
tequilas_ratings <- aggregate(cbind(Panel, community) ~ title, data = fulllist, FUN = mean, na.rm = TRUE)

# Create a new column for the average of Panel and Community ratings
tequilas_ratings$Average_Rating <- rowMeans(tequilas_ratings[, c("Panel", "community")], na.rm = TRUE)

# Order the data frame by Average_Rating in descending order
tequilas_ratings <- tequilas_ratings[order(-tequilas_ratings$Average_Rating), ]

# Select the top 5 tequilas
top_5_ratings <- head(tequilas_ratings, 5)

# Print the average ratings for the top 5 tequilas
print(top_5_ratings[, c("title", "Average_Rating")])

##############################
# Top Aroma by Extraction Method (Sorted by Count):

# Remove rows where 'Extraction' is "-" and 'aroma1t' is empty
fulllist <- fulllist[fulllist$Extraction != "-" & !is.na(fulllist$aromas1t) & fulllist$aromas1t != "", ]

# Count the occurrences of each aroma1t by extraction method
aroma1t_counts <- table(fulllist$Extraction, fulllist$aromas1t)

# Convert the counts to a data frame
aroma1t_df <- as.data.frame(aroma1t_counts)

# Rename the columns for better clarity
colnames(aroma1t_df) <- c("Extraction", "Aroma", "Count")  # Rename 'Aroma1t' to 'Aroma'

# Order the data frame by count within each extraction method and then by overall count
aroma1t_df <- aroma1t_df[order(aroma1t_df$Extraction, -aroma1t_df$Count), ]

# Select the top aroma1t for each extraction method
top_aroma1t_by_extraction <- by(aroma1t_df, aroma1t_df$Extraction, function(x) head(x, 1))

# Combine the results into a single data frame
top_aroma1t_by_extraction_df <- do.call(rbind, top_aroma1t_by_extraction)

# Remove the "Diffuser" column
top_aroma1t_by_extraction_df <- top_aroma1t_by_extraction_df[, -1]

# Sort the data frame by the last column (Count) in descending order
top_aroma1t_by_extraction_df <- top_aroma1t_by_extraction_df[order(-top_aroma1t_by_extraction_df$Count), ]

# Display the updated data frame
print("Top Aroma by Extraction Method (Sorted by Count):")
print(top_aroma1t_by_extraction_df)



#########################
# weather information

# Load the ggplot2 library
library(ggplot2)

# Read the CSV file into a data frame
weather <- read.csv("C:\\Users\\belgr\\R Proj\\Tequila\\weather.csv")

# Assuming datetime is in the format "YYYY-MM-DD"
weather$datetime <- as.POSIXct(paste(weather$DateYear, weather$DateMonth, weather$DateDay, sep = "-"), format = "%Y-%m-%d")

# Create a ggplot scatter plot
ggplot(weather, aes(x = datetime, y = temp)) +
  geom_point() +
  labs(x = "Date", y = "Temperature", title = "Typical Temperature Curve")
########################################
# averages for three years 

# Read the CSV file
data <- read.csv("ybyratings.csv")

# Print column names
print(colnames(data))

# Convert YR20, YR21, and YR22 to numeric
data[, c("YR20", "YR21", "YR22")] <- lapply(data[, c("YR20", "YR21", "YR22")], as.numeric)

# Check for any non-numeric values
non_numeric_values <- data[!sapply(data[, c("YR20", "YR21", "YR22")], is.numeric)]

# If there are non-numeric values, inspect and clean the data
if (ncol(non_numeric_values) > 0) {
  print("Non-numeric values detected. Inspect the following:")
  print(non_numeric_values)
  # Depending on the nature of non-numeric values, you may need to clean or handle them
} else {
  # Calculate the row-wise average of YR20, YR21, and YR22 with two decimal places
  data$Average_All_Years <- round(rowMeans(select(data, c("YR20", "YR21", "YR22")), na.rm = TRUE), 2)
}

# Select the top 25 rows
top_25 <- head(data[order(-data$Average_All_Years), c("title", "Average_All_Years")], 25)

# Display the top 25 rows with titles and the final average rounded to two decimal places
print(top_25)

###############################
# Year over Year for top 25 tequilas 

library(ggplot2)

# Select the top 25 rows
top_25 <- head(data[order(-data$Average_All_Years), c("title", "YR20", "YR21", "YR22")], 25)

# Reshape the data for ggplot2
top_25_long <- tidyr::gather(top_25, key = "Year", value = "Rating", YR20:YR22)

# Convert Year to a factor for proper ordering
top_25_long$Year <- factor(top_25_long$Year, levels = c("YR20", "YR21", "YR22"))

# Create a line plot
ggplot(top_25_long, aes(x = Year, y = Rating, group = title, color = title)) +
  geom_line() +
  geom_point() +
  labs(title = "Year-over-Year Ratings for Top 25 Titles",
       x = "Year",
       y = "Rating") +
  theme_minimal()

###########################################
# percentages up and down yr over yr

# Convert YR20, YR21, and YR22 to numeric
data[, c("YR20", "YR21", "YR22")] <- lapply(data[, c("YR20", "YR21", "YR22")], as.numeric)

# Exclude rows with missing values in any of the years
data_no_na <- na.omit(data[, c("YR20", "YR21", "YR22")])

# Calculate the percentage of tequilas that went up between YR20 and YR21
percentage_increase_20_to_21 <- sum(data_no_na$YR21 > data_no_na$YR20) / nrow(data_no_na) * 100

# Calculate the percentage of tequilas that went down between YR20 and YR21
percentage_decrease_20_to_21 <- sum(data_no_na$YR21 < data_no_na$YR20) / nrow(data_no_na) * 100

# Calculate the percentage of tequilas that went up between YR21 and YR22
percentage_increase_21_to_22 <- sum(data_no_na$YR22 > data_no_na$YR21) / nrow(data_no_na) * 100

# Calculate the percentage of tequilas that went down between YR21 and YR22
percentage_decrease_21_to_22 <- sum(data_no_na$YR22 < data_no_na$YR21) / nrow(data_no_na) * 100

# Display the results
print(paste("Percentage of tequilas that went up between YR20 and YR21:", round(percentage_increase_20_to_21, 2), "%"))
print(paste("Percentage of tequilas that went down between YR20 and YR21:", round(percentage_decrease_20_to_21, 2), "%"))
print(paste("Percentage of tequilas that went up between YR21 and YR22:", round(percentage_increase_21_to_22, 2), "%"))
print(paste("Percentage of tequilas that went down between YR21 and YR22:", round(percentage_decrease_21_to_22, 2), "%"))

#################################
# general trend for top 25 yr over yr

# Calculate the average rating for each year
average_ratings <- data %>%
  gather(key = "Year", value = "Rating", starts_with("YR")) %>%
  group_by(Year) %>%
  summarise(Average_Rating = mean(Rating, na.rm = TRUE))

# Convert Year to a factor for proper ordering
average_ratings$Year <- factor(average_ratings$Year, levels = c("YR20", "YR21", "YR22"))

# Create a line plot for the general trend of the top 25 tequilas
ggplot(average_ratings, aes(x = Year, y = Average_Rating, group = 1)) +
  geom_line() +
  labs(title = "Overall Trend of Ratings for Top 25 Tequilas Over the Years",
       x = "Year",
       y = "Average Rating") +
  theme_minimal()


#######################################
# year by year average temperature

# Load the "weather.csv" data
weather_data <- read.csv("weather.csv")


# Step 2: Ensure the 'datetime' column is in the Date format
# Create a new 'datetime' column based on DateYear, DateMonth, and DateDay
weather_data$datetime <- as.Date(paste(weather_data$DateYear, weather_data$DateMonth, weather_data$DateDay, sep="-"))

weather_data$datetime <- as.Date(weather_data$datetime)

# Check the unique years in the data
unique_years <- unique(format(weather_data$datetime, "%Y"))
print(unique_years)

# Step 3: Extract the year from the 'datetime' column
weather_data$year <- format(weather_data$datetime, "%Y")

# Step 4: Filter data for the specific years (2021, 2022, and 2023)
selected_years <- c("2021", "2022", "2023")
filtered_data <- weather_data[weather_data$year %in% selected_years, ]

# Calculate the average temperature by year
average_temp_by_year <- aggregate(temp ~ format(datetime, "%Y"), data = weather_data, FUN = mean, na.rm = TRUE)

# Print the result
print(average_temp_by_year)

####################################################
# Yearly Average Ratings and Temperature Correlation

# year by year average temperature

# Step 1: Load the "weather.csv" data
weather_data <- read.csv("weather.csv")

# Step 2: Ensure the 'datetime' column is in the Date format
weather_data$datetime <- as.Date(paste(weather_data$DateYear, weather_data$DateMonth, weather_data$DateDay, sep="-"))

# Step 3: Extract the year from the 'datetime' column
weather_data$year <- factor(format(weather_data$datetime, "%Y"))

# Calculate the average temperature by year
average_temp_by_year <- aggregate(temp ~ year, data = weather_data, FUN = mean, na.rm = TRUE)

# Load ybyratings data
ybyratings_data <- read.csv("ybyratings.csv")

# Select relevant columns for average ratings
ratings_columns <- c("YR20", "YR21", "YR22")

# Convert columns to numeric (handling NAs)
ybyratings_data[, ratings_columns] <- sapply(ybyratings_data[, ratings_columns], function(x) as.numeric(x, na.rm = TRUE))

# Calculate average ratings
average_ratings <- colMeans(ybyratings_data[, ratings_columns], na.rm = TRUE)

# Correlate average temperatures and average ratings
correlation_result <- cor(average_temp_by_year$temp, average_ratings)

# Print the correlation result
print(correlation_result)

# Create a line chart with specific years
plot(
  seq_along(ratings_columns),
  average_ratings,
  type = "l",
  col = "gray",  # Set blue line color to gray
  xlab = "Year",
  ylab = "Average Ratings",
  main = "Yearly Average Ratings and Temperature Correlation",
  xaxt = "n",
  lwd = 8  # Set line width to 8
)

# Add specific labels to the x-axis
axis(1, at = seq_along(ratings_columns), labels = ratings_columns)

# Add points for the correlation values with larger size and black color
points(
  seq_along(ratings_columns),
  rep(correlation_result, length(ratings_columns)),
  col = "green",  
  pch = 19,
  cex = 8  # Set point size to 8
)

#################################

# RMSE for temperature and avg temp

correlation_result <- cor(average_temp_by_year$temp, average_ratings)
YR20 <- ybyratings_data$YR20 
YR21 <- ybyratings_data$YR21 
YR22 <- ybyratings_data$YR22 

# Calculate the Panel
Panel <- correlation_result * YR20

# Create a data frame with Panel and Predicted Ratings
prediction_data <- data.frame(
  Panel = Panel,
  Prediction = YR20  # Assuming you want to compare YR20
)

# Remove rows with missing values in 'Panel' and 'Prediction' columns
prediction_data <- prediction_data[complete.cases(prediction_data[, c("Panel", "Prediction")]), ]

# Calculate the RMSE
rmse_temp <- sqrt(mean((prediction_data$Panel - prediction_data$Prediction)^2))

# Display the RMSE
print(paste("Root Mean Squared Error (RMSE) between Panel and Predicted Ratings:", rmse_temp))

###RMSE ratings and Panel
# Correlate average temperatures and average ratings
correlation_result <- cor(average_temp_by_year$temp, average_ratings)
AP20 <- ybyratings_data$AP20 
AP21 <- ybyratings_data$AP21 
AP22 <- ybyratings_data$AP22 

# Calculate the Panel
Panel <- correlation_result * AP20

# Create a data frame with Panel and Predicted Ratings
prediction_data <- data.frame(
  Panel = Panel,
  Prediction = AP20  # Assuming you want to compare AP20
)

# Remove rows with missing values in 'Panel' and 'Prediction' columns
prediction_data <- prediction_data[complete.cases(prediction_data[, c("Panel", "Prediction")]), ]

# Calculate the RMSE
rmse_ap <- sqrt(mean((prediction_data$Panel - prediction_data$Prediction)^2))

# Display the RMSE 
print(paste("Root Mean Squared Error (RMSE) between Temperature and Predicted Ratings:", rmse_ap))

###RMSE ratings and community

# Correlate average temperatures and average ratings
correlation_result <- cor(average_temp_by_year$temp, average_ratings)
AC20 <- ybyratings_data$AC20 
AC21 <- ybyratings_data$AC21 
AC22 <- ybyratings_data$AC22 

# Calculate the Panel
Panel <- correlation_result * AC20

# Create a data frame with Panel and Predicted Ratings
prediction_data <- data.frame(
  Panel = Panel,
  Prediction = AC20  # Assuming you want to compare AP20
)

# Remove rows with missing values in 'Panel' and 'Prediction' columns
prediction_data <- prediction_data[complete.cases(prediction_data[, c("Panel", "Prediction")]), ]

# Calculate the RMSE
rmse_ac <- sqrt(mean((prediction_data$Panel - prediction_data$Prediction)^2))

# Display the RMSE 
print(paste("Root Mean Squared Error (RMSE) between Panel and Predicted Ratings:", rmse_ac))

######### Ranges for the NRMSE 
# AC20
# Extract the AC20 column
AC20 <- ybyratings_data$AC20

# Check for missing values in AC20
missing_values <- sum(is.na(AC20))

if (missing_values > 0) {
  cat("There are missing values in the AC20 column. Handling missing values...\n")
  
  # Handle missing values by omitting them
  AC20 <- na.omit(AC20)
  
  # Find the range for AC20
  range_AC20 <- range(AC20)
  
  # Display the result
  cat("Range of AC20 column after handling missing values:\n")
  print(range_AC20)
} else {
  # Find the range for AC20
  range_AC20 <- range(AC20)
  
  # Display the result
  cat("Range of AC20 column:\n")
  print(range_AC20)
}


# Extract the AP20 column
AP20 <- ybyratings_data$AP20

# Check for missing values in AP20
missing_values <- sum(is.na(AP20))

if (missing_values > 0) {
  cat("There are missing values in the AP20 column. Handling missing values...\n")
  
  # Handle missing values by omitting them
  AP20 <- na.omit(AP20)
  
  # Find the range for AP20
  range_AP20 <- range(AP20)
  
  # Display the result
  cat("Range of AP20 column after handling missing values:\n")
  print(range_AP20)
} else {
  # Find the range for AP20
  range_AP20 <- range(AP20)
  
  # Display the result
  cat("Range of AP20 column:\n")
  print(range_AP20)
}


#### NRMSE
#the range of observed values for each case
range_ac <- range_AC20
range_ap <- range_AP20  # Replace with the range for rmse_ap


# the RMSE values for each case
rmse_ac <- rmse_ac
rmse_ap <- rmse_ap


# Calculate NRMSE for each case
nrmse_ac <- rmse_ac / range_ac
nrmse_ap <- rmse_ap / range_ap


# Display the NRMSE values
cat("NRMSE for Average Ratings and Panel Ratings (nrmse_ac):", nrmse_ac, "\n")
cat("NRMSE for Panel and Predicted Ratings (nrmse_ap):", nrmse_ap, "\n")

#calculated nrmse_ac and nrmse_ap
nrmse_ac <- nrmse_ac
nrmse_ap <- nrmse_ac

# Create a bar plot to compare NRMSE values
barplot(c(nrmse_ac, nrmse_ap), col = c("blue", "green"),
        main = "Comparison of NRMSE Values", ylab = "NRMSE", ylim = c(0, max(nrmse_ac, nrmse_ap) + 0.1))

# Add text labels
text(1, nrmse_ac + 0.01, round(nrmse_ac, 3), pos = 3, col = "blue", cex = 1.2)
text(2, nrmse_ap + 0.01, round(nrmse_ap, 3), pos = 3, col = "green", cex = 1.2)

# Add legend
legend("topright", legend = c("nrmse_ac", "nrmse_ap"), fill = c("blue", "green"))


