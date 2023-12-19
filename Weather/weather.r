# Load the ggplot2 library
library(ggplot2)

# Set the path to your CSV file
file_path <- "C:\\Tequila\\weather.csv"

# Read the CSV file into a data frame
weather <- read.csv(file_path)

# Assuming datetime is in the format "YYYY-MM-DD HH:MM:SS"
weather$datetime <- as.POSIXct(weather$datetime, format = "%Y-%m-%d")

# Create a ggplot scatter plot
ggplot(weather, aes(x = datetime, y = temp)) +
  geom_point() +
  labs(x = "Date", y = "Temperature", title = "Typical Temperature Curve")

