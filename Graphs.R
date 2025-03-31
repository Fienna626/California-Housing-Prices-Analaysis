# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(mice)
library(VIM)
library(tidyverse)
library(ggplot2)
library(viridis)
library(gridExtra)

# Import cleaned No NA data
df_ca_cleaned <- read_csv("California_realtor.csv")

city_counts <- df_ca_cleaned %>%
  group_by(city) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Display the top 10 cities with the most data points
top_10_cities <- head(city_counts, 10)
print(top_10_cities)

# Filter the dataset to include only the top 10 cities
df_top_10 <- df_ca_cleaned %>%
  filter(city %in% top_10_cities$city)

######################################Bar Graph###########################################################

# Top 10 Cities BarGraph
ggplot(top_10_cities, aes(x = reorder(city, -count), y = count, fill = count)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#D3A9F1", high = "#4B0082") +  # Gradient from light purple to dark purple
  labs(title = "Top 10 Cities with Most Data Points",
       x = "City",
       y = "Number of Data Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##################################Scatter Plot#########################################################

# Filter the dataset to include only the top 10 cities
df_top_10 <- df_ca_cleaned %>%
  filter(city %in% top_10_cities$city)

# Function to remove outliers based on IQR
remove_outliers_iqr <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  df %>%
    filter(df[[column_name]] >= (Q1 - 1.5 * IQR) & df[[column_name]] <= (Q3 + 1.5 * IQR))
}

# Remove outliers for 'price' and 'acre_lot' in the top 10 cities
df_no_outliers <- df_top_10 %>%
  remove_outliers_iqr('price') %>%
  remove_outliers_iqr('acre_lot')

# Scatter plot of prices vs. acre lot sizes with outliers removed
ggplot(df_top_10, aes(x = acre_lot, y = price)) +
  geom_hex(bins = 30) +  # Adjust the number of bins as needed
  scale_fill_viridis_c() +  
  labs(title = "Density of Price vs. Acre Lot in Popular California Cities",
       x = "Lot Size (Acres)",
       y = "Price (USD)",
       fill = "Density") +
  theme_minimal() +
  scale_x_log10(labels = scales::comma) +  # Format x-axis with commas
  scale_y_log10(labels = scales::comma)    # Format y-axis with commas



###############################Scatter Plot Cities#######################################

# Count data points by city and get the top 10 cities
city_counts <- df_ca_cleaned %>%
  group_by(city) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Display the top 10 cities with the most data points
top_10_cities <- head(city_counts, 10)

# Filter the dataset to include only the top 10 cities
df_top_10 <- df_ca_cleaned %>%
  filter(city %in% top_10_cities$city)

# Function to remove outliers based on IQR
remove_outliers_iqr <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  df %>%
    filter(df[[column_name]] >= (Q1 - 1.5 * IQR) & df[[column_name]] <= (Q3 + 1.5 * IQR))
}

# Remove outliers for 'price' and 'house_size' in the top 10 cities
df_no_outliers <- df_top_10 %>%
  remove_outliers_iqr('price') %>%
  remove_outliers_iqr('house_size')

# Custom color palette
custom_colors <- c(
  "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
  "#66A61E", "#E6AB02", "#A6761D", "#666666",
  "#1F78B4", "#B2DF8A"
)

# Scatter plot of prices vs. house size with outliers removed, colored by city
plot <- ggplot(df_no_outliers, aes(x = house_size, y = price, color = city)) +
  geom_point(alpha = 0.5) +
  labs(title = "Prices vs. House Size in Top 10 Cities (Outliers Removed)",
       x = "House Size (sqft)",
       y = "Price ($)") +
  theme_minimal() +
  scale_color_manual(values = custom_colors)  # Use custom colors

# Add regression line
plot <- plot +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed")

# Print the plot
print(plot)

# Linear regression model
model <- lm(price ~ house_size, data = df_no_outliers)

# Display the regression equation
summary(model)




#########################################Histograms########################################

# Function to remove outliers using IQR
remove_outliers <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  df %>% filter(df[[column]] >= lower_bound & df[[column]] <= upper_bound)
}

# Import cleaned data
df_ca_cleaned <- read_csv("California_realtor.csv")

# Remove outliers in house_size
df_no_outliers <- remove_outliers(df_ca_cleaned, "house_size")

# Count data points by city and get the top 10 cities
city_counts <- df_no_outliers %>%
  group_by(city) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

# Display the top 10 cities with the most data points
top_10_cities <- head(city_counts, 10)

# Filter the dataset to include only the top 10 cities
df_top_10 <- df_no_outliers %>%
  filter(city %in% top_10_cities$city)

# Define the global max and min for house_size
global_max <- max(df_top_10$house_size, na.rm = TRUE)
global_min <- min(df_top_10$house_size, na.rm = TRUE)

# Define bin width
bin_width <- (global_max - global_min) / 50  # Example: 1/50th of the global range

# Calculate overall median house size for all 10 cities combined
median_house_size_overall <- median(df_top_10$house_size, na.rm = TRUE)

# Create the histogram with density plot overlaid and vertical line for overall median house size
ggplot(df_top_10, aes(x = house_size)) +
  geom_histogram(aes(y = ..density..), binwidth = bin_width, fill = "#4B0082", color = "black", alpha = 0.6) +
  geom_density(color = "#FF4500", size = 1) +  # Overlay density plot with a contrasting color
  geom_vline(xintercept = median_house_size_overall, linetype = "dashed", color = "red", size = 1) +  # Add vertical line for overall median size
  annotate("text", x = median_house_size_overall, y = Inf, label = paste("Median Size =", round(median_house_size_overall, 2)), vjust = -0.5, color = "red") +
  facet_wrap(~ city, scales = "fixed") +  # Ensure all plots use the same x-axis scale
  labs(title = "Distribution of House Sizes in Top 10 Cities (Outliers Removed)",
       x = "House Size (sqft)",
       y = "Density") +
  theme_minimal() +
  coord_cartesian(xlim = c(global_min, global_max))



#####################################Prices boxplot######################################

# Import cleaned data (ensure correct path)
df_ca_cleaned <- read_csv("California_realtor.csv")

# Function to remove outliers using IQR
remove_outliers <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  df %>% filter(df[[column]] >= lower_bound & df[[column]] <= upper_bound)
}

# Remove outliers in price
df_no_outliers <- remove_outliers(df_ca_cleaned, "price")

# Count data points by city and get the top 10 cities
city_counts <- df_no_outliers %>%
  group_by(city) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Display the top 10 cities with the most data points
top_10_cities <- head(city_counts, 10)

# Filter the dataset to include only the top 10 cities
df_top_10 <- df_no_outliers %>%
  filter(city %in% top_10_cities$city)

# Boxplot for prices in the top 10 cities
ggplot(df_top_10, aes(x = city, y = price, fill = city)) +
  geom_boxplot() +
  scale_fill_viridis_d() +  # Use a color palette that avoids white
  labs(title = "Boxplot of Prices in Top 10 Cities (Outliers Removed)",
       x = "City",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability



###########################################Bedroom/Bathroom Bar Charts##########################################

# Function to remove outliers using IQR for bedrooms and bathrooms
remove_outliers_numeric <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  df %>% filter(df[[column]] >= lower_bound & df[[column]] <= upper_bound)
}

# Remove outliers in bedrooms and bathrooms
df_no_outliers_bath_bed <- remove_outliers_numeric(df_top_10, "bed")
df_no_outliers_bath_bed <- remove_outliers_numeric(df_no_outliers_bath_bed, "bath")

# Calculate counts for bedrooms
bedroom_counts <- df_no_outliers_bath_bed %>%
  group_by(city, bed) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(type = "Bedrooms")

# Calculate counts for bathrooms
bathroom_counts <- df_no_outliers_bath_bed %>%
  group_by(city, bath) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(type = "Bathrooms") %>%
  rename(bed = bath)  # Rename 'bath' to 'bed' for consistency in plotting

# Combine both counts into one data frame
combined_counts <- bind_rows(bedroom_counts, bathroom_counts)

# Display the first few rows of the combined counts
print(head(combined_counts))

# Create the overlaid bar plot with updated data
ggplot(combined_counts, aes(x = as.factor(bed), y = count, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ city) +
  labs(title = "Distribution of Bedrooms and Bathrooms in Top 10 Cities (Outliers Removed)",
       x = "Number",
       y = "Number of Properties",
       fill = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))