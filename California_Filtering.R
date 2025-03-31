# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(mice)
library(VIM)

# Import cleaned No NA data
df <- read_csv("cleaned_realtor_data.csv")


##############################California Filter ############################

# Filter the data to include only rows where state is "California"
df_ca <- df %>% filter(state == "California")

# Check the number of rows to verify the filter
cat("Number of rows after filtering for California:", nrow(df_ca), "\n")


##############################Duplicates#######################################

# Identify exact duplicates across all columns
duplicates_exact <- df_ca[duplicated(df_ca) | duplicated(df_ca, fromLast = TRUE), ]

# Print the duplicates to review
print(duplicates_exact)

# Remove exact duplicates, keeping only the first occurrence
df_ca_cleaned <- df_ca[!duplicated(df_ca), ]

# Verify the number of rows before and after
nrow_before <- nrow(df_ca)
nrow_after <- nrow(df_ca_cleaned)
cat("Number of rows before removing duplicates:", nrow_before, "\n")
cat("Number of rows after removing duplicates:", nrow_after, "\n")


###################################Outliers#################################
# Remove outliers for price, bed, and bath
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  df[df[[column_name]] >= lower_bound & df[[column_name]] <= upper_bound, ]
}

# Remove outliers from the dataframe
df_ca_cleaned_no_outliers <- remove_outliers(df_ca_cleaned, "price")
df_ca_cleaned_no_outliers <- remove_outliers(df_ca_cleaned_no_outliers, "bed")
df_ca_cleaned_no_outliers <- remove_outliers(df_ca_cleaned_no_outliers, "bath")
df_ca_cleaned_no_outliers <- remove_outliers(df_ca_cleaned_no_outliers, "house_size")

# Verify the number of rows before and after
nrow_before <- nrow(df_ca_cleaned)
nrow_after <- nrow(df_ca_cleaned_no_outliers)
cat("Number of rows before removing outliers:", nrow_before, "\n")
cat("Number of rows after removing outliers:", nrow_after, "\n")


#################################Box Plot##############################################
# Box plot for price
boxplot(df_ca_cleaned_no_outliers$price, main = "Boxplot for Price", ylab = "Price")

# Box plot for bed
boxplot(df_ca_cleaned_no_outliers$bed, main = "Boxplot for Bed", ylab = "Number of Beds")

# Box plot for bath
boxplot(df_ca_cleaned_no_outliers$bath, main = "Boxplot for Bath", ylab = "Number of Baths")

# Box plot for house_size
boxplot(df_ca_cleaned_no_outliers$house_size, main = "Boxplot for House Size", ylab = "House Size")
########################Save into new DAta#########################

# Save the cleaned dataframe to a CSV file
write.csv(df_ca_cleaned, "california_realtor.csv", row.names = FALSE)
