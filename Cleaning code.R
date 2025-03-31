# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(mice)
library(VIM)

##The CLEANING##
# Read the CSV file
df <- read_csv("realtor-data.csv")

# Display the structure of the dataframe
str(df)

# Show the first few rows
print(head(df))

# Display summary statistics
summary(df)

# Check for missing values
colSums(is.na(df))

# Dropping Missing Values: brokered_by, street, zip_code, City and state
# This is because brokered_by, street, zip_code, and state are not critical
# city is critical but 1407 missing rows should not be significant
df <- df[!is.na(df$brokered_by) & !is.na(df$street) & !is.na(df$zip_code) & !is.na(df$state) & !is.na(df$city), ]

# Imputing Price and Acre Lot
# Median imputation for price and acre_lot
# Since the missing values are relatively small & these variables are critical
# i decided to keep them and used median imputation
df$price[is.na(df$price)] <- median(df$price, na.rm = TRUE)
df$acre_lot[is.na(df$acre_lot)] <- median(df$acre_lot, na.rm = TRUE)

# Median imputation for bed, bath, and house_size 
# I Wouldve prefered to use KNN but i couldnt get it to work so i chose median imputation 
df$bed[is.na(df$bed)] <- median(df$bed, na.rm = TRUE)
df$bath[is.na(df$bath)] <- median(df$bath, na.rm = TRUE)
df$house_size[is.na(df$house_size)] <- median(df$house_size, na.rm = TRUE)

# Delete the prev_sold_date column
# Unused for the analysis im making
df <- df %>% select(-prev_sold_date)

# Check the summary of the dataframe to confirm imputation
# Should result in the NA rows not appearing
summary(df)

# Saving cleaned data to a new file for later use
write.csv(df, "cleaned_realtor_data.csv", row.names = FALSE)

