# Load necessary libraries
library(tidyverse)
library(dplyr)

# Load data
train_x <- read_csv("airbnb_train_x_2024.csv")
train_y <- read_csv("airbnb_train_y_2024.csv")
test <- read_csv("airbnb_test_x_2024.csv")

# Merge train_x and train_y
train <- cbind(train_x, train_y) %>%
  mutate(
    perfect_rating_score = as.factor(perfect_rating_score),
    high_booking_rate = as.factor(high_booking_rate)
  )

# Convert percentage strings to numeric values in test dataset
test <- test %>%
  mutate(
    host_acceptance_rate = ifelse(
      grepl("%$", test$host_acceptance_rate),
      as.numeric(gsub("%", "", test$host_acceptance_rate)),
      as.numeric(test$host_acceptance_rate)
    ),
    host_acceptance_rate = if_else(is.na(host_acceptance_rate), 0, host_acceptance_rate),
    host_acceptance_rate = host_acceptance_rate / 100
  )

# Identify columns with many NA values and create binary indicators
columns_na_train <- names(which(colSums(is.na(train)) > 45000))
for (col in columns_na_train) {
  train <- train %>% mutate(!!paste0(col, "_present") := ifelse(!is.na(.data[[col]]), 1, 0))
  test <- test %>% mutate(!!paste0(col, "_present") := ifelse(!is.na(.data[[col]]), 1, 0))
}

# Function to clean data
clean_data <- function(data) {
  data <- data %>% select(-c(zipcode, experiences_offered, host_name, host_about, notes))
  
  # Impute missing values
  data <- data %>% mutate(
    cancellation_policy = ifelse(cancellation_policy %in% c("strict", "super_strict_30"), "strict", cancellation_policy),
    host_response_time = if_else(is.na(host_response_time), "Unknown", host_response_time),
    host_neighbourhood = if_else(is.na(host_neighbourhood), host_location, host_neighbourhood),
    jurisdiction_names = if_else(is.na(jurisdiction_names), smart_location, jurisdiction_names),
    cleaning_fee = if_else(is.na(cleaning_fee), 0, cleaning_fee),
    accommodates = if_else(is.na(accommodates), mean(accommodates, na.rm = TRUE), accommodates),
    bathrooms = if_else(is.na(bathrooms), mean(bathrooms, na.rm = TRUE), bathrooms),
    bedrooms = if_else(is.na(bedrooms), mean(bedrooms, na.rm = TRUE), bedrooms),
    beds = if_else(is.na(beds), mean(beds, na.rm = TRUE), beds),
    host_response_rate = if_else(is.na(host_response_rate), 0, host_response_rate),
    host_total_listings_count = if_else(is.na(host_total_listings_count), 0, host_total_listings_count),
    property_type = if_else(is.na(property_type), "Other", property_type),
    license = if_else(is.na(license), "No License", license),
    host_acceptance_rate = if_else(is.na(host_acceptance_rate), 0, host_acceptance_rate),
    room_type = if_else(is.na(room_type), "Other", room_type),
    bed_type = if_else(is.na(bed_type), "Other", bed_type),
    market = if_else(is.na(market), city, market),
    city = if_else(is.na(city), market, city),
    summary = if_else(is.na(summary), 'NoSummary', summary),
    neighborhood_group = if_else(is.na(neighborhood_group), city, neighborhood_group),
    square_feet = if_else(is.na(square_feet), mean(square_feet, na.rm = TRUE), bedrooms),
    security_deposit = case_when(
      security_deposit <= 250 ~ "Small",
      (security_deposit > 250) & (security_deposit <= 500) ~ "Medium",
      (security_deposit > 500) & (security_deposit <= 750) ~ "Moderate",
      (security_deposit > 750) & (security_deposit <= 1000) ~ "Large",
      is.na(security_deposit) ~ "Unstated"
    ),
    host_since = ifelse(is.na(host_since), first_review, host_since),
    host_since = as.Date(host_since, origin = "1899-12-30"),
    first_review = as.Date(first_review, origin = "1899-12-30"),
    price = ifelse(is.na(price), mean(price, na.rm = TRUE), price)
  )
  
  return(data)
}

# Apply data cleaning function
train_clean <- clean_data(train)
test_clean <- clean_data(test)
