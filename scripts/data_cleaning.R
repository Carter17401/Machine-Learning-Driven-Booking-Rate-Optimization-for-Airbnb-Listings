train_x <- read_csv("airbnb_train_x_2024.csv")
train_y <- read_csv("airbnb_train_y_2024.csv")
test <- read_csv("airbnb_test_x_2024.csv")
#test_c <- read_csv("HomeTestDS_y.csv")
#test <- read_csv("HomeTestDS.csv")



#join the training y to the training x file
#also turn the target variables into factors
train <- cbind(train_x, train_y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score),
         high_booking_rate = as.factor(high_booking_rate))

#test <- cbind(test_p, test_c) %>%
#mutate(perfect_rating_score = as.factor(perfect_rating_score),
#      high_booking_rate = as.factor(high_booking_rate))

test <- test %>%
  mutate(host_acceptance_rate = ifelse(grepl("%$", test$host_acceptance_rate), as.numeric(gsub("%", "", test$host_acceptance_rate)), 
                                       as.numeric(test$host_acceptance_rate)),
         host_acceptance_rate = if_else(is.na(host_acceptance_rate), 0, host_acceptance_rate),
         host_acceptance_rate = host_acceptance_rate/100)



# Identify columns with more than 5000 NA values in 'train'
columns_na_train <- names(which(colSums(is.na(train)) > 45000))

# For each column with many NA values, convert it into binary column
for (col in columns_na_train) {
  train <- train %>%
    mutate(!!paste0(col, "_present") := ifelse(!is.na(.data[[col]]), 1, 0))  # Remove the original column
  test <- test %>%
    mutate(!!paste0(col, "_present") := ifelse(!is.na(.data[[col]]), 1, 0))
}

# Function to calculate score - Rajat's code


clean_data <- function(data) {
  
  # Remove unnecessary columns
  data <- data %>%
    select(-c( zipcode,
               experiences_offered, host_name, 
               host_about, notes))
  
  # Impute missing values
  data <- data %>%
    mutate(
      cancellation_policy = ifelse(cancellation_policy %in% 
                                     c("strict", "super_strict_30"), "strict", cancellation_policy),
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
        (security_deposit > 700) & (security_deposit <= 1000) ~ "Large",
        is.na(security_deposit) ~ "Unstated"
      ),
      host_since = ifelse(is.na(host_since),first_review, host_since),
      host_since = as.Date(host_since, origin = "1899-12-30"),
      first_review = as.Date(first_review, origin = "1899-12-30"),
      price = ifelse(is.na(price), mean(price, na.rm = TRUE), price))
  
  data_week_check <- data %>%
    select(price, weekly_price) %>%
    filter(!is.na(weekly_price)) %>%
    mutate(ratio_weekly = weekly_price/price) %>%
    summarize(mean_ratio = mean(ratio_weekly))
  
  data_month_check <- data %>%
    select(price, monthly_price) %>%
    filter(!is.na(monthly_price)) %>%
    mutate(ratio_monthly = monthly_price/price) %>%
    summarize(mean_ratio = mean(ratio_monthly))
  
  data <- data %>%
    mutate(monthly_price = ifelse(is.na(monthly_price), price * data_month_check$mean_ratio , monthly_price),
           monthly_price = round(monthly_price,0),
           weekly_price = ifelse(is.na(weekly_price), price * data_week_check$mean_ratio , weekly_price),
           weekly_price = round(weekly_price,0))
  
  data <- data %>%
    select(-c(host_location))  # Drop unnecessary columns
  
  return(data)
}


#host_location host_neighbourhood remove non english and NA

# Apply the cleaning function to your dataset
train_clean <- clean_data(train)
test_clean <- clean_data(test)

## Code to make list of unique verifications
split_ver <- strsplit(as.character(train_clean$host_verifications), ",")
unique_verifications <- na.omit(unique(unlist(split_ver)))

##Test clean

split_verifications <- strsplit(as.character(test_clean$host_verifications), ",")
verifications_matrix <- sapply(unique_verifications, function(x) {
  sapply(split_verifications, function(y) { 
    as.numeric(x %in% y)
  })
})
length(verifications_matrix)

test_clean <- cbind(test_clean, verifications_matrix)
test_clean <- test_clean[, !names(test_clean) %in% "host_verifications"]

##Train clean

verifications_matrix <- sapply(unique_verifications, function(x) {
  sapply(split_ver, function(y) { 
    as.numeric(x %in% y)
  })
})
length(verifications_matrix)

train_clean <- cbind(train_clean, verifications_matrix)
train_clean <- train_clean[, !names(train_clean) %in% "host_verifications"]


## Code for amenities

## Code to make list of unique amenities
split_ver <- strsplit(as.character(train_clean$amenities), ",")
unique_amenities <- na.omit(unique(unlist(split_ver)))

##Test clean

split_amenities <- strsplit(as.character(test_clean$amenities), ",")
amenities_matrix <- sapply(unique_amenities, function(x) {
  sapply(split_amenities, function(y) { 
    as.numeric(x %in% y)
  })
})
length(amenities_matrix)

test_clean <- cbind(test_clean, amenities_matrix)
test_clean <- test_clean[, !names(test_clean) %in% "amenities"]

##Train clean

amenities_matrix <- sapply(unique_amenities, function(x) {
  sapply(split_ver, function(y) { 
    as.numeric(x %in% y)
  })
})
length(amenities_matrix)

train_clean <- cbind(train_clean, amenities_matrix)
train_clean <- train_clean[, !names(train_clean) %in% "amenities"]

summary(train_clean)
summary(test_clean)

## TRain amenities



columns_with_na <- colSums(is.na(train_clean)) > 0

# Print the column names with NA values
print(names(train_clean)[columns_with_na])


## TEXT MINING



train_clean <- train_clean %>%
  mutate(
         transit = ifelse(is.na(transit), "Nonegiven", transit),
         description = ifelse(is.na(description), "Nonegiven", description),
         access = ifelse(is.na(access), "Nonegiven", access),
         neighborhood_overview = ifelse(is.na(neighborhood_overview), "Nonegiven", neighborhood_overview),
         interaction = ifelse(is.na(interaction), "Nonegiven", interaction),
         name = ifelse(is.na(name), "Nonegiven", name),
         house_rules = ifelse(is.na(house_rules), "Nonegiven", house_rules),
         space = ifelse(is.na(space), "Nonegiven", space),
         features = ifelse(is.na(features), "Nonegiven", features),
         id = row_number()) 

summary(train_clean)
cleaning_tokenizer <- function(v) {
  v %>%
    removeNumbers %>% #remove all numbers
    removePunctuation %>% #remove all punctuation
    removeWords(tm::stopwords(kind="en")) %>% #remove stopwords
    stemDocument %>%
    word_tokenizer 
}

it_train_transit = itoken(train_clean$transit, 
                          preprocessor = tolower, #preprocessing by converting to lowercase
                          tokenizer = cleaning_tokenizer, 
                          ids = train_clean$id, 
                          progressbar = FALSE)


it_train_desc = itoken(train_clean$description, 
                       preprocessor = tolower, #preprocessing by converting to lowercase
                       tokenizer = cleaning_tokenizer, 
                       ids = train_clean$id, 
                       progressbar = FALSE)

it_train_access = itoken(train_clean$access, 
                         preprocessor = tolower, #preprocessing by converting to lowercase
                         tokenizer = cleaning_tokenizer, 
                         ids = train_clean$id, 
                         progressbar = FALSE)

it_train_neigh = itoken(train_clean$neighborhood_overview, 
                        preprocessor = tolower, #preprocessing by converting to lowercase
                        tokenizer = cleaning_tokenizer, 
                        ids = train_clean$id, 
                        progressbar = FALSE)


it_train_interaction = itoken(train_clean$interaction, 
                              preprocessor = tolower, #preprocessing by converting to lowercase
                              tokenizer = cleaning_tokenizer, 
                              ids = train_clean$id, 
                              progressbar = FALSE)


it_train_name = itoken(train_clean$name, 
                       preprocessor = tolower, #preprocessing by converting to lowercase
                       tokenizer = cleaning_tokenizer, 
                       ids = train_clean$id, 
                       progressbar = FALSE)

it_train_house_rules = itoken(train_clean$house_rules, 
                              preprocessor = tolower, #preprocessing by converting to lowercase
                              tokenizer = cleaning_tokenizer, 
                              ids = train_clean$id, 
                              progressbar = FALSE)

it_train_space = itoken(train_clean$space, 
                        preprocessor = tolower, #preprocessing by converting to lowercase
                        tokenizer = cleaning_tokenizer, 
                        ids = train_clean$id, 
                        progressbar = FALSE)

it_train_features = itoken(train_clean$features, 
                           preprocessor = tolower, #preprocessing by converting to lowercase
                           tokenizer = cleaning_tokenizer, 
                           ids = train_clean$id, 
                           progressbar = FALSE)

it_train_juris = itoken(train_clean$jurisdiction_names, 
                        preprocessor = tolower, #preprocessing by converting to lowercase
                        tokenizer = cleaning_tokenizer, 
                        ids = train_clean$id, 
                        progressbar = FALSE)


it_train_hneigh = itoken(train_clean$host_neighbourhood, 
                         preprocessor = tolower, #preprocessing by converting to lowercase
                         tokenizer = cleaning_tokenizer, 
                         ids = train_clean$id, 
                         progressbar = FALSE)


##Transit Vocab
stop_words_transit <- c("your", "s", "get", "also", "im", "go", "take")
vocab_transit <- create_vocabulary(it_train_transit, ngram = c(1L, 2L), stopwords = stop_words_transit)

vocab_transit_final = prune_vocabulary(vocab_transit, doc_proportion_max = 0.8, doc_proportion_min = 0.02)
vectorizer_transit = vocab_vectorizer(vocab_transit_final)

##Description vocab
stop_words_desc <- c("also", "can", "us")
vocab_desc <- create_vocabulary(it_train_desc, ngram = c(1L, 2L), stopwords = stop_words_desc)

vocab_desc_final = prune_vocabulary(vocab_desc, doc_proportion_max = 0.8, doc_proportion_min = 0.02)

vectorizer_desc = vocab_vectorizer(vocab_desc_final)

##Neighborhood overview vocab
stop_words_neigh <- c("will", "can")
vocab_neigh <- create_vocabulary(it_train_neigh, ngram = c(1L, 2L), stopwords = stop_words_neigh)

vocab_neigh_final = prune_vocabulary(vocab_neigh,  doc_proportion_max = 0.8, doc_proportion_min = 0.02)

vectorizer_neigh = vocab_vectorizer(vocab_neigh_final)

## Access vocab
stop_words_access <- c("also", "can")
vocab_access <- create_vocabulary(it_train_access, ngram = c(1L, 2L), stopwords = stop_words_access)

vocab_access_final = prune_vocabulary(vocab_access,  doc_proportion_max = 0.8, doc_proportion_min = 0.02)

vectorizer_access = vocab_vectorizer(vocab_access_final)

stop_words_interaction <- c("your", "s", "get", "also", "im", "go", "take")
vocab_interaction <- create_vocabulary(it_train_interaction, ngram = c(1L, 2L), stopwords = stop_words_interaction)

vocab_interaction_final = prune_vocabulary(vocab_interaction, doc_proportion_max = 0.8, doc_proportion_min = 0.02)
?prune_vocabulary
vectorizer_interaction = vocab_vectorizer(vocab_interaction_final)

## Name vocab
stop_words_name <- c( "the", "a", "an", "and", "of", "in", "on", "at", "with", "for")
vocab_name <- create_vocabulary(it_train_name, ngram = c(1L, 2L), stopwords = stop_words_name)

vocab_name_final = prune_vocabulary(vocab_name, doc_proportion_max = 0.8, doc_proportion_min = 0.02)

vectorizer_name = vocab_vectorizer(vocab_name_final)

## House Rules vocab
stop_words_house_rules <- c("the", "a", "an", "and", "of", "in", "on", "at", "with", "for")
vocab_house_rules <- create_vocabulary(it_train_house_rules, ngram = c(1L, 2L), stopwords = stop_words_house_rules)

vocab_house_rules_final = prune_vocabulary(vocab_house_rules,  doc_proportion_max = 0.8, doc_proportion_min = 0.02)

vectorizer_house_rules = vocab_vectorizer(vocab_house_rules_final)

## Space vocab
stop_words_space <- c("the", "a", "an", "and", "of", "in", "on", "at", "with", "for")
vocab_space <- create_vocabulary(it_train_space, ngram = c(1L, 2L), stopwords = stop_words_space)

vocab_space_final = prune_vocabulary(vocab_space,  doc_proportion_max = 0.8, doc_proportion_min = 0.02)

vectorizer_space = vocab_vectorizer(vocab_space_final)

#features vocab
stop_words_features <- c("host", "is", "has", "with","from")
vocab_features <- create_vocabulary(it_train_features, ngram = c(1L, 2L), stopwords = stop_words_features)

vocab_features_final = prune_vocabulary(vocab_features, doc_proportion_max = 0.8, doc_proportion_min = 0.02)
?prune_vocabulary
vectorizer_features = vocab_vectorizer(vocab_features_final)

##Jurisdiction Names vocab
stop_words_juris <- c("city", "state")
vocab_juris <- create_vocabulary(it_train_juris, ngram = c(1L, 2L), stopwords = stop_words_juris)

vocab_juris_final = prune_vocabulary(vocab_juris,  doc_proportion_max = 0.8, doc_proportion_min = 0.02)

vectorizer_juris = vocab_vectorizer(vocab_juris_final)

##Host Neighborhood vocab

vocab_hneigh <- create_vocabulary(it_train_hneigh, ngram = c(1L, 2L), stopwords = stop_words_juris)

vocab_hneigh_final = prune_vocabulary(vocab_hneigh,  doc_proportion_max = 0.8, doc_proportion_min = 0.02)

vectorizer_hneigh = vocab_vectorizer(vocab_hneigh_final)

# Convert the training documents into a DTM
dtm_train_transit = create_dtm(it_train_transit, vectorizer_transit)
dtm_train_desc = create_dtm(it_train_desc, vectorizer_desc)
dtm_train_neigh = create_dtm(it_train_neigh, vectorizer_neigh)
dtm_train_access = create_dtm(it_train_access, vectorizer_access)
dtm_train_interaction = create_dtm(it_train_interaction, vectorizer_interaction)
dtm_train_name = create_dtm(it_train_name, vectorizer_name)
dtm_train_house_rules = create_dtm(it_train_house_rules, vectorizer_house_rules)
dtm_train_space = create_dtm(it_train_space, vectorizer_space)
dtm_train_features = create_dtm(it_train_features, vectorizer_features)
dtm_train_juris = create_dtm(it_train_juris, vectorizer_juris)
dtm_train_hneigh = create_dtm(it_train_hneigh, vectorizer_hneigh)

dtm_train_bin <- cbind(dtm_train_access, dtm_train_neigh, dtm_train_desc, dtm_train_transit,
                       dtm_train_interaction, dtm_train_name, dtm_train_house_rules, dtm_train_space,
                       dtm_train_features, dtm_train_juris, dtm_train_hneigh)
# Make a TFIDF DTM
tfidf = TfIdf$new()
dtm_train_tfidf = fit_transform(dtm_train_bin, tfidf)
#dtm_train_tfidf <- as.data.frame(dtm_train_tfidf)
dtm_train_tfidf_dense <- as.matrix(dtm_train_tfidf)

# Convert regular matrix to dataframe
dtm_train_tfidf_df_1 <- as.data.frame(dtm_train_tfidf_dense)

mean_tfidf <- colMeans(dtm_train_tfidf_df_1)

# Sort mean TF-IDF scores in descending order
sorted_mean_tfidf <- sort(mean_tfidf, decreasing = TRUE)

# Select top terms with highest mean TF-IDF scores
top_terms <- head(sorted_mean_tfidf, 50) 

# Get indices of top terms
top_term_indices <- match(names(sorted_mean_tfidf), names(mean_tfidf))[1:length(top_terms)]

# Filter DTM to keep only columns of top terms
dtm_train_tfidf_df<- dtm_train_tfidf_df_1[, top_term_indices]

# Convert filtered DTM to a dataframe
dtm_train_tfidf_df <- as.data.frame(as.matrix(dtm_tfidf_filtered))
