library(tidyr)
library(tidyverse)
library(dplyr)

setwd("/Users/carter/Library/CloudStorage/OneDrive-UniversityofMaryland/Data Mining/Project")

train_clean <- read.csv("airbnb_train_x_2024.csv")

train_clean <- train_clean %>%
  mutate(host_response_rate = ifelse(is.na(train_clean$host_response_rate), 0 , host_response_rate))

train_clean <- train_clean %>%
  mutate(host_total_listings_count = ifelse(is.na(train_clean$host_total_listings_count), 0 , host_total_listings_count))

train_clean <- train_clean %>%
  mutate(room_type = ifelse(is.na(train_clean$room_type), "Other", room_type))

train_clean <- train_clean %>%
  mutate(license = ifelse(is.na(train_clean$license), "No License", license))

train_clean <- train_clean %>%
  mutate(interaction = ifelse(is.na(train_clean$interaction), "No Interaction", interaction))

train_clean <- train_clean %>%
  mutate(name = ifelse(is.na(train_clean$name), "No Interaction", name))

na_count_host_response_rate <- sum(is.na(train_clean$host_response_rate))
na_count_room_type <- sum(is.na(train_clean$room_type))
na_count_host_total_listings_count <- sum(is.na(train_clean$host_total_listings_count))
na_count_license <- sum(is.na(train_clean$license))
na_count_interaction <- sum(is.na(train_clean$interaction))
na_count_name <- sum(is.na(train_clean$name))

write.csv(train_clean, file = "airbnb_train_x_2024_clean.csv", row.names = FALSE)

set.seed(1)

cleaning_tokenizer <- function(v) {
  v %>%
    #removeNumbers %>% #remove all numbers
    removePunctuation %>% #remove all punctuation
    removeWords(stopwords(kind="en")) %>% #remove stopwords
    stemDocument %>%
    word_tokenizer 
}

# Iterate over the individual documents and convert them to tokens
# Uses the function defined above.
it_train = itoken(train_clean$interaction, 
                  preprocessor = tolower, #preprocessing by converting to lowercase
                  tokenizer = cleaning_tokenizer, 
                  ids = train_clean$id, 
                  progressbar = FALSE)

# Step 2: create the vocabulary

# Create the vocabulary from the itoken object
vocab = create_vocabulary(it_train)

#can define your own list of stopwords to supplement the main one, if you want
#stop_words = c("will", "new", "us")
#vocab2 <- create_vocabulary(it_train, stopwords = stop_words)

#Include ngrams
vocab_interaction <- create_vocabulary(it_train, ngram = c(1L, 2L))

#Prune vocabulary
?prune_vocabulary

#Try a few values here - what happens?
vocab_final = prune_vocabulary(vocab, term_count_min = 10, doc_proportion_max = 0.75)

# Step 3: Vectorize 

# Create a vectorizer object using the vocabulary we learned
vectorizer = vocab_vectorizer(vocab_final)

# Convert the training documents into a DTM
dtm_train = create_dtm(it_train, vectorizer)
dim(dtm_train)






    
         