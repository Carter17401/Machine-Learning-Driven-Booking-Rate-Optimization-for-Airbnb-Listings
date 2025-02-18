# Load necessary libraries
library(text2vec)
library(tm)
library(quanteda)

# Function for text tokenization and preprocessing
cleaning_tokenizer <- function(v) {
  v %>%
    removeNumbers %>% 
    removePunctuation %>% 
    removeWords(tm::stopwords(kind="en")) %>% 
    stemDocument %>%
    word_tokenizer 
}

# Tokenize text features
it_train_desc = itoken(train_clean$description, 
                       preprocessor = tolower,
                       tokenizer = cleaning_tokenizer, 
                       ids = train_clean$id, 
                       progressbar = FALSE)

# Create vocabulary and vectorizer
vocab_desc <- create_vocabulary(it_train_desc, ngram = c(1L, 2L))
vocab_desc_final = prune_vocabulary(vocab_desc, doc_proportion_max = 0.8, doc_proportion_min = 0.02)
vectorizer_desc = vocab_vectorizer(vocab_desc_final)

# Convert training documents into a Document-Term Matrix (DTM)
dtm_train_desc = create_dtm(it_train_desc, vectorizer_desc)

# Apply TF-IDF transformation
tfidf = TfIdf$new()
dtm_train_tfidf = fit_transform(dtm_train_desc, tfidf)

dtm_train_tfidf_dense <- as.matrix(dtm_train_tfidf)

# Convert to dataframe
dtm_train_tfidf_df <- as.data.frame(dtm_train_tfidf_dense)

# Select top TF-IDF features
mean_tfidf <- colMeans(dtm_train_tfidf_df)
sorted_mean_tfidf <- sort(mean_tfidf, decreasing = TRUE)
top_terms <- head(sorted_mean_tfidf, 50)
top_term_indices <- match(names(sorted_mean_tfidf), names(mean_tfidf))[1:length(top_terms)]
dtm_train_tfidf_df <- dtm_train_tfidf_df[, top_term_indices]
