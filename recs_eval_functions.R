# Load required libraries
library(tm)
library(Matrix)
library(dplyr)
library(text2vec)
library(tidyverse)

# Load the dataset
books <- read.csv("book1500k-1600k.csv", stringsAsFactors = FALSE)

##CLEAN DATA
# Handle missing descriptions
books$Description[is.na(books$Description)] <- ""  # Replace NA with empty strings
# Handle missing metadata values
books$Authors[is.na(books$Authors)] <- "Unknown"
books$Publisher[is.na(books$Publisher)] <- "Unknown"
books$PublishYear[is.na(books$PublishYear)] <- median(books$PublishYear, na.rm = TRUE)


#FUNCTION FOR TF_IDF BASED ON DESCRIPTION
# Tokenize text
tokens <- word_tokenizer(books$Description)

# Create vocabulary with frequency filtering
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it, stopwords = stopwords("en"))
vocab <- prune_vocabulary(vocab, term_count_min = 5, doc_proportion_max = 0.5)

# Create vectorizer
vectorizer <- vocab_vectorizer(vocab)
# Convert to sparse matrix (memory efficient)
dtm_sparse <- create_dtm(it, vectorizer)

# Convert to TF-IDF
tfidf <- TfIdf$new()
tfidf_matrix <- tfidf$fit_transform(dtm_sparse)
# Ensure tfidf_matrix is a standard matrix before computing similarity
tfidf_similarity <- text2vec::sim2(tfidf_matrix, method = "cosine", norm = "l2")

# Check similarity matrix dimensions
dim(tfidf_similarity)
recommend_books_tf <- function(book_title, n = 5) {
  # Find the index of the book
  book_index <- which(books$Name == book_title)
  if (length(book_index) == 0) {
    stop("Book not found in dataset.")
  }
  # Get similarity scores for this book
  scores <- tfidf_similarity[book_index, ]
  # Sort scores in descending order
  recommended_indices <- order(scores, decreasing = TRUE)[2:(n+1)]  # Skip the first (itself)
  # Return the recommended book titles
  return(books$Name[recommended_indices])
}


##FUNCTION FOR METADATA 
# Combine metadata into a single string
books$metadata <- paste(books$Authors, books$Publisher, books$PublishYear)
# Reduce vocabulary size (keep only frequent words)
vocab <- create_vocabulary(itoken(books$metadata)) %>%
  prune_vocabulary(term_count_min = 10)  # Keep words that appear in at least 10 books
# Recreate TF-IDF matrix with a smaller vocabulary
vectorizer <- vocab_vectorizer(vocab)
dtm_meta <- create_dtm(itoken(books$metadata), vectorizer)
# Convert to sparse matrix
tfidf_sparse <- as(dtm_meta, "sparseMatrix")
# Compute cosine similarity efficiently
metadata_similarity <- text2vec::sim2(tfidf_sparse, method = "cosine", norm = "l2")


# ---- Step 3: Define Recommendation Function ----
recommend_books_metadata <- function(book_title, n = 5) {
  # Find the index of the book
  book_index <- which(books$Name == book_title)
  if (length(book_index) == 0) {
    stop("Book not found in dataset.")
  }
  # Get similarity scores for this book
  scores <- metadata_similarity[book_index, ]
  # Sort scores in descending order
  recommended_indices <- order(scores, decreasing = TRUE)[2:(n+1)]  # Skip the first (itself)
  # Return the recommended book titles
  return(books$Name[recommended_indices])
}

