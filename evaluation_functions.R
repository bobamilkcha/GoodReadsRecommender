#FUNCTION FOR RMSE
# Load library
library(Metrics)
# ---- Step 1: Get Actual Ratings for Books ----
# Ensure your ratings dataset is loaded
ratings <- read.csv("user_rating_1000_to_2000.csv")
# Define the rating mapping (same as before)
rating_mapping <- c(
  "it was amazing" = 5,
  "really liked it" = 4,
  "liked it" = 3,
  "it was ok" = 2,
  "did not like it" = 1,
  "This user doesn't have any rating" = NA  # Ignore missing ratings
)

# Convert text ratings to numeric values
ratings$Rating <- as.numeric(rating_mapping[ratings$Rating])
# Remove rows with NA ratings (users who didn't rate)
ratings <- na.omit(ratings)
# Check if conversion worked
table(ratings$Rating)
# Check data types
str(ratings)
# Convert ratings to numeric
ratings$Rating <- as.numeric(ratings$Rating)
# Ensure `actual_ratings` is numeric
actual_ratings <- as.numeric(ratings$Rating)

# Step 2: Predict Ratings Using Metadata Similarity
# Ensure books and ratings are available
print(head(books$Name))  # Check book names in books dataset
print(head(ratings$Name))  # Check book names in ratings dataset
# Update the predict_rating function to improve debugging and variable references
predict_rating <- function(user_id, book_title) {
  # Find book index in books dataset
  book_index <- which(books$Name == book_title)
  
  if (length(book_index) == 0) {
    print(paste("Book not found:", book_title))
    return(NA)
  }
  # Get similarity scores for the book
  similarity_scores <- metadata_similarity[book_index, ]
  # Get top N similar books
  recommended_indices <- order(similarity_scores, decreasing = TRUE)[2:6]  # Top 5 similar books
  # Debug: Print recommended book indices
  print(paste("Recommended book indices for", book_title, ":", paste(recommended_indices, collapse = ", ")))
  # Filter ratings dataset for this user and books from similar ones
  user_ratings <- ratings %>%
    filter(ID == user_id & Name %in% books$Name[recommended_indices]) %>%
    pull(Rating)
  # Debug: Check if any ratings were found
  if (length(user_ratings) == 0) {
    print(paste("No ratings for similar books by user:", user_id))
    return(NA)
  }
  # Return the average rating of similar books
  return(mean(user_ratings, na.rm = TRUE))
}

# Step 3: Apply the function and compute predictions
predicted_ratings <- mapply(predict_rating, ratings$ID, ratings$Name)
# Debug: Check predicted ratings
print(head(predicted_ratings))
# Remove NA values before calculating RMSE
valid_indices <- !is.na(predicted_ratings) & !is.na(actual_ratings)
# Compute RMSE
rmse_value <- rmse(actual_ratings[valid_indices], predicted_ratings[valid_indices])
# Output the RMSE value
print(paste("RMSE:", round(rmse_value, 3)))




# FUNCTIION FOR PRECISION AND RECALL
calculate_precision_recall <- function(user_id, recommended_books) {
  # Get the user's actual ratings
  user_ratings <- ratings %>%
    filter(ID == user_id & Name %in% recommended_books) %>%
    select(Name, Rating)
  # Define threshold for positive ratings (e.g., users who rated >= 3 liked the book)
  threshold <- 3
  user_ratings$predicted_liked <- ifelse(user_ratings$Rating >= threshold, 1, 0)
  # Assume top N recommended books are 'positive'
  user_ratings$recommended <- 1  # Assuming all recommended books are 'positive' initially
  # Calculate TP, FP, FN
  TP <- sum(user_ratings$predicted_liked == 1 & user_ratings$recommended == 1)
  FP <- sum(user_ratings$predicted_liked == 0 & user_ratings$recommended == 1)
  FN <- sum(user_ratings$predicted_liked == 1 & user_ratings$recommended == 0)
  # Calculate Precision and Recall
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  
  return(c(precision, recall))
}

# Apply the function for each user and calculate average precision and recall
user_ids <- unique(ratings$ID)
precision_recall_results <- sapply(user_ids, function(user_id) {
  # Get the recommended books for this user
  recommended_books <- recommend_books_metadata(book_title = "Go For It!: Finding Your Own Frontier", n = 5)
  
  # Check if recommended_books is empty
  if (length(recommended_books) == 0) {
    # Handle empty recommendations (e.g., return NA or zeroes for precision/recall)
    return(c(precision = NA, recall = NA))
  }
  
  # Calculate precision and recall for this user
  return(calculate_precision_recall(user_id, recommended_books))
})

# If needed, you can assign column names to the result for clarity
colnames(precision_recall_results) <- c("precision", "recall")
# Calculate average precision and recall
average_precision <- mean(precision_recall_results[1, ], na.rm = TRUE)
average_recall <- mean(precision_recall_results[2, ], na.rm = TRUE)
# Output the results
print(paste("Average Precision:", round(average_precision, 3)))
print(paste("Average Recall:", round(average_recall, 3)))