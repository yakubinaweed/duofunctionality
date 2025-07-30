# R/serversecond/gmms.R (Complete Script)

# Ensure you have 'mclust' and 'moments' packages installed
# install.packages("mclust")
# install.packages("moments")
library(mclust)
library(moments)

# Function to calculate skewness (requires 'moments' package)
calculate_skewness <- function(x) {
  if (!requireNamespace("moments", quietly = TRUE)) {
    warning("Package 'moments' needed for skewness calculation. Please install it: install.packages('moments')")
    return(NA)
  }
  moments::skewness(x, na.rm = TRUE)
}

# Function to apply conditional Yen-Johnson (simplified to log for high positive skewness)
# For a more robust Yen-Johnson that finds optimal lambda, consider 'car::powerTransform'.
# This simplified version applies a log transformation if skewness is high and positive.
# It adds a small offset to prevent log(0) or log(negative) if data contains such values.
apply_conditional_yen_johnson <- function(data_vector, skewness_threshold = 0.5) {
  current_skewness <- calculate_skewness(data_vector)

  if (is.na(current_skewness)) {
    warning("Skewness could not be calculated (e.g., all NAs or not enough data). No transformation applied.")
    return(list(transformed_data = data_vector, transformation_applied = FALSE))
  }

  if (current_skewness > skewness_threshold) {
    # Add a small constant to handle zero/negative values if they were present
    # For HGB, values are typically positive, so min(data_vector[data_vector > 0]) is safe.
    # A common approach is `log(x + 1)` or `log(x + small_const)`.
    # Using `min(data_vector[data_vector > 0], na.rm = TRUE) / 2` attempts to make the offset relevant to data scale.
    offset <- 0 # Default offset
    if(any(data_vector <= 0, na.rm = TRUE)) {
      # If there are non-positive values, find a suitable offset
      min_val <- min(data_vector, na.rm = TRUE)
      if(min_val <= 0) {
        offset <- abs(min_val) + 1 # Ensure positive after offset
      }
    } else if (min(data_vector, na.rm = TRUE) == 0) {
      offset <- 1e-6 # A tiny offset for exactly zero values
    }

    transformed_data <- log(data_vector + offset)
    return(list(transformed_data = transformed_data, transformation_applied = TRUE))
  } else {
    return(list(transformed_data = data_vector, transformation_applied = FALSE))
  }
}

# Function to run Gaussian Mixture Model (GMM)
# Expects data_for_gmm to already have 'HGB' and 'Age' columns (which should be z-standardized)
run_gmm <- function(data_for_gmm, min_components = 2, max_components = 5) {
  if (!requireNamespace("mclust", quietly = TRUE)) {
    stop("Package 'mclust' needed for GMM. Please install it: install.packages('mclust')")
  }

  # Ensure data_for_gmm is a data frame with columns named 'HGB' and 'Age'
  if (!("HGB" %in% colnames(data_for_gmm) && "Age" %in% colnames(data_for_gmm))) {
    stop("Input data for GMM must contain 'HGB' and 'Age' columns (likely z-transformed).")
  }

  # Select only numeric columns for clustering
  gmm_input_data <- data_for_gmm[, c("HGB", "Age")]

  # Run Mclust
  # Using various modelNames to allow Mclust to find the best fit based on BIC.
  # EII, VII, EEE, VVV are common choices.
  gmm_model <- mclust::Mclust(gmm_input_data, G = min_components:max_components,
                               modelNames = c("EII", "VII", "EEE", "VVV", "VEV", "VEE", "EVE", "EEV"))

  if (is.null(gmm_model)) {
    stop("GMM model could not be fitted. Check data for issues (e.g., too few observations, constant values).")
  }

  return(gmm_model)
}

# Function to assign clusters based on the model's classification
assign_clusters <- function(data_df, gmm_model) {
  # Add the classification (cluster assignment) from the Mclust model to the dataframe
  data_df$cluster <- gmm_model$classification
  return(data_df)
}