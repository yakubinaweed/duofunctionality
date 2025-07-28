library(mclust)

run_gmm <- function(data_mat, G_range) {
  print("DEBUG (gmms.R): run_gmm function called.")
  print(paste("DEBUG (gmms.R): Input data_mat class:", class(data_mat)))
  print(paste("DEBUG (gmms.R): Input data_mat length:", length(data_mat)))
  print(paste("DEBUG (gmms.R): G_range parameter:", G_range))

  if (!is.numeric(data_mat)) {
    stop("Input data_mat must be numeric for GMM analysis.")
  }
  if (length(data_mat) < 2) {
    stop("Not enough data points for GMM analysis. Need at least 2.")
  }

  print("DEBUG (gmms.R): Summary of data_mat before Mclust:")
  print(summary(data_mat))
  print(paste("DEBUG (gmms.R): Number of unique values in data_mat:", length(unique(data_mat))))
  if (length(unique(data_mat)) < 2) {
    print("DEBUG (gmms.R): WARNING: data_mat contains only one unique value or very few values. Mclust may struggle.")
  }

  tryCatch({
    print("DEBUG (gmms.R): Attempting Mclust fit with modelNames = \"EII\".")
    # Changed modelNames from "VVV" to "EII"
    gmm_model <- Mclust(data_mat, G = G_range, modelNames = "EII")
    print("DEBUG (gmms.R): Mclust fit completed.")
    return(gmm_model)
  }, error = function(e) {
    print(paste("DEBUG ERROR (gmms.R): Error during Mclust fit:", e$message))
    stop(paste("GMM Mclust Error:", e$message)) # Re-throw the error to be caught by server.R's tryCatch
  })
}

assign_clusters <- function(df, gmm_model) {
  print("DEBUG (gmms.R): assign_clusters function called.")
  if (is.null(gmm_model) || is.null(gmm_model$classification)) {
    warning("GMM model or classification is NULL. Cannot assign clusters.")
    return(df)
  }
  if (nrow(df) != length(gmm_model$classification)) {
    warning("Dataframe row count does not match GMM classification length. Clusters might be assigned incorrectly if data was filtered (e.g., NA values removed).")
  }
  df$cluster <- gmm_model$classification
  print("DEBUG (gmms.R): Clusters assigned.")
  return(df)
}