library(mclust)

run_gmm <- function(data_mat, G_range) {
  print("DEBUG (gmms.R): run_gmm function called.")
  print(paste("DEBUG (gmms.R): Input data_mat class:", class(data_mat)))
  print(paste("DEBUG (gmms.R): Input data_mat dimensions:", paste(dim(data_mat), collapse = "x")))
  print(paste("DEBUG (gmms.R): G_range parameter:", G_range))

  # Check if input is a matrix or data frame
  if (!is.matrix(data_mat) && !is.data.frame(data_mat)) {
    stop("Input data_mat must be a matrix or data frame for GMM analysis.")
  }

  # Ensure numeric
  if (!all(sapply(data_mat, is.numeric))) {
    stop("All columns in data_mat must be numeric.")
  }

  # Check for missing values
  if (any(is.na(data_mat))) {
    stop("Input data_mat contains NA values. Please remove or impute before clustering.")
  }

  # Choose appropriate modelNames based on number of dimensions
  dims <- ncol(data_mat)
  modelNames <- if (dims == 1) "V" else "VVI"  # Adjust as needed

  tryCatch({
    print(paste("DEBUG (gmms.R): Attempting Mclust fit with modelNames =", modelNames))
    gmm_model <- Mclust(data_mat, G = G_range, modelNames = modelNames)
    print("DEBUG (gmms.R): Mclust fit completed.")
    return(gmm_model)
  }, error = function(e) {
    print(paste("DEBUG ERROR (gmms.R): Error during Mclust fit:", e$message))
    stop(paste("GMM Mclust Error:", e$message)) # Re-throw for upstream handling
  })
}
