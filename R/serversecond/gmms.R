library(mclust)

run_gmm <- function(data_mat, G_range = 2:5) {
  gmm_model <- Mclust(data_mat, G = G_range, modelNames = "VVI")  # Diagonal covariance
  return(gmm_model)
}

assign_clusters <- function(df, gmm_model) {
  df$cluster <- gmm_model$classification
  return(df)
}
