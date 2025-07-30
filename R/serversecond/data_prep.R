# R/serversecond/data_prep.R

# Function to apply universal plausibility limits
# This is a placeholder. You'll need to define your specific rules here.
# For example, filtering HGB values outside a physiologically plausible range.
apply_universal_plausibility_limits <- function(data_df) {
  # Example: Filter HGB to be between 5 and 20 g/dL (adjust as needed)
  # Example: Filter Age to be non-negative
  filtered_data <- data_df %>%
    filter(HGB >= 5 & HGB <= 20) %>% # Example HGB range
    filter(Age >= 0) # Example non-negative age

  if (nrow(filtered_data) < nrow(data_df)) {
    warning(paste(nrow(data_df) - nrow(filtered_data), "rows removed due to plausibility limits."))
  }
  return(filtered_data)
}