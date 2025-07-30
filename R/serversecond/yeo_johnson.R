# R/serversecond/yeo_johnson.R

# Requires the 'car' package for powerTransform
# install.packages("car")
library(car)

# Function to apply conditional Yeo-Johnson transformation
# It calculates skewness and applies Yeo-Johnson if abs(skewness) > threshold.
# The transformation itself handles positive/negative values.
apply_conditional_yeo_johnson <- function(data_vector, skewness_threshold = 0.5) {
  current_skewness <- calculate_skewness(data_vector) # Assuming calculate_skewness is available (from gmms.R)

  if (is.na(current_skewness)) {
    warning("Skewness could not be calculated. No transformation applied.")
    return(list(transformed_data = data_vector, transformation_applied = FALSE))
  }

  if (abs(current_skewness) > skewness_threshold) {
    tryCatch({
      # Yeo-Johnson can handle zeros and negative values
      yj_transform_result <- powerTransform(data_vector, family = "yj")
      transformed_data <- yj_transform_result$y.t
      return(list(transformed_data = transformed_data, transformation_applied = TRUE))
    }, error = function(e) {
      warning(paste("Error applying Yeo-Johnson transformation:", e$message, "No transformation applied."))
      return(list(transformed_data = data_vector, transformation_applied = FALSE))
    })
  } else {
    return(list(transformed_data = data_vector, transformation_applied = FALSE))
  }
}